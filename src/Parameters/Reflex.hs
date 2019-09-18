{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameters.Reflex where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.Trans

import qualified Data.Char              as C
import           Data.Dependent.Map
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Time

import           Parameters.Model

import qualified Prelude                (Show, show)

import           Protolude

import           Reflex
import           Reflex.Host.Basic

import           Text.Pretty.Simple

--------------------------------------------------------------------------------
-- reflex standard constraints
--------------------------------------------------------------------------------


type ReflexC t m = ( PerformEvent t m
              , MonadFix m
              , Reflex t
              , MonadHold t m
              , MonadIO m
              , MonadIO (Performable m)
              , TriggerEvent t m
              )

--------------------------------------------------------------------------------
-- acquisition function from a blocking IO
--------------------------------------------------------------------------------

-- | start a pulling thread that pushes into the network through the fresh
-- created Event
mkPullEvent
    :: (ReflexC t m)
    => Event t () -- ^ kill thread event
    -> IO a -- ^ blocking data acquisition
    -> m (Event t a) -- ^ event 
mkPullEvent kE pull = do
    (xE, send) <- newTriggerEvent
    tid <- liftIO $ forkIO $ forever $ pull >>= send
    performEvent_ $ liftIO (killThread tid) <$ kE
    pure xE

--------------------------------------------------------------------------------
-- initiators
--------------------------------------------------------------------------------

    

-- | start control acquisitions threads, add output rendering, return calibrated
-- signal
startControls :: forall t m r.
              ( Ord (Calibrated r)
              , ReflexC t m 
              )
              => Event t () -- ^  kill event
              -> Controls r -- ^ 
              -> Dynamic t r -- ^ input signal
              -> m (Dynamic t (ProcessingOutput r), Event t (CalibrationCoefficient r))
startControls kE (Controls cm (Signal c0 pullCCG) (Signal a0 pullLG) ) rD = do
    pullCC <- liftIO $ pullCCG
    cD <- mkPullEvent kE (atomically pullCC) >>=  holdDyn c0 
    pullL <- liftIO $ pullLG
    aD <- mkPullEvent kE (atomically pullL) >>= foldDyn updateLimits a0 
    -- produce a ProcessingOutput dynamic that fires only when rD changes
    let crD = processNode cm <$> cD <*> aD <*> rD
    r0 <- sample $ current crD
    crD' <- holdDyn r0 $ tagPromptlyDyn crD $ updated rD
    pure (crD', updated cD)
--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------
 
type Cable tag k = DMap k (Map tag) -- ^ collect the outputs of different types
type CableD t tag k = Dynamic t (Cable tag k) -- ^ the dynamic output
type CableE t tag k = Event t (Cable tag k) -- ^ the dynamic output
type GraphR  tag t k  = Graph tag k  (Dynamic t) -- ^ graph specialized to Reflex instance

-- | compile the instruction graph to a reflex network 
buildGraph
    :: forall a k t tag m. (GCompare k, ReflexC t m, Ord tag)
    => Event t () -- ^ kill signal
    -> Free (GraphR tag t k) a  -- ^ the graph

    -> m (CableD t tag k, CableE t tag k) -- ^ anything relevant out of the building (dynamics ?)
buildGraph _ (Pure x) = pure mempty
buildGraph kE (Free y) = case y of
    Input (Node (InputConfig (Signal r0 pullG)) cs k ) t f 
        -> liftIO pullG 
        >>= mkPullEvent kE . atomically 
        >>= holdDyn r0 
        >>= startControls kE cs 
        >>= commonPart k t f 
    Synth2 (Node  (SyntheticConfig comp) cs k) t aD bD f   
        -> startControls kE cs (comp <$> aD <*> bD) 
        >>= commonPart k t f
  where commonPart
            :: OutputKeys k r
            -> tag 
            -> (Dynamic t (Calibrated r) -> Free (GraphR tag t k) a)
            -> (Dynamic t (ProcessingOutput r), Event t (CalibrationCoefficient r))
            -> m (CableD t tag k, CableE t tag k) -- (CableD k)
        commonPart (OutputKeys k kc)  i f (poD,cE) = do
            (posD,csE) <- buildGraph kE $ f $ calibratedValue <$> poD
            let posD' = (unionWithKey  (\_ -> (<>))) 
                    <$> (singleton k  . M.singleton i <$> poD) -- (singleton k . Identity $ (Output <$> chE <*> poD))
                    <*> posD
            let csE' = leftmost 
                    [ singleton kc  . M.singleton i <$> cE -- (singleton k . Identity $ (Output <$> chE <*> poD))
                    , csE
                    ]
            pure (posD', csE')

-- | wrap up a generic graph and output handler over any t and m to be passed as
-- argument
newtype G tag k = G 
    (   forall t m
    .   ( ReflexC t m, Applicative (Dynamic t)) 
     => ( Free (GraphR tag t k) () , (CableD t tag k, CableE t tag k) -> m ())
    )


runNetwork :: (GCompare k, Ord tag)
           => G tag k  -- ^ a graph forall quantified on 't'
           -> IO ()  -- ^ kill (blocking action)
           -> IO ()
runNetwork g k = do
    print "Run with Reflex"
    basicHostWithQuit 100 $ case g of
        G (g, output) -> do
            (kE, kIO) <- newTriggerEvent
            liftIO $ forkIO $ k >>= kIO
            buildGraph kE g >>= output
            pure kE


