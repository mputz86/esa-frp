{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, Rank2Types, OverloadedStrings #-}

module Parameters.Reflex where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans

import qualified Data.Char              as C
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Time
import Data.Dependent.Map
import           Parameters.Model

import qualified Prelude                (Show, show)

import           Protolude

import           Reflex
import           Reflex.Host.Basic
import Parameters.Graph
import Control.Monad.Free
import Text.Pretty.Simple

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
              => Dynamic t r -- ^ input signal
              -> Event t () -- ^  kill event
              -> Controls r -- ^ 
              -> m (Dynamic t (ProcessingOutput r))
startControls rD kE (Controls cm (Signal c0 pullCCG) (Signal a0 pullLG) ) = do
    pullCC <- liftIO $ pullCCG
    cD <- mkPullEvent kE pullCC >>=  holdDyn c0 
    pullL <- liftIO $ pullLG
    aD <- mkPullEvent kE pullL >>= foldDyn updateLimits a0 
    pure $ process cm <$> cD <*> aD <*> rD

--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------
   {-case mLogR of
        Just logR -> performEvent_ $ (liftIO . logR) <$> oE
        Nothing   -> pure ()-}

type Cable k = DMap k Identity
type CableD t k = Dynamic t (Cable k)
type GraphR t k = Graph k (Dynamic t)

buildGraph
    :: forall a k t m.
    (GCompare k, ReflexC t m)
    => Event t () -- ^ kill signal
    -> Free (GraphR t k) a  -- ^ the graph
    -> m (CableD t k) -- ^ anything relevant out of the building (dynamics ?)
buildGraph _ (Pure x) = pure mempty
buildGraph kE (Free y) = case y of
    Input (InputConfig (Signal r0 pullG) cs (k, i)) f -> do
        -- raw value acquisition
        pull <- liftIO pullG
        rD <- mkPullEvent kE pull >>= holdDyn r0
        poD <- startControls rD kE cs
        commonPart k f poD
    Synth2 (SyntheticConfig comp cs (k, i)) aD bD f   -> startControls
        (comp <$> aD <*> bD) kE cs >>= commonPart k f
  where commonPart
            :: k (Bool, ProcessingOutput r)
            -> (Dynamic t (Calibrated r) -> Free (GraphR t k) a)
            -> Dynamic t (ProcessingOutput r)
            -> m (CableD t k) -- (CableD k)
        commonPart k f poD = do
            posD <- buildGraph kE $ f $ calibratedValue <$> poD
            chE <- holdDyn False
                $ leftmost [True <$ updated poD, False <$ updated posD]
            pure $ (<>) <$> (singleton k . Identity <$> ((,) <$> chE <*> poD))
                <*> posD

-- | wrap up a generic graph and output handler over any t and m to be passed as
-- argument
newtype G k = G 
    (   forall t m
    .   (ReflexC t m, Applicative (Dynamic t)) 
     => ( Free (GraphR t k) () , CableD t k -> m ())
    )


runNetwork :: GCompare k
           => G k  -- ^ a graph forall quantified on 't'
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

--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------

prettyInput (fired, e) 
    | fired = "-> " <> show e
    | True = "   " <>  show e

prettySync (fired, e) 
    | fired = ">> " <> show e
    | True = "   " <> show e
reportT :: DMap T Identity -> [Text]
reportT m = 
    [  "-----------------"
    ,  prettyInput  $  runIdentity $ m ! TA
    ,  prettyInput  $  runIdentity $ m ! TB
    ,  prettyInput  $  runIdentity $ m ! TE
    ,  prettySync $  runIdentity $ m ! TC
    ,  prettySync $  runIdentity $ m ! TD
    ,  prettySync $  runIdentity $ m ! TF
    ]

reportTM e = performEvent_ $ liftIO . mapM_ putText . reportT <$> updated e

test1 = runNetwork (G (graphABC, reportTM)) (threadDelay $ 10 * 10 ^ 6)
