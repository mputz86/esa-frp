{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, Rank2Types #-}

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

import           Parameters.Model

import qualified Prelude                (Show, show)

import           Protolude

import           Reflex
import           Reflex.Host.Basic
import Parameters.Graph
import Control.Monad.Free

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
              -> m (Dynamic t (Calibrated r))
startControls rD kE (Controls cm (Signal c0 pullCCG) (Signal a0 pullLG) mLogR) = do
    pullCC <- liftIO $ pullCCG
    cD <- mkPullEvent kE pullCC >>=  holdDyn c0 
    pullL <- liftIO $ pullLG
    aD <- mkPullEvent kE pullL >>= foldDyn updateLimits a0 
    let oFD = process cm <$> cD <*> aD
        oE = attachWith ($) (current oFD) (updated rD)
    case mLogR of
        Just logR -> performEvent_ $ (liftIO . logR) <$> oE
        Nothing   -> pure ()
    pure $ calibratedValue <$> (oFD <*> rD)

--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------


buildGraph :: ReflexC t m
           => Event t () -- ^ kill signal
           -> Free (Graph (Dynamic t)) a -- ^ the graph
           -> m a -- ^ anything relevant out of the building (dynamics ?)
buildGraph _ (Pure x) = pure x
buildGraph kE (Free y) = case y of
    Input (InputConfig (Signal r0 pullG) cs) f ->  do  
        -- raw value acquisition
        pull <- liftIO pullG
        rD <- mkPullEvent kE pull >>=  holdDyn r0 
        startControls rD kE cs >>= buildGraph kE . f
    Synth2 (SyntheticConfig comp cs) aD bD f -> do
        startControls (comp <$> aD <*> bD) kE cs >>= buildGraph kE . f 

data G q =  G { unG :: forall t. Applicative (Dynamic t) => Free (Graph (Dynamic t)) q}


runNetwork :: G ()
           -> IO ()
           -> IO ()
runNetwork graph k = do
    print "Run with Reflex"
    basicHostWithQuit 100 $ case graph of 
        G g -> do
            (kE , kIO) <- newTriggerEvent
            liftIO $ forkIO $ k >>= kIO
            _ <- buildGraph kE g
            pure kE 
