{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
startControls rD kE (Controls cm (c0, pullCC) (a0, pullL) mLogR) = do
    cD <- mkPullEvent kE pullCC >>=  holdDyn c0 
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
    Input (InputConfig (r0, pull) cs) f ->  do  
        -- raw value acquisition
        rD <- mkPullEvent kE pull >>=  holdDyn r0 
        startControls rD kE cs >>= buildGraph kE . f
    Synth2 (SyntheticConfig comp cs) aD bD f -> do
        startControls (comp <$> aD <*> bD) kE cs >>= buildGraph kE . f 

--------------------------------------------------------------------------------
-- old stuff
--------------------------------------------------------------------------------


processNetwork :: (Reflex t, Ord (Calibrated r))
               => CalibrationModel r
               -> Event t r
               -> Dynamic t (CalibrationCoefficient r)
               -> Dynamic t (ActualLimits r)
               -> Event t (ProcessingOutput r)
processNetwork calibrationModel rawE coefficientD limitD =
    attachWith (uncurry $ process calibrationModel) (current $ (,) <$> coefficientD <*> limitD) rawE

setupNetwork :: ( TriggerEvent t m
                , MonadIO m
                , PerformEvent t m
                , MonadIO (Performable m)
                , MonadHold t m
                , MonadFix m
                , Ord (Calibrated r)
                , Show r
                , Show (Bounds r)
                , Show (Calibrated r)
                , Show (CalibrationCoefficient r)
                )
             => ProcessingConfig r
             -> ProcessingInitial r
             -> m (Event t ())
setupNetwork (ProcessingConfig killProcess calibrationModel getRawValue
              getCoefficient getLimit pushResult)
    (ProcessingInitial initialCoefficient initialLimits) = do
        (kE, sendKill) <- newTriggerEvent
        rawE           <- mkPullEvent kE getRawValue
        coefficientE   <- mkPullEvent kE getCoefficient
        limitE         <- mkPullEvent kE getLimit
        _              <- liftIO $ forkIO $ killProcess >>= sendKill
        coefficientD   <- holdDyn initialCoefficient coefficientE
        limitD         <- foldDyn updateLimits initialLimits limitE
        performEvent_ $ liftIO . pushResult
            <$> processNetwork calibrationModel rawE coefficientD limitD
        pure kE

runNetwork :: ( Ord (Calibrated r)
              , Show r
              , Show (Bounds r)
              , Show (Calibrated r)
              , Show (CalibrationCoefficient r)
              )
           => ProcessingConfig r
           -> ProcessingInitial r
           -> IO ()
runNetwork c i = do
    print "Run with Reflex"
    basicHostWithQuit 100 $ setupNetwork c i

