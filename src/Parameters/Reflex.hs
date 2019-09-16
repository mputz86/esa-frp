{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

-- FIXME Missing syntethic parameters.

data Acquisition = Sync | Async

acquireIO
    :: (TriggerEvent t m, MonadIO m, PerformEvent t m, MonadIO (Performable m))
    => Acquisition
    -> Event t () -- ^ kill thread event
    -> IO a -- ^ blocking data acquisition
    -> m (Event t a) -- ^ event 
acquireIO ac kE pull = do
    (xE, loop) <- case ac of
        Sync  -> do
            (xE, send) <- newTriggerEventWithOnComplete
            let loop = do
                    x <- pull
                    send x loop
            pure (xE, loop)
        Async -> do
            (xE, send) <- newTriggerEvent
            pure (xE, forever $ pull >>= send)
    tid <- liftIO $ forkIO loop
    performEvent_ $ liftIO (killThread tid) <$ kE
    pure xE

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
        rawE           <- acquireIO Async kE getRawValue
        coefficientE   <- acquireIO Async kE getCoefficient
        limitE         <- acquireIO Async kE getLimit
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

