{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Reflex where


import           Protolude

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time
import qualified Prelude (Show, show)
import           Reflex
import           Reflex.Host.Basic

import           FRP.Model


processNetwork
    :: (Reflex t, Ord (Calibrated r))
    => CalibrationModel r
    -> Event t r
    -> Dynamic t (CalibrationCoefficient r)
    -> Dynamic t (ActualLimits r)
    -> Event t (ProcessingOutput r)
processNetwork calibrationModel rawE coefficientD limitD =
    -- FIXME Missing syntethic parameters.
    attachWith (uncurry $ process calibrationModel) (current $ (,) <$> coefficientD <*> limitD) rawE

setupNetwork
    :: ( BasicGuestConstraints t m
       , Ord (Calibrated r)
       , Show r
       , Show (Bounds r)
       , Show (Calibrated r)
       , Show (CalibrationCoefficient r)
       )
    => ProcessingConfig r
    -> ProcessingInitial r
    -> BasicGuest t m (Event t ())
setupNetwork
        (ProcessingConfig killProcess calibrationModel getRawValue getCoefficient getLimit pushResult)
        (ProcessingInitial initialCoefficient initialLimits) = do

    (rawE, sendRaw) <- newTriggerEvent
    (coefficientE, sendCoefficient) <- newTriggerEvent
    (limitE, sendLimit) <- newTriggerEvent
    (killE, sendKill) <- newTriggerEvent

    threadIdKill <- liftIO $ forkIO $ forever $ killProcess >>= sendKill
    threadIdRaw <- liftIO $ forkIO $ forever $ getRawValue >>= sendRaw
    threadIdCoefficient <- liftIO $ forkIO $ forever $ getCoefficient >>= sendCoefficient
    threadIdLimit <- liftIO $ forkIO $ forever $ getLimit >>= sendLimit

    coefficientD <- holdDyn initialCoefficient coefficientE
    limitD <- foldDyn updateLimits initialLimits limitE

    let resultE = processNetwork calibrationModel rawE coefficientD limitD
    performEvent_ $ liftIO . pushResult <$> resultE

    let threadIds = [threadIdKill, threadIdRaw, threadIdCoefficient, threadIdLimit]
    performEvent_ $ liftIO (mapM_ killThread threadIds) <$ killE

    pure killE

runNetwork
    :: ( Ord (Calibrated r)
       , Show  r
       , Show (Bounds r)
       , Show (Calibrated r)
       , Show (CalibrationCoefficient r)
       )
    => ProcessingConfig r
    -> ProcessingInitial r
    -> IO ()
runNetwork c i = do
    print "Run with Reflex"
    basicHostWithQuit (setupNetwork c i)

