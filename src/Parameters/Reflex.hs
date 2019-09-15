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

processNetwork :: (Reflex t, Ord (Calibrated r))
               => CalibrationModel r
               -> Event t r
               -> Dynamic t (CalibrationCoefficient r)
               -> Dynamic t (ActualLimits r)
               -> Event t (ProcessingOutput r)
processNetwork calibrationModel rawE coefficientD limitD =
    -- FIXME Missing syntethic parameters.
    attachWith (uncurry $ process calibrationModel)
    (current $ (,) <$> coefficientD <*> limitD) rawE

setupNetwork :: ( BasicGuestConstraints t m
                , Ord (Calibrated r)
                , Show r
                , Show (Bounds r)
                , Show (Calibrated r)
                , Show (CalibrationCoefficient r)
                )
             => ProcessingConfig r
             -> ProcessingInitial r
             -> BasicGuest t m (Event t ())
setupNetwork (ProcessingConfig killProcess calibrationModel getRawValue
              getCoefficient getLimit pushResult)
    (ProcessingInitial initialCoefficient initialLimits) = do
        (rawE, sendRaw)                 <- newTriggerEvent
        (coefficientE, sendCoefficient) <- newTriggerEvent
        (limitE, sendLimit)             <- newTriggerEvent
        (killE, sendKill)               <- newTriggerEvent

        killThreads <- liftIO $  do
            threadIdKill <- forkIO $ forever $ killProcess >>= sendKill
            threadIdRaw  <- forkIO $ forever $ getRawValue >>= sendRaw
            threadIdCoefficient <- forkIO $ forever $ getCoefficient >>= sendCoefficient
            threadIdLimit <- liftIO $ forkIO $ forever $ getLimit >>= sendLimit
            pure $ mapM_ killThread 
                [ threadIdKill
                , threadIdRaw
                , threadIdCoefficient
                , threadIdLimit
                ]

        coefficientD <- holdDyn initialCoefficient coefficientE

        limitD <- foldDyn updateLimits initialLimits limitE

        performEvent_ $ liftIO . pushResult 
            <$> processNetwork calibrationModel rawE coefficientD limitD

        performEvent_ $ liftIO killThreads <$ killE

        pure killE

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
    basicHostWithQuit (setupNetwork c i)

