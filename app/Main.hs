{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where


import           Protolude

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans
import qualified Data.Map as M
import qualified Data.Set as S
import           Reflex
import           Reflex.Host.Basic

newtype Bogous = Bogous Int
    deriving (Eq, Enum, Ord, Show)

type instance CalibrationCoefficient Bogous = ()

type instance Calibrated Bogous = Bogous

type CalibrationModel r = CalibrationCoefficient r -> r -> Calibrated r

type family Calibrated r

type family CalibrationCoefficient r

data ProcessingConfig r = ProcessingConfig
    { killProcess :: IO ()
    , calibrationModel :: CalibrationModel r
    , pullRawParameter :: IO r
    , pullCalibrationCoefficient :: IO (CalibrationCoefficient r)
    , pullLimit :: IO (InputLimit r)
    , pushResult :: ProcessingOutput r -> IO ()
    }

data ProcessingInitial r = ProcessingInitial
    { initialCalibrationCoefficient :: CalibrationCoefficient r
    , initialLimits :: ActualLimits r
    }

data Bounds r = Bounds 
    { boundsLow :: Calibrated r
    , boundsHigh :: Calibrated r
    }

-- | State of the limits.
type ActualLimits r = Map LimitTag (Bounds r)

newtype LimitTag = LimitTag Text
    deriving (Eq, Ord, Show)

-- | Updating the actual limits.
data InputLimit r = InputLimit
    { inputTag :: LimitTag
    , inputBounds :: Bounds r
    }

data LimitCheck = InLimit | SoftLimitExceeded | HardLimitExceeded

type LimitExceedings = Set LimitTag

data ProcessingOutput r = ProcessingOutput
    { rawValue :: r
    , calibratedValue :: Calibrated r
    , limitExceedings :: LimitExceedings
    }

deriving instance (Show r, Show (Calibrated r)) => Show (ProcessingOutput r)

notInLimit :: Ord (Calibrated r) => Calibrated r -> Bounds r -> Bool
notInLimit r (Bounds l h) = r < l || r > h

checkLimits :: Ord (Calibrated r) => Calibrated r -> ActualLimits r -> LimitExceedings
checkLimits r al =
    fold $ do
        (t, b) <- M.assocs al
        guard $ notInLimit r b
        pure $ S.singleton t

process
    :: (Ord (Calibrated r))
    => CalibrationModel r
    -> CalibrationCoefficient r
    -> ActualLimits r
    -> r
    -> ProcessingOutput r
process m c al r =
    let cr = m c r
    in ProcessingOutput r cr (checkLimits cr al)

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

updateLimits :: InputLimit r -> ActualLimits r -> ActualLimits r
updateLimits (InputLimit t v) = M.insert t v

setupNetwork
    :: (BasicGuestConstraints t m, Show r, Ord (Calibrated r))
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

  -- FIXME Threads must be stopped.
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


main :: IO ()
main = do
    chan <- newTChanIO
    let 
        valueSource :: Bogous -> IO ()
        valueSource n = do
            threadDelay 100000 
            atomically $ writeTChan chan n
            valueSource (succ n)

        c = ProcessingConfig
                (threadDelay 10000000)
                (const identity)
                (atomically $ readTChan chan)
                (atomically retry)
                (atomically retry)
                print
        i :: ProcessingInitial Bogous
        i = ProcessingInitial
                ()
                mempty
    forkIO $ valueSource (Bogous 0)
    basicHostWithQuit (setupNetwork c i)

