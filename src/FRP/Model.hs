{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Model where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans

import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Time

import qualified Prelude                (Show, show)

import           Protolude

import           Reflex
import           Reflex.Host.Basic

import qualified Test.QuickCheck.Gen    as Q

-------------------------------------------------------------------------------
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

data Bounds r
    = Bounds { boundsLow :: Calibrated r, boundsHigh :: Calibrated r }
  deriving Generic

instance NFData (Calibrated r) => NFData (Bounds r)

-- deriving instance (Show r, Show (Calibrated r)) => Show (ProcessingOutput r)
-- | State of the limits.
type ActualLimits r = Map LimitTag (Bounds r)

newtype LimitTag = LimitTag Text
  deriving (Eq, Ord, Show, Generic)

instance NFData LimitTag

-- | Updating the actual limits.
data InputLimit r
    = InputLimit { inputTag :: LimitTag, inputBounds :: Bounds r }
  deriving Generic

instance (NFData (Calibrated r), NFData r) => NFData (InputLimit r)

data LimitCheck = InLimit
                | SoftLimitExceeded
                | HardLimitExceeded
  deriving Generic

instance NFData LimitCheck

type LimitExceedings = Set LimitTag

data ProcessingOutput r = ProcessingOutput
    { rawValue        :: r
    , calibratedValue :: Calibrated r
    , limitExceedings :: LimitExceedings
    }
  deriving Generic

instance (NFData (Calibrated r), NFData r) => NFData (ProcessingOutput r)

deriving instance (Show r, Show (Calibrated r)) => Show (ProcessingOutput r)

notInLimit :: Ord (Calibrated r) => Calibrated r -> Bounds r -> Bool
notInLimit r (Bounds l h) = r < l || r > h

checkLimits
    :: Ord (Calibrated r) => Calibrated r -> ActualLimits r -> LimitExceedings
checkLimits r al = fold $ do
    (t, b) <- M.assocs al
    guard $ notInLimit r b
    pure $ S.singleton t

updateLimits :: InputLimit r -> ActualLimits r -> ActualLimits r
updateLimits (InputLimit t v) = M.insert t v

process :: (Ord (Calibrated r))
        => CalibrationModel r
        -> CalibrationCoefficient r
        -> ActualLimits r
        -> r
        -> ProcessingOutput r
process m c al r = let cr = m c r in ProcessingOutput r cr (checkLimits cr al)
