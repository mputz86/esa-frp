-- |
-- Module      :  Parameters.Model
-- Copyright   :  Paolo Veronelli, Matthias Putz, 2019
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A model to express live parameter refinement 
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameters.Model where

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
-- | model for calibration of values of type 'r'
type CalibrationModel r = CalibrationCoefficient r -> r -> Calibrated r

-- | type of the calibrated parameter
type family Calibrated r

-- | signal type parameter for calibration
type family CalibrationCoefficient r

-- | full configuration of the process
data ProcessingConfig r = ProcessingConfig
    { killProcess :: IO ()
    -- ^ when to stop the process
    , calibrationModel :: CalibrationModel r
    -- ^ the chosen calibration model
    , pullRawParameter :: IO r
    -- ^ how to wait for a row parameter
    , pullCalibrationCoefficient :: IO (CalibrationCoefficient r)
    -- ^ how to wait for a calibration coefficient change
    , pullLimit :: IO (InputLimit r)
    -- ^ how to wait for a single limit change
    , pushResult :: ProcessingOutput r -> IO ()
    -- ^ how to push results, dangerous if slower than pullRawParameter
    }

-- | initial values for the process
data ProcessingInitial r = ProcessingInitial
    { initialCalibrationCoefficient :: CalibrationCoefficient r
    -- ^ initial calibration coefficient value
    , initialLimits :: ActualLimits r
    -- ^ inital map of limits
    }

-- | full configuration of the process
data InputConfig r = InpèutConfig
    { i_calibrationModel :: CalibrationModel r
    -- ^ the chosen calibration model
    , i_pullRawParameter :: IO r
    -- ^ how to wait for a row parameter
    , i_controls :: Controls r
    }

data  Controls r = Controls 
    { c_pullCalibrationCoefficient :: (CalibrationCoefficient r, IO (CalibrationCoefficient r))
    -- ^ how to wait for a calibration coefficient change
    , c_pullLimit :: (ActualLimits r, IO (InputLimit r))
    -- ^ how to wait for a single limit change
    , c_logResult :: Maybe (ProcessingOutput r -> IO ())
    -- ^ how to push results, dangerous if slower than pullRawParameter
    }

data SyntheticConfig a b r = SyntheticConfig
    {   s_compose :: Calibrated a -> Calibrated b -> r
    ,   s_controls :: Controls r
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
