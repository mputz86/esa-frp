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
{-# LANGUAGE UndecidableInstances, DeriveFunctor #-}

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

import           Control.Monad.Fix
import           Control.Monad.Free


-------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- node type families, 'r' is the node type and is also the input type of the
-- node
--------------------------------------------------------------------------------


-- | model for calibration of values of type 'r'
type CalibrationModel r = CalibrationCoefficient r -> r -> Calibrated r

-- | type of the calibrated parameter
type family Calibrated r

-- | signal type parameter for calibration
type family CalibrationCoefficient r

--------------------------------------------------------------------------------
-- node data
--------------------------------------------------------------------------------

-- | bounds for 'r' based on Ord
data Bounds r
    = Bounds { boundsLow :: Calibrated r, boundsHigh :: Calibrated r }
  deriving Generic

instance NFData (Calibrated r) => NFData (Bounds r)

-- | instruction to pdate a LimitsMap.
data InputLimit r
    = InputLimit { inputTag :: LimitTag, inputBounds :: Bounds r }
  deriving Generic

instance (NFData (Calibrated r), NFData r) => NFData (InputLimit r)

-- | State of the limits.
type LimitsMap r = Map LimitTag (Bounds r)

newtype LimitTag = LimitTag Text
  deriving (Eq, Ord, Show, Generic)

instance NFData LimitTag

-- | set of which limits have been trespassed
type LimitExceedings = Set LimitTag

-- | observable status of a node
data ProcessingOutput r = ProcessingOutput
    { rawValue        :: r
    , calibratedValue :: Calibrated r
    , limitExceedings :: LimitExceedings
    }
  deriving Generic

instance (NFData (Calibrated r), NFData r) => NFData (ProcessingOutput r)
deriving instance (Show r, Show (Calibrated r)) => Show (ProcessingOutput r)


--------------------------------------------------------------------------------
-- node operations
--------------------------------------------------------------------------------

updateLimits :: InputLimit r -> LimitsMap r -> LimitsMap r
updateLimits (InputLimit t v) = M.insert t v

processNode :: (Ord (Calibrated r))
        => CalibrationModel r
        -> CalibrationCoefficient r
        -> LimitsMap r
        -> r
        -> ProcessingOutput r
processNode m c al r = let cr = m c r in ProcessingOutput r cr (checkLimits cr al)
    where
    notInLimit :: Ord (Calibrated r) => Calibrated r -> Bounds r -> Bool
    notInLimit r (Bounds l h) = r < l || r > h

    checkLimits
        :: Ord (Calibrated r) => Calibrated r -> LimitsMap r -> LimitExceedings
    checkLimits r al = fold $ do
        (t, b) <- M.assocs al
        guard $ notInLimit r b
        pure $ S.singleton t


--------------------------------------------------------------------------------
-- computational nodes configuration
--------------------------------------------------------------------------------

data Node q k r = Node
    { nodeComputation :: q r
    , nodeControls :: Controls r
    , nodeKeys :: OutputKeys k r
    }


-- | a signal folding x into a, the acquiring part is nested to support self booting
data Signal a x = Signal a (IO (STM x))
    deriving Functor

signalL f (Signal x y) = (\x' -> Signal x' y) <$> f x
-- | definition of common controls common to input nodes and synthetic nodes
data Controls r = Controls
    { c_calibrationModel :: CalibrationModel r
    -- ^ the chosen calibration model
    , c_pullCalibrationCoefficient :: Signal (CalibrationCoefficient r) (CalibrationCoefficient r)
    -- ^ how to wait for a calibration coefficient change
    , c_pullLimit :: Signal (LimitsMap r) (InputLimit r)
    -- ^ how to wait for a single limit change
    }

data OutputKeys k r = OutputKeys
    { processingOutput :: k (ProcessingOutput r)
    , coefficientOutput :: k (CalibrationCoefficient r)
    }

newtype InputConfig r = InputConfig (Signal r r)

newtype SyntheticConfig a b r = SyntheticConfig (a -> b -> r)


--------------------------------------------------------------------------------
-- functor graph definition form a free monad
--------------------------------------------------------------------------------

-- | a DSL to represent graph of synthetic parameters
data Graph t k d a where

    -- | introduce an input
    Input   :: Ord (Calibrated r)
            => Node InputConfig k r
            -> t
            -> (d  (Calibrated r) -> a)
            -> Graph t k d a
    -- | introduce a syntetic parameter depending on 2 others
    Synth2  :: Ord (Calibrated r)
            => Node (SyntheticConfig b c) k  r
            -> t
            -> d b
            -> d c
            -> (d (Calibrated r) -> a)
            -> Graph t k d a

deriving instance Functor (Graph t k d)

type GraphDSL t k d a = Free (Graph t k d) a

--------------------------------------------------------------------------------
-- Graph compositional verbs
--------------------------------------------------------------------------------

input :: Ord (Calibrated r)
      => Node (InputConfig) k r -- ^ the input configuration
      -> t
      -> Free (Graph t k d) (d (Calibrated r)) -- ^ output signal
input c t = liftF $ Input c t identity

synthetic :: Ord (Calibrated r)
       => Node (SyntheticConfig b c) k r -- ^ the synthetic configuration
       -> t
       -> d b -- ^ first input signal
       -> d c -- ^ second input signal
       -> Free (Graph t k d) (d (Calibrated r)) -- ^ output signal
synthetic c t a b = liftF $ Synth2 c t a b identity

