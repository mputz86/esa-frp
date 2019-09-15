{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances, TemplateHaskell #-}

module Parameters.Streamly where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans

import qualified Data.Char              as C
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Data.Time

import           Parameters.Model

import qualified Prelude                (Show, show)

import           Protolude hiding (to)

import           Streamly
import qualified Streamly.Prelude       as S

import qualified Test.QuickCheck.Gen    as Q
import Optics
import Optics.State.Operators

data Status r = Status
    { _statusCalibrationModel       :: CalibrationModel r
    , _statusCalibrationCoefficient :: CalibrationCoefficient r
    , _statusActualLimits           :: ActualLimits r
    }
makeLenses ''Status

data Event r = RawValue r
             | UpdateCalibrationCoefficient (CalibrationCoefficient r)
             | UpdateLimit (InputLimit r)

data ParallelResult r = Kill
                      | InputEvent (Event r)

data Result r = Quit
              | OutputResult (ProcessingOutput r)
              | Updated (Status r)

readEvents
    :: (Show (CalibrationCoefficient r), Ord (Calibrated r), MonadAsync m)
    => ProcessingConfig r
    -> SerialT m (Event r)
readEvents
    (ProcessingConfig _ calibrationModel getRawValue getCoefficient getLimit _)
    = rawS `parallel` coefficientS `parallel` limitS
  where rawS         = S.repeatM $ RawValue <$> liftIO getRawValue
        coefficientS
            = S.repeatM $ UpdateCalibrationCoefficient <$> liftIO getCoefficient
        limitS       = S.repeatM $ UpdateLimit <$> liftIO getLimit

setupNetwork :: ( MonadAsync m
                , MonadState (Status r) m
                , Ord (Calibrated r)
                , Show (CalibrationCoefficient r)
                )
             => ProcessingConfig r
             -> SerialT m (Result r)
setupNetwork c@(ProcessingConfig killProcess _ _ _ _ pushResult) = do
    event <- (InputEvent <$> readEvents c)
        `parallel` (Kill <$ liftIO killProcess)
    case event of
        InputEvent (RawValue v) -> do
            Status m co l <- get
            let r = process m co l v
            liftIO $ pushResult r
            return $ OutputResult r
        InputEvent (UpdateCalibrationCoefficient nc) -> do
            statusCalibrationCoefficient .= nc
            gets Updated
        InputEvent (UpdateLimit nl) -> do
            statusActualLimits %= updateLimits nl 
            gets Updated
        Kill -> return Quit

runNetwork :: (Ord (Calibrated r), Show (CalibrationCoefficient r))
           => ProcessingConfig r
           -> ProcessingInitial r
           -> IO ()
runNetwork c (ProcessingInitial initialCoefficient initialLimits) = do
    print "Run with Streamly"
    let r = S.runWhile isAlive (setupNetwork c)
    void $ runStateT r
        $ Status (calibrationModel c) initialCoefficient initialLimits

isAlive Quit = False
isAlive _ = True
