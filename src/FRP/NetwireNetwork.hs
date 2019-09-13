{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.NetwireNetwork where

import           Protolude hiding ((.))

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad.Fix
import           Control.Wire.Core
import           Data.IORef
import           FRP.Netwire
import           FRP.Model


--
-- Wire s e m a b
-- s - time diff, step
-- e - wire is blocked, value
-- m - monad to life in
-- a - input value
-- b - output value

readValuesFromChan :: (Show b) => TChan b -> Wire s () IO a b
readValuesFromChan chan = mkGen_ $ const $ do
    v <- atomically $ tryReadTChan chan
    pure $ case v of
      Just v' -> Right v'
      Nothing -> Left ()

calibrationCoefficientWire
    :: (Show b)
    => TChan b
    -> b
    -> Wire s () IO a b
calibrationCoefficientWire ccc = run
  where
    rW = readValuesFromChan ccc
    rB d = mkPureN $ \case
            Just x -> (Right x, rB x)
            Nothing -> (Right d, rB d)
    run xc = (Just <$> rW <|> pure Nothing) >>> rB xc

limitWire :: (Show (InputLimit r)) => TChan (InputLimit r) -> Wire s () IO a (InputLimit r)
limitWire = readValuesFromChan

-- FIXME: Similar to calibrationCoefficientWire, abstract here?
actualLimitWire :: (Show (Bounds r), Show (InputLimit r)) => TChan (InputLimit r) -> ActualLimits r -> Wire s () IO a (ActualLimits r)
actualLimitWire lc al = run al
  where
    rW = flip updateLimits al <$> limitWire lc
    rB d = mkPureN $ \case
            Just x -> (Right x, rB x)
            Nothing -> (Right d, rB d)
    run xc = (Just <$> rW <|> pure Nothing) >>> rB xc

pushValues :: (ProcessingOutput r -> IO ()) -> Wire s () IO (ProcessingOutput r) ()
pushValues push = mkGen_ $ fmap Right . push

processRawInput
    :: ( Ord (Calibrated r)
       , Show (CalibrationCoefficient r)
       )
    => CalibrationModel r
    -> Wire s () IO (CalibrationCoefficient r, ActualLimits r, Maybe r) (ProcessingOutput r)
processRawInput m = mkPure_ $ \(c, al, r) -> case r of
    Just r -> Right $ process m c al r
    Nothing -> Left ()

processRaw
    :: (Ord (Calibrated r))
    => CalibrationModel r
    -> CalibrationCoefficient r
    -> ActualLimits r
    -> Wire s () IO r (ProcessingOutput r)
processRaw cm cc al = mkPure_ $ \r -> Right $ process cm cc al r

setupNetwork
    :: ( HasTime t s
       , Ord (Calibrated r)
       , Show (Bounds r)
       , Show (CalibrationCoefficient r)
       , Show (InputLimit r)
       , Show r
       )
    => CalibrationModel r
    -> ProcessingInitial r
    -> TChan r
    -> TChan (CalibrationCoefficient r)
    -> TChan (InputLimit r)
    -> (ProcessingOutput r -> IO ())
    -> Wire s () IO () ()
setupNetwork cm (ProcessingInitial icc ial) rpc ccc lc pr =
    proc _ -> do
       rec
          -- FIXME: Fires very often, can this be reduced?
          cc <- calibrationCoefficientWire ccc icc -< ()
          nal <- actualLimitWire lc ial -< ()
          rp <- (Just <$> readValuesFromChan rpc) <|> pure Nothing -< ()
          r <- pushValues pr <<< processRawInput cm -< (cc, nal, rp)
       returnA -< ()

runNetwireNetwork :: (HasTime t s) => IORef Bool -> Session IO s -> Wire s e IO a () -> IO ()
runNetwireNetwork closedRef session wire = do
  closed <- readIORef closedRef
  Protolude.unless closed $ do
    (st , session') <- stepSession session
    -- FIXME: Remove undefined??
    (wt', wire'   ) <- stepWire wire st $ Right undefined
    runNetwireNetwork closedRef session' wire'

runNetwork
    :: ( Ord (Calibrated r)
       , Show r
       , Show (Bounds r)
       , Show (Calibrated r)
       , Show (CalibrationCoefficient r)
       , Show (InputLimit r)
       )
    => ProcessingConfig r
    -> ProcessingInitial r
    -> IO ()
runNetwork c@ProcessingConfig{..} i = do
    print "Run with Netwire"

    -- FIXME: Values are taken in Main from TBChan and here they are written into TChan again. Optimize.
    rawParameterChan <- newTChanIO
    rawParameterThreadId <- forkIO $ forever $ do
        p <- pullRawParameter 
        atomically . writeTChan rawParameterChan $ p

    calibrationCoefficientChan <- newTChanIO
    calibrationCoefficientThreadId <- forkIO $ forever $ pullCalibrationCoefficient >>= (atomically . writeTChan calibrationCoefficientChan)

    limitChan <- newTChanIO
    limitThreadId <- forkIO $ forever $ pullLimit >>= (atomically . writeTChan limitChan)

    killed <- newIORef False
    killThreadId <- forkIO $ killProcess >> writeIORef killed True

    runNetwireNetwork killed clockSession_
        (setupNetwork calibrationModel i rawParameterChan calibrationCoefficientChan limitChan pushResult)

    killThread killThreadId
    killThread limitThreadId
    killThread calibrationCoefficientThreadId
    killThread rawParameterThreadId
