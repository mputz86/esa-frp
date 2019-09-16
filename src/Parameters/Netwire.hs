{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeApplications, PackageImports, ConstraintKinds #-}

module Parameters.Netwire where

import           Protolude hiding ((.))
import           Unsafe (unsafeFromJust)

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad.Fix
import           Control.Wire.Core
import           Control.Wire.Unsafe.Event
import           Data.IORef
import "netwire" FRP.Netwire
import           Parameters.Model


--
-- Wire s e m a b
-- s - time diff, step
-- e - wire is blocked, value
-- m - monad to life in
-- a - input value
-- b - output value

-- | Inhibits: never

type WireE s e m a b = Wire s e m a (Event b) 

logE ::  Show b => Wire s e IO b b
logE = proc x -> do
    mkGen_ (\x -> fmap Right $ print x) -< x
    id -< x

calibrationCoefficientWire :: (Show b, Monoid e) =>  WireE s e IO () b  -> b -> Wire s e IO () b
calibrationCoefficientWire coeffE v = pure v >-- (coeffE >>> hold)

actualLimitWire ::Monoid e =>  WireE s e IO () (InputLimit r) -> ActualLimits r -> Wire s e IO () (ActualLimits r)
actualLimitWire inputLimitE ial = pure ial >-- (inputLimitE >>> accumE (flip updateLimits) ial >>> hold)

type ShowAll r = (Show r, Show (Calibrated r), Show (CalibrationCoefficient r))

pushValues :: ShowAll r => (ProcessingOutput r -> IO ()) -> Wire s e IO (ProcessingOutput r) ()
pushValues push = mkGen_ $ \x -> fmap Right $ push x 

processRawInput
    :: (Ord (Calibrated r)) 
    => CalibrationModel r
    -> Wire s e IO (CalibrationCoefficient r, ActualLimits r, r) (ProcessingOutput r)
processRawInput m = mkPure_ $ \(c, al, r) -> Right $ process m c al r

setupNetwork
    :: forall e r s. ( Monoid e,   Ord (Calibrated r), ShowAll r)
    => CalibrationModel r
    -> ProcessingInitial r
    -> Wire s e IO () r
    -> WireE s e IO () (CalibrationCoefficient r)
    -> WireE s e IO () (InputLimit r)
    -> (ProcessingOutput r -> IO ())
    -> Wire s e IO () ()
setupNetwork cm (ProcessingInitial icc ial) rpc ccc lc pr =
    proc _ -> do
        cc <- calibrationCoefficientWire ccc icc -<()
        nal <- actualLimitWire lc ial -< ()
        rp <- rpc  -< ()
        pushValues pr <<< processRawInput cm -< (cc, nal, rp)
        
runNetwireNetwork ::  IORef Bool -> Session IO s -> Wire s e IO () () -> IO ()
runNetwireNetwork closedRef = go 
    where 
    go session wire = do
        closed <- readIORef closedRef
        Protolude.unless closed $ do
            (st , session') <- stepSession session
            (_, wire' ) <- stepWire wire st $ Right ()
            go session' wire'

pullIO ::  IO b -> IO (WireE s e IO () b, ThreadId)
pullIO pull = do
    chan <- newTChanIO
    let wire = mkGen_ $ const $ atomically $
                    tryReadTChan chan >>= \case
                        Just v' -> pure $ Right $ Event v'
                        Nothing -> pure $ Right NoEvent
    tid <- forkIO $ forever $ pull >>= atomically . writeTChan chan
    pure (wire,tid)

pullIOT :: Monoid e => IO b -> IO (Wire s e IO () b, ThreadId)
pullIOT pull = do
    chan <- newTChanIO
    let wire = mkGen_ $ const $ atomically $
                    tryReadTChan chan >>= \case
                        Just v' -> pure $ Right v'
                        Nothing -> pure $ Left mempty
    tid <- forkIO $ forever $ pull >>= atomically . writeTChan chan
    pure (wire, tid)

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
    (rawParameterChan, rawParameterThreadId) <- pullIOT pullRawParameter 

    (calibrationCoefficientChan, calibrationCoefficientThreadId) <- pullIO pullCalibrationCoefficient 

    (limitChan, limitThreadId) <- pullIO pullLimit 

    killed <- newIORef False
    killThreadId <- forkIO $ killProcess >> writeIORef killed True

    runNetwireNetwork 
        killed 
        (countSession_ ())
        $ setupNetwork @ () calibrationModel i rawParameterChan calibrationCoefficientChan limitChan pushResult

    killThread killThreadId
    killThread limitThreadId
    killThread calibrationCoefficientThreadId
    killThread rawParameterThreadId
