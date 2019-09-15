{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeApplications #-}

module FRP.NetwireNetwork where

import           Protolude hiding ((.))
import           Unsafe (unsafeFromJust)

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad.Fix
import           Control.Wire.Core
import           Control.Wire.Unsafe.Event
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

-- | Inhibits: never

type WireE s e m a b = Wire s e m a (Event b) 



calibrationCoefficientWire :: Monoid e =>  WireE s e IO () b  -> b -> Wire s e IO () b
calibrationCoefficientWire coeffE v = pure v >-- (coeffE >>> hold)


actualLimitWire ::Monoid e =>  WireE s e IO () (InputLimit r) -> ActualLimits r -> Wire s e IO () (ActualLimits r)
actualLimitWire inputLimitE ial = inputLimitE >>> accumE (flip updateLimits) ial >>> hold


pushValues :: (ProcessingOutput r -> IO ()) -> Wire s e IO (ProcessingOutput r) ()
pushValues push = mkGen_ $ fmap Right . push

processRawInput
    :: (Ord (Calibrated r)) 
    => CalibrationModel r
    -> Wire s e IO (CalibrationCoefficient r, ActualLimits r, r) (ProcessingOutput r)
processRawInput m = mkPure_ $ \(c, al, r) -> Right $ process m c al r


setupNetwork
    :: forall e r s. ( Monoid e,   Ord (Calibrated r))
    => CalibrationModel r
    -> ProcessingInitial r
    -> WireE s e IO () r
    -> WireE s e IO () (CalibrationCoefficient r)
    -> WireE s e IO () (InputLimit r)
    -> (ProcessingOutput r -> IO ())
    -> Wire s e IO () ()
setupNetwork cm (ProcessingInitial icc ial) rpc ccc lc pr =
    proc _ -> do
        cc <- calibrationCoefficientWire ccc icc -<()
        nal <- actualLimitWire lc ial -< ()
        rp <- rpc >>> hold -< ()
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
    let wire = mkGen_ $ const $ atomically $ Right <$> (Event <$> readTChan chan <|> pure NoEvent)
    tid <- forkIO $ forever $  pull >>=  atomically . writeTChan chan 
    pure (wire,tid)

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
    (rawParameterChan, rawParameterThreadId) <- pullIO pullRawParameter 

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
