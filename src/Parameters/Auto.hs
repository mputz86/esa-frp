-- |
-- Module      :  Parameters.Auto
-- Copyright   :  
-- License     :  
--
-- Maintainer  :  paolo@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
--
{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
module Parameters.Auto where

import           Protolude hiding ((.))
import           Unsafe (unsafeFromJust)

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad.Fix
import           Control.Auto
import           Control.Auto.Interval
import           Control.Auto.Blip.Internal
import           Control.Auto.Blip
import           Data.IORef
import           Parameters.Model
import Control.Category



--
-- Auto  m a b
-- s - time diff, step
-- e - wire is blocked, value
-- m - monad to life in
-- a - input value
-- b - output value

-- | Inhibits: never

type AutoE  m a b = Auto  m a (Blip b) 

pullIO ::  IO b -> IO (AutoE  IO () b, ThreadId)
pullIO pull = do
    chan <- newTChanIO
    let auto = effect $ atomically $ (Blip <$> readTChan chan <|> pure NoBlip)
    tid <- forkIO $ forever $  pull >>=  atomically . writeTChan chan 
    pure (auto,tid)


calibrationCoefficientAuto :: AutoE  IO () b  -> b -> Auto IO () b
calibrationCoefficientAuto coeffE v = coeffE >>> holdWith_ v


actualLimitAuto ::  AutoE  IO () (InputLimit r) -> LimitsMap r -> Auto  IO () (LimitsMap r)
actualLimitAuto inputLimitE ial = inputLimitE >>> scanB_ (flip updateLimits) ial 

processRawInput
    :: (Ord (Calibrated r)) 
    => CalibrationModel r
    -> Auto  IO (CalibrationCoefficient r, LimitsMap r, r) (ProcessingOutput r)
processRawInput m = arr $ \(c, al, r) -> processNode m c al r

pushValues :: (ProcessingOutput r -> IO ()) -> Auto  IO (ProcessingOutput r) ()
pushValues push = arrM push


{-

setupNetwork
    :: forall e r s. ( Monoid e,   Ord (Calibrated r))
    => CalibrationModel r
    -> ProcessingInitial r
    -> AutoE  IO () r
    -> AutoE  IO () (CalibrationCoefficient r)
    -> AutoE  IO () (InputLimit r)
    -> (ProcessingOutput r -> IO ())
    -> Auto  IO () ()
setupNetwork cm (ProcessingInitial icc ial) rpc ccc lc pr =
    proc _ -> do
        cc <- calibrationCoefficientAuto ccc icc -<()
        nal <- actualLimitAuto lc ial -< ()
        rp <- rpc >>> hold -< ()
        pushValues pr <<< processRawInput cm -< (cc, nal, rp)
        
runNetwireNetwork ::  IORef Bool -> Session IO s -> Auto  IO () () -> IO ()
runNetwireNetwork closedRef = go 
    where 
    go session wire = do
        closed <- readIORef closedRef
        Protolude.unless closed $ do
            (st , session') <- stepSession session
            (_, wire' ) <- stepAuto wire st $ Right ()
            go session' wire'


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
-}
