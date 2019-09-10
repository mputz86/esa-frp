{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude

import           Control.Concurrent.STM
import qualified Data.Text as T
import           Data.Time
import qualified Test.QuickCheck.Gen as Q

import qualified MainReflex as R
import qualified MainStreamly as S
import           Model


bogousGen :: Q.Gen Bogous
bogousGen = Bogous . toS <$> do
    l <- Q.elements [1 .. 10]
    replicateM l (Q.elements "abc")

inputLimitGen :: Q.Gen (InputLimit Bogous)
inputLimitGen = do
    t <- LimitTag . toS <$> Q.elements ["key1", "key2", "key3"]
    low <- Bogous . toS . flip replicate 'x' <$> Q.elements [1 .. 5]
    high <- Bogous . toS . flip replicate 'x' <$> Q.elements [5 .. 10]
    pure $ InputLimit t (Bounds low high)

data RunConfig = RunConfig
    { valueDelay :: Int
    , calibrateDelay :: Int
    , limitDelay :: Int
    , runTime :: Int
    , genBogous :: Bool
    , sampleEveryValue :: Int
    }

main :: IO ()
main = do
    -- Streamly can run this for max performance. Reflex hangs, since it catches up.
    -- let c = RunConfig 0 100000 500000 10000000 True 100000
    -- Runs pretty well for both.
    let c = RunConfig 10 100000 500000 10000000 True 5000

    -- Run with Reflex.
    run c R.runNetwork
    -- Run with Streamly.
    --run c S.runNetwork

run :: RunConfig -> (ProcessingConfig Bogous -> ProcessingInitial Bogous -> IO ()) -> IO ()
run RunConfig{..} networkRunner = do
    valueChan <- newTChanIO
    calibrationChan <- newTChanIO
    limitChan <- newTChanIO
    count <- newTVarIO 0
    let 
        constValue = Bogous $ T.pack "abcabcabc"
        valueSource :: IO ()
        valueSource = do
            threadDelay valueDelay
            b <- if genBogous then Q.generate bogousGen
                              else pure constValue
            atomically $ writeTChan valueChan b
            valueSource

        calibrateSource :: IO ()
        calibrateSource = do
            threadDelay calibrateDelay
            b <- Q.generate $ Q.elements "abc"
            atomically $ writeTChan calibrationChan b
            calibrateSource

        limitSource :: IO ()
        limitSource = do
            threadDelay limitDelay
            b <- Q.generate inputLimitGen
            atomically $ writeTChan limitChan b
            limitSource

        c = ProcessingConfig
                (threadDelay runTime)
                calibrateBogous
                (atomically $ readTChan valueChan)
                (atomically $ readTChan calibrationChan)
                (atomically $ readTChan limitChan)
                (\r -> do
                    c <- readTVarIO count 
                    if c `mod` sampleEveryValue == 0 then print r
                                                     else pure ()
                    atomically . modifyTVar count $ succ)
        i :: ProcessingInitial Bogous
        i = ProcessingInitial 'a' mempty
    threadIdValue <- forkIO valueSource
    threadIdCalibrate <- forkIO calibrateSource
    threadIdLimit <- forkIO limitSource

    tStart <- getCurrentTime
    networkRunner c i
    tEnd <- getCurrentTime
    print $ diffUTCTime tEnd tStart
    resultCount <- atomically $ readTVar count
    print resultCount

    killThread threadIdLimit
    killThread threadIdCalibrate
    killThread threadIdValue
