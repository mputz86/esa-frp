{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan

import qualified Data.Char                     as C
import qualified Data.Text                     as T
import           Data.Time

import           FRP.Model
import qualified FRP.NetwireNetwork            as N
import qualified FRP.Reflex                    as R
import qualified FRP.Streamly                  as S

import qualified Prelude

import           Protolude

import qualified Test.QuickCheck.Gen           as Q

newtype Bogous = Bogous { unBogous :: Text }
  deriving (Show, NFData)

instance Prelude.Show (Bounds Bogous) where
    show (Bounds (Bogous l) (Bogous h)) = show (T.length l, T.length h)

deriving instance Prelude.Show (InputLimit Bogous)

instance Eq Bogous where
    (==) = (==) `on` T.length . unBogous

instance Ord Bogous where
    compare = comparing $ T.length . unBogous

calibrateBogous :: CalibrationModel Bogous
calibrateBogous l (Bogous t) = Bogous $ T.map
    (\c -> if c == l then C.toUpper c else c) t

type instance CalibrationCoefficient Bogous = Char

type instance Calibrated Bogous = Bogous

bogousGen :: Q.Gen Bogous
bogousGen = Bogous . toS <$> do
    l <- Q.elements [1 .. 10]
    replicateM l (Q.elements "abc")

limitGen :: Q.Gen (InputLimit Bogous)
limitGen = do
    t <- LimitTag . toS <$> Q.elements ["key1", "key2", "key3"]
    low <- Bogous . toS . flip replicate 'x' <$> Q.elements [1 .. 5]
    high <- Bogous . toS . flip replicate 'x' <$> Q.elements [5 .. 10]
    pure $ InputLimit t (Bounds low high)

data RunConfig = RunConfig
    { valueDelay       :: Int
    , chanBound        :: Int
    , calibrateDelay   :: Int
    , limitDelay       :: Int
    , runTime          :: Int
    , genBogous        :: Bool
    , sampleEveryValue :: Int
    }

main :: IO ()
main = do
    -- Streamly can run this for max performance. Reflex hangs, since it catches up.
    -- let c = RunConfig 0 100000 500000 10000000 True 100000
    -- Runs pretty well for both.
    let c = RunConfig 1 100 100000 500000 10000000 True 100000

    -- Run with Streamly.
    -- run c S.runNetwork
    -- Run with Netwire.
    run c N.runNetwork
    -- Run with Reflex.
    -- run c R.runNetwork

valueSource :: NFData a
            => Int  -- ^ delay
            -> Maybe a
            -> TBChan a
            -> Q.Gen a
            -> IO ()
valueSource delay genOrNot valueChan gen = forever $ do
    threadDelay delay
    b <- case genOrNot of
        Nothing -> Q.generate gen
        Just x  -> pure x
    atomically $ writeTBChan valueChan b

run :: RunConfig
    -> (ProcessingConfig Bogous -> ProcessingInitial Bogous -> IO ())
    -> IO ()
run RunConfig
    { .. } networkRunner = do
        valueChan <- newTBChanIO chanBound
        calibrateChan <- newTBChanIO chanBound
        limitChan <- newTBChanIO chanBound
        count <- newTVarIO 0
        let c = ProcessingConfig (threadDelay runTime) calibrateBogous
                (atomically $ readTBChan valueChan)
                (atomically $ readTBChan calibrateChan)
                (atomically $ readTBChan limitChan)
                (\r -> deepseq r $ do
                     c <- readTVarIO count
                     if c `mod` sampleEveryValue == 0 then print r else pure ()
                     atomically . modifyTVar count $ succ)
            i :: ProcessingInitial Bogous
            i = ProcessingInitial 'a' mempty

        threadIdValue
            <- forkIO $ valueSource valueDelay Nothing valueChan bogousGen
        threadIdCalibrate <- forkIO
            $ valueSource calibrateDelay Nothing calibrateChan $ Q.elements "abc"
        threadIdLimit
            <- forkIO $ valueSource limitDelay Nothing limitChan limitGen

        tStart <- getCurrentTime
        networkRunner c i
        tEnd <- getCurrentTime
        print $ diffUTCTime tEnd tStart
        resultCount <- atomically $ readTVar count
        print resultCount

        killThread threadIdLimit
        killThread threadIdCalibrate
        killThread threadIdValue
