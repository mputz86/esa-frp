-- |
-- Module      :  ThreeParams
-- Copyright   :   2018-2019
-- License     :  BSD3
--
-- Maintainer  :  
-- Stability   :  experimental
-- Portability :  unknown
--
-- A netowrk of 2 sources and one synthtic param
--
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, PackageImports, BlockArguments, GADTs, DeriveFunctor, DataKinds #-}

module Parameters.Graph where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan

import qualified Data.Char                     as C
import qualified Data.Text                     as T
import           Data.Time

import           Parameters.Model
import qualified Parameters.Reflex                    as R

import qualified Prelude

import           Protolude

import qualified Test.QuickCheck.Gen           as Q

import Parameters.Model
import Control.Monad.Free
import Control.Monad.Free.TH
import Reflex

--------------------------------------------------------------------------------
-- initiators
--------------------------------------------------------------------------------


startInputConfig :: InputConfig r -> IO (Dynamic t  (Calibrated r))
startInputConfig = notImplemented

startSyntheticConfig 
    :: SyntheticConfig a b r 
    ->  IO (Dynamic t (a ->b -> Calibrated r))
startSyntheticConfig  = notImplemented

--------------------------------------------------------------------------------
-- free monad
--------------------------------------------------------------------------------


data Graph t a where
    Input   :: InputConfig r 
            -> (Dynamic t  (Calibrated r) -> a) 
            -> Graph t a
    Synth2  :: SyntheticConfig b c r 
            -> Dynamic t b 
            -> Dynamic t c 
            -> (Dynamic t (Calibrated r) -> a) 
            -> Graph t a

deriving instance Functor (Graph t)

--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------


buildGraph :: Reflex t => Free (Graph t) a -> IO a
buildGraph (Pure x) = pure x
buildGraph (Free y) = case y of
    Input c f ->  startInputConfig c >>= buildGraph . f
    Synth2 c aD bD f -> do
        fD <- startSyntheticConfig c         
        buildGraph $ f $ fD <*> aD <*> bD 

--------------------------------------------------------------------------------
-- dsl verbs, compositional 
--------------------------------------------------------------------------------

inputRaw :: InputConfig r -> Free (Graph t) (Dynamic t (Calibrated r))
inputRaw c = liftF $ Input c identity

synth2 :: SyntheticConfig b c r->  Dynamic t b -> Dynamic t c ->  Free (Graph t) (Dynamic t (Calibrated r))
synth2 c a b = liftF $ Synth2 c a b identity

--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------


data A 
data B 
data C
data D

iA :: InputConfig A 
iA = notImplemented

iB :: InputConfig B
iB = notImplemented

sABC :: SyntheticConfig (Calibrated A) (Calibrated B) C
sABC = notImplemented

sACD :: SyntheticConfig (Calibrated A) (Calibrated C) D
sACD = notImplemented


graphABC :: Reflex t => Free (Graph t) (Dynamic t (Calibrated A, Calibrated B, Calibrated C, Calibrated D))
graphABC = do
    a <- inputRaw iA
    b <- inputRaw iB
    c <- synth2 sABC a b 
    d <- synth2 sACD a c 
    pure $ (,,,) <$> a <*> b <*> c <*> d


{-chanBound = 100

createSource :: Int -> [r] -> IO (STM r)
createSource l xs = do
    ch <- newTBChanIO chanBound
    forkIO $ forM_ xs $ \x -> do
        threadDelay l
        atomically $ writeTBChan ch x
    pure $ readTBChan ch

newtype LimitedInteger = LimitedInteger Double

type instance Calibrated LimitedInteger = Int
type instance CalibrationCoefficient LimitedInteger = Double

mkB k (x,y) = InputLimit k (Bounds x y)
mkLinear n (x,y) = [x,x + d..y] -}
{-mkT1 :: IO (InputConfig LimitedInteger)
mkT1 = do
    rC <- createSource 100000 [1..100]
    cC <- createSource 200000 [1, 1.1 .. 2]
    lCH <- createSource 300000 $ map ("Hard" ,) [0..5]
    lCS <- createSource 300000 $ map ("Soft" ,) [0..5]
InputConfig 
    do \d x -> floor $ d * x-}


