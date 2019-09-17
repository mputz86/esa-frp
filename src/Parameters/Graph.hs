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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Parameters.Graph where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.Free.TH

import qualified Data.Char                     as C
import qualified Data.Text                     as T
import           Data.Time

import           Parameters.Model

import qualified Prelude

import           Protolude


import qualified Test.QuickCheck.Gen           as Q

--------------------------------------------------------------------------------
-- free monad
--------------------------------------------------------------------------------

-- | a DSL to represent graph of synthetic parameters
data Graph d a where
    -- | intropduce an input
    Input   :: Ord (Calibrated r) 
            => InputConfig r 
            -> (d  (Calibrated r) -> a) 
            -> Graph d a
    -- | introduce a syntetic parameter depending on 2 others
    Synth2  :: Ord (Calibrated r) 
            => SyntheticConfig b c r 
            -> d b 
            -> d c 
            -> (d (Calibrated r) -> a) 
            -> Graph d a

deriving instance Functor (Graph d)


--------------------------------------------------------------------------------
-- Graph compositional verbs
--------------------------------------------------------------------------------

inputRaw :: Ord (Calibrated r)
         => InputConfig r -- ^ the input configuration
         -> Free (Graph d) (d (Calibrated r)) -- ^ output signal
inputRaw c = liftF $ Input c identity

synth2 :: Ord (Calibrated r)
       => SyntheticConfig b c r -- ^ the synthetic configuration
       -> d b -- ^ first input signal
       -> d c -- ^ second input signal
       -> Free (Graph d) (d (Calibrated r)) -- ^ output signal
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

type Ords = (Ord (Calibrated A), Ord (Calibrated B),Ord (Calibrated C), Ord (Calibrated D))

graphABC 
    :: (Applicative d , Ords)
    => Free (Graph d) (d (Calibrated A, Calibrated B, Calibrated C, Calibrated D))
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


