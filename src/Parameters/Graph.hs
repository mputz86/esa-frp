-- |
-- Module      :  ThreeParams
-- Copyright   :   2018-2019
-- License     :  BSD3
--
-- Maintainer  :  
-- Stability   :  experimental
-- Portability :  unknown
--
-- A language to encode a typed Graph with 2 types of polimorphic nodes
--
-- * Input nodes
-- * Synthetic nodes depending on 2 other nodes
--

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
{-# LANGUAGE TypeFamilies, ConstraintKinds, OverloadedStrings #-}

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

input :: Ord (Calibrated r)
         => InputConfig r -- ^ the input configuration
         -> Free (Graph d) (d (Calibrated r)) -- ^ output signal
input c = liftF $ Input c identity

synth2 :: Ord (Calibrated r)
       => SyntheticConfig b c r -- ^ the synthetic configuration
       -> d b -- ^ first input signal
       -> d c -- ^ second input signal
       -> Free (Graph d) (d (Calibrated r)) -- ^ output signal
synth2 c a b = liftF $ Synth2 c a b identity



--------------------------------------------------------------------------------
-- event DSL, for testing
--------------------------------------------------------------------------------

data List x a where
    Element :: Int -> x -> a -> List x a
    deriving Functor

event :: Int -> x -> Free (List x) ()
event t x = liftF $ Element t x ()

unroll :: Free (List x) a -> IO (IO x)
unroll y = do
    ch <- newTBChanIO 100
    let
        go (Pure _) = pure ()
        go (Free (Element t x f)) = do
            threadDelay (t * 1000) 
            atomically (writeTBChan ch x) 
            go f
    forkIO $ go y
    pure $ atomically $ readTBChan ch

noSig x = Signal x $ unroll $ pure ()

someInts :: Num a => IO (IO a)
someInts = unroll $ do
    event 0 1
    event 1000 2
    event 1000 3
    event 1000 4
    event 1000 5
--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------


newtype A = A Int deriving (Real, Enum, Num, Ord, Eq, Show, Integral)
newtype B = B Int deriving (Real, Enum, Num, Ord, Eq, Show, Integral)
newtype C = C Int deriving (Real, Enum, Num, Ord, Eq, Show, Integral)
newtype D = D Int deriving (Real, Enum, Num, Ord, Eq, Show, Integral)

type instance Calibrated A = Int
type instance Calibrated B = Int
type instance Calibrated C = Int
type instance Calibrated D = Int

type instance CalibrationCoefficient A = ()
type instance CalibrationCoefficient B = ()
type instance CalibrationCoefficient C = ()
type instance CalibrationCoefficient D = ()

calibrateA () (A x) = x
calibrateB () (B x) = x
calibrateC () (C x) = x
calibrateD () (D x) = x

iA :: Chan Log ->  InputConfig A 
iA ch = InputConfig (Signal 0 someInts) (Controls calibrateA (noSig ()) (noSig mempty ) 
        (Just $ writeChan ch . InputLog "A" . show))

iB :: Chan Log -> InputConfig B
iB ch = InputConfig (Signal 0 someInts) (Controls calibrateB (noSig () ) (noSig mempty ) 
        (Just $ writeChan ch . InputLog "B" . show))

sABC :: Chan Log -> SyntheticConfig (Calibrated A) (Calibrated B) C
sABC ch = SyntheticConfig (\x y -> C $ x + y) (Controls calibrateC (noSig () ) (noSig mempty ) 
        (Just $ writeChan ch . SyncLog "C" . show))

sACD :: Chan Log -> SyntheticConfig (Calibrated A) (Calibrated C) D
sACD ch = SyntheticConfig (\x y -> D $ x + y) (Controls calibrateD (noSig () ) (noSig mempty ) 
        (Just $ writeChan ch . SyncLog "D" . show))

type Ords = (Ord (Calibrated A), Ord (Calibrated B),Ord (Calibrated C), Ord (Calibrated D))

graphABC 
    :: (Applicative d , Ords)
    => Chan Log
    -> Free (Graph d) (d (Calibrated A, Calibrated B, Calibrated C, Calibrated D))
graphABC ch = do
    a <- input $ iA ch
    b <- input $ iB ch
    c <- synth2 (sABC ch) a b 
    d <- synth2 (sACD ch) a c 
    pure $ (,,,) <$> a <*> b <*> c <*> d




