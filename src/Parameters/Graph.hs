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
import           Parameters.Model
import qualified Parameters.Reflex             as R
import           Parameters.Reflex

import qualified Prelude

import           Protolude

import           Reflex

import qualified Test.QuickCheck.Gen           as Q

--------------------------------------------------------------------------------
-- reflex standard constraints
--------------------------------------------------------------------------------



type ReflexC t m = ( PerformEvent t m
              , MonadFix m
              , Reflex t
              , MonadHold t m
              , MonadIO m
              , MonadIO (Performable m)
              , TriggerEvent t m
              )

--------------------------------------------------------------------------------
-- initiators
--------------------------------------------------------------------------------

-- | start control acquisitions threads, add output rendering, return calibrated
-- signal
startControls :: forall t m r.
              ( Ord (Calibrated r)
              , ReflexC t m 
              )
              => Dynamic t r -- ^ input signal
              -> Event t () -- ^  kill event
              -> Controls r -- ^ 
              -> m (Dynamic t (Calibrated r))
startControls rD kE (Controls cm (c0, pullCC) (a0, pullL) mLogR) = do
    cD <- acquireIO kE pullCC >>=  holdDyn c0 
    aD <- acquireIO kE pullL >>= foldDyn updateLimits a0 
    let oFD = process cm <$> cD <*> aD
        oE = attachWith ($) (current oFD) (updated rD)
    case mLogR of
        Just logR -> performEvent_ $ (liftIO . logR) <$> oE
        Nothing   -> pure ()
    pure $ calibratedValue <$> (oFD <*> rD)

--------------------------------------------------------------------------------
-- free monad
--------------------------------------------------------------------------------

-- | a DSL to represent graph of synthetic parameters
data Graph t a where
    -- | intropduce an input
    Input   :: Ord (Calibrated r) 
            => InputConfig r 
            -> (Dynamic t  (Calibrated r) -> a) 
            -> Graph t a
    -- | introduce a syntetic parameter depending on 2 others
    Synth2  :: Ord (Calibrated r) 
            => SyntheticConfig b c r 
            -> Dynamic t b 
            -> Dynamic t c 
            -> (Dynamic t (Calibrated r) -> a) 
            -> Graph t a

deriving instance Functor (Graph t)

--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------


buildGraph :: ReflexC t m
           => Event t () -- ^ kill signal
           -> Free (Graph t) a -- ^ the graph
           -> m a -- ^ anything relevant out of the building (dynamics ?)
buildGraph _ (Pure x) = pure x
buildGraph kE (Free y) = case y of
    Input (InputConfig (r0, pull) cs) f ->  do  
        -- raw value acquisition
        rD <- acquireIO kE pull >>=  holdDyn r0 
        startControls rD kE cs >>= buildGraph kE . f
    Synth2 (SyntheticConfig comp cs) aD bD f -> do
        startControls (comp <$> aD <*> bD) kE cs >>= buildGraph kE . f 

--------------------------------------------------------------------------------
-- Graph compositional verbs
--------------------------------------------------------------------------------

inputRaw :: Ord (Calibrated r)
         => InputConfig r -- ^ the input configuration
         -> Free (Graph t) (Dynamic t (Calibrated r)) -- ^ output signal
inputRaw c = liftF $ Input c identity

synth2 :: Ord (Calibrated r)
       => SyntheticConfig b c r -- ^ the synthetic configuration
       -> Dynamic t b -- ^ first input signal
       -> Dynamic t c -- ^ second input signal
       -> Free (Graph t) (Dynamic t (Calibrated r)) -- ^ output signal
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
    :: ( Reflex t , Ords)
    => Free (Graph t) (Dynamic t (Calibrated A, Calibrated B, Calibrated C, Calibrated D))
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


