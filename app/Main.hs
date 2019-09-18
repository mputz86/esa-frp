{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor #-}

module Main where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan

import qualified Data.Char                     as C
import           Data.Dependent.Map
import           Data.Dependent.Map.Lens
import           Data.GADT.Compare
import           Data.GADT.Compare.TH
import qualified Data.Text                     as T
import           Data.Time

import           Parameters.Graph
import           Parameters.Model
import           Parameters.Reflex
import qualified Data.Map as M
import Control.Monad.Free
import qualified Prelude
import Control.Lens
import           Protolude

import           Reflex

--------------------------------------------------------------------------------
-- event creation DSL, for testing
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

someInts :: Num a => Int -> Signal a a 
someInts n = Signal 0 $ unroll $ do
    event n 1
    event 1000 2
    event 1000 3
    event 1000 4
    event 1000 5

someBools :: Int -> Signal Bool Bool
someBools n = Signal False $ unroll $ do
    event n True
    event 1000 False
    event 1000 True
    event 1000 False
--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------
newtype AnInt = AnInt Int
  deriving (Real, Enum, Num, Ord, Eq, Show, Integral)

type instance Calibrated AnInt = Int

type instance CalibrationCoefficient AnInt = Bool

calibrateA False (AnInt x) = x
calibrateA True (AnInt x) = x*2

data T a where
    TA :: T (ProcessingOutput AnInt)
    TAC :: T Bool

deriveGCompare ''T
deriveGEq ''T


iA :: Int -> Int -> InputConfig T AnInt
iA n cn = InputConfig (someInts n)
    (Controls calibrateA (someBools cn) (noSig mempty)) (OutputKeys TA TAC)

sAA :: Int -> SyntheticConfig T (Calibrated AnInt) (Calibrated AnInt) AnInt
sAA cn = SyntheticConfig (\x y -> AnInt $ x + y)
    (Controls calibrateA (someBools cn) (noSig mempty)) (OutputKeys TA TAC)


type Ords = (Ord (Calibrated AnInt))

graphABC 
    :: (Applicative d , Ords)
    =>  GraphDSL Text T d ()
graphABC = do
    a <- input (iA 0 600) "A" 
    b <- input (iA 400 1200) "B"
    e <- input (iA 700 1500) "E"
    c <- synth2 (sAA 200) "C" a b 
    d <- synth2 (sAA 500) "D" a c 
    e <- synth2 (sAA 1100) "F" a e 
    pure ()


prettyInputT name e = name <> ": " <> show e

prettyInput k t m = prettyInputT t $ m ! k M.! t
prettyInputM k t m = case  m ^? dmix k . ix t of
    Nothing -> []
    Just e ->[ prettyInputT t e]


reportT :: DMap T (Map Text) -> [Text]
reportT m = [ "----------value--------"
            , prettyInput TA "A" m 
            , prettyInput TA "B" m 
            , prettyInput TA "C" m 
            , prettyInput TA "D" m 
            , prettyInput TA "E" m 
            , prettyInput TA "F" m 
            ]

reportCT :: DMap T (Map Text) -> [Text]
reportCT m = concat [ ["-----coefficient------"]
            , prettyInputM TAC "A" m 
            , prettyInputM TAC "B" m 
            , prettyInputM TAC "C" m 
            , prettyInputM TAC "D" m 
            , prettyInputM TAC "E" m 
            , prettyInputM TAC "F" m 
            ]


reportTM (poD, cD)  = do
    performEvent_ $ liftIO . mapM_ putText . reportT <$> updated poD
    performEvent_ $ liftIO . mapM_ putText . reportCT <$> cD

test1 = runNetwork (G (graphABC, reportTM)) (threadDelay $ 10 * 10 ^ 6)
main :: IO ()
main = test1
