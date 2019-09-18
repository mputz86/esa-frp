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
{-# LANGUAGE TypeFamilies, DeriveFunctor, BlockArguments #-}

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

unroll :: Free (List x) a -> IO (STM x)
unroll y = do
    ch <- newTBChanIO 100
    let
        go (Pure _) = pure ()
        go (Free (Element t x f)) = do
            threadDelay (t * 1000) 
            atomically (writeTBChan ch x) 
            go f
    forkIO $ go y
    pure $ readTBChan ch

noSig x = Signal x $ pure retry

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
  -- deriving (Real, Enum, Num, Ord, Eq, Show, Integral)

newtype ABool = ABool Bool deriving (Show)
type instance Calibrated ABool = Bool
type instance CalibrationCoefficient ABool = ()

calibrateABool _ (ABool x) = x

newtype AnInt = AnInt Int deriving (Num, Show)
type instance Calibrated AnInt = Int
type instance CalibrationCoefficient AnInt = Bool

calibrateAInt False (AnInt x) = x
calibrateAInt True (AnInt x) = x*2

data T a where
    TA :: T (ProcessingOutput AnInt)
    TBC :: T Bool
    TB :: T (ProcessingOutput ABool)
    TNil :: T ()

deriveGCompare ''T
deriveGEq ''T


iA :: Int -> Int -> Node InputConfig T AnInt
iA n cn = Node 
    do InputConfig $ someInts n
    do Controls calibrateAInt (someBools cn) (noSig mempty) 
    do OutputKeys TA TBC

iABool :: Int -> Node InputConfig T ABool
iABool n  = Node 
    do InputConfig $ signalL %~ ABool $ ABool <$> someBools n
    do Controls calibrateABool (noSig ()) (noSig mempty) 
    do OutputKeys TB TNil

sAA :: Int -> Node (SyntheticConfig (Calibrated AnInt) (Calibrated AnInt)) T AnInt
sAA cn = Node 
    do SyntheticConfig $ \x y -> AnInt $ x + y
    do Controls calibrateAInt (someBools cn) (noSig mempty) 
    do OutputKeys TA TBC

gateG :: Int -> Node (SyntheticConfig (Calibrated ABool) (Calibrated AnInt)) T AnInt
gateG cn = Node 
    do SyntheticConfig 
        do \x y -> AnInt $ case x of
                True -> y
                False -> 0
    do Controls calibrateAInt (someBools cn) (noSig mempty) 
    do OutputKeys TA TBC

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
    e <- input (iA 700 1500) "E"
    f <- synth2 (sAA 1100) "F" a e 
    g <- input (iABool 200) "G" 
    synth2 (gateG 550) "H" g f
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
            , prettyInput TB "G" m 
            , prettyInput TA "H" m 
            ]

reportCT :: DMap T (Map Text) -> [Text]
reportCT m = concat [ ["-----coefficient------"]
            , prettyInputM TBC "A" m 
            , prettyInputM TBC "B" m 
            , prettyInputM TBC "C" m 
            , prettyInputM TBC "D" m 
            , prettyInputM TBC "E" m 
            , prettyInputM TBC "F" m 
            , prettyInputM TNil "G" m 
            , prettyInputM TBC "H" m 
            ]


reportTM (poD, cD)  = do
    performEvent_ $ liftIO . mapM_ putText . reportT <$> updated poD
    performEvent_ $ liftIO . mapM_ putText . reportCT <$> cD

test1 = runNetwork (G (graphABC, reportTM)) (threadDelay $ 10 * 10 ^ 6)
main :: IO ()
main = test1
