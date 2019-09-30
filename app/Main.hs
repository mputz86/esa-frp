{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor, BlockArguments, TypeApplications #-}

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

import           Parameters.Model
import           Parameters.Reflex
import qualified Data.Map as M
import Control.Monad.Free
import qualified Prelude
import Control.Lens
import           Protolude
import Reflex -- (Event, Dynamic)


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


someA ::  Int -> Int ->  [a] -> IO (IO a) 
someA n d (x:xs) = unroll $ do
    event n x
    forM_ xs $ event d


data OT a where
    OTInt :: OT Int
    OTMInt :: OT (Maybe Int)

deriveGEq ''OT
deriveGCompare ''OT
--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------
  -- deriving (Real, Enum, Num, Ord, Eq, Show, Integral)
calibs :: Map Text (Int -> Int -> Int)
calibs = M.fromList
    [ ("shift-negate", \d x -> d - x)
    , ("shift", \d x -> d + x)
    ]

mkGraph 
    :: (Applicative d)
    => IO (GraphDSL Text OT e d ())
mkGraph = do
    i <- someA 200 300 [1 .. 20]
    i2 <- someA 130 450 [20,19..1]
    sd2 <- someA 130 450 $ concat $ replicate 10 [False,True]
    s <- someA 400 2000 ["shift-negate","shift","shift-negate","shift"]
    c <- someA 300 1000 [1,2,1,2]
    v <- someA 250 1200 [False, True, False, True, False]
    pure $ do
        -- controls
        switchCoeffD <- control c 0
        switchBranchE <- inputF sd2 
        valControlD <- control v True
        -- signals
        rE <- inputF i
        r2E <- inputF i2
        vE <- validate valControlD rE
        let branch1 switchCoeffD rE = do
                -- restart the static switching :-) on every branch1 switch
                switchingE <- inputF s
                switchF switchingE vE switchCoeffD calibs
            -- branch2 is a different closure (r2E vs vE i.e.)
            branch2 _ rE = composeF rE r2E (+)
        cE <- switchDynF switchBranchE rE switchCoeffD $ M.fromList
            [  (True , branch1)
            ,  (False, branch2)
            ]
        diffE <- composeF cE rE (\x y -> Just $ subtract x y)
        -- output
        output "raw" 0 rE OTInt
        output "validated" 0 vE OTInt
        output "calibrated" 0 cE OTInt
        output "absolute change" Nothing diffE OTMInt
        pure ()

prettyInputT name e = name <> ": " <> show e

prettyInput k t m = prettyInputT t $ m ! k M.! t

reportT :: DMap OT (Map Text) -> [Text]
reportT m = [ "----------value--------"
            , prettyInput OTInt "raw" m 
            , prettyInput OTInt "validated" m 
            , prettyInput OTInt "calibrated" m 
            , prettyInput OTMInt "absolute change" m 
            ]

reportTM :: ReflexC t m => CableD t Text OT -> m ()
reportTM poD  = do
    performEvent_ $ liftIO . mapM_ putText . reportT <$> updated poD

test1 = do
    runNetwork (G (mkGraph, reportTM)) (threadDelay $ 10 * 10 ^ 6)

main = test1

