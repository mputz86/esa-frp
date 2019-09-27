-- |
-- Module      :  Parameters.Model
-- Copyright   :  Paolo Veronelli, Matthias Putz, 2019
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A model to express live parameter refinement
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, BlockArguments #-}
{-# LANGUAGE UndecidableInstances, DeriveFunctor #-}

module Parameters.Model where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Fix
import           Control.Monad.Trans

import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Time

import qualified Prelude                (Show, show)

import           Protolude

import           Control.Monad.Fix
import           Control.Monad.Free




---------------------------------------------------------------------------------
-- functor graph definition form a free monad
--------------------------------------------------------------------------------

-- | a DSL to represent graph of synthetic parameters
data Graph t k e d a where
    Input :: IO r -> (e r -> a) -> Graph t k e d a
    Control :: IO r -> r -> (d r -> a) -> Graph t k e d a
    Output :: t -> r -> e r -> k r -> a -> Graph t k e d a
    -- | introduce an input
    Validate  :: d Bool
            -> e r
            -> (e r -> a)
            -> Graph t k e d a
    -- | introduce a syntetic parameter depending on 2 others
    Switch ::  Ord s 
            => e s
            -> e i
            -> d c
            -> M.Map s (c -> i -> o)
            -> (e o -> a)
            -> Graph t k e d a



deriving instance Functor (Graph t k e d)

type GraphDSL t k e d a = Free (Graph t k e d) a

--------------------------------------------------------------------------------
-- Graph compositional verbs
--------------------------------------------------------------------------------
input pull = liftF $ Input pull identity
control pull c0 = liftF $ Control pull c0 identity
output name r0 iE tag = liftF $ Output name r0 iE tag ()
validate gate iE = liftF $ Validate gate iE identity
switchF selectE iE controlD calibs = liftF $ Switch selectE iE controlD calibs identity

--------------------------------------------------------------------------------
-- validation synth
--------------------------------------------------------------------------------



