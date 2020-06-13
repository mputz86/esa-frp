{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module Parameters.Model where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Protolude
import qualified Prelude (Show, show)

---------------------------------------------------------------------------------
-- functor graph definition form a free monad
--------------------------------------------------------------------------------

-- | a DSL to represent graph of synthetic parameters
data Graph t k e d a where
  Input ::
    -- | signal
    IO r ->
    -- | introduce an event
    (e r -> a) ->
    Graph t k e d a
  Control ::
    -- | signal
    IO r ->
    -- | first value
    r ->
    -- | introduce a dynamic
    (d r -> a) ->
    Graph t k e d a
  Output ::
    -- | parameter name
    t ->
    -- | first in value
    r ->
    -- | output event
    e r ->
    -- | output type key
    k r ->
    a ->
    Graph t k e d a
  Validate ::
    -- | dynamic gate
    d Bool ->
    -- | the event
    e r ->
    -- | introduce an event if passes
    (e r -> a) ->
    Graph t k e d a
  -- | introduce a syntetic parameter depending on 2 others
  Switch ::
    Ord s => -- switch key

    -- | event switch key
    e s ->
    -- | input signal
    e i ->
    -- | control dynamic
    d c ->
    -- | map of switches
    M.Map s (c -> i -> o) ->
    -- | introduce switch output
    (e o -> a) ->
    Graph t k e d a
  DynSwitch ::
    Ord s => -- switch key

    -- | event switch key
    e s ->
    -- | input signal
    e i ->
    -- | control dynamic
    d c ->
    -- | map of switches
    M.Map s (d c -> e i -> GraphDSL t k e d (e o)) ->
    -- | introduce switch output
    (e o -> a) ->
    Graph t k e d a
  Compose ::
    -- | left signal
    e b ->
    -- | right signal
    e c ->
    -- | composing function
    (b -> c -> r) ->
    -- | introduce composed signal
    (e r -> a) ->
    Graph t k e d a

deriving instance Functor (Graph t k e d)

type GraphDSL t k e d a = Free (Graph t k e d) a

--------------------------------------------------------------------------------
-- Graph compositional verbs
--------------------------------------------------------------------------------

inputF :: MonadFree (Graph t k e d) m => IO r -> m (e r)
inputF pull = liftF $ Input pull identity

control :: MonadFree (Graph t k e d) m => IO r -> r -> m (d r)
control pull c0 = liftF $ Control pull c0 identity

output :: MonadFree (Graph t k e d) m => t -> r -> e r -> k r -> m ()
output name r0 iE tag = liftF $ Output name r0 iE tag ()

validate :: MonadFree (Graph t k e d) m => d Bool -> e r -> m (e r)
validate gate iE = liftF $ Validate gate iE identity

switchF :: (MonadFree (Graph t k e d) m, Ord s) => e s -> e i -> d c -> Map s (c -> i -> o) -> m (e o)
switchF selectE iE controlD calibs = liftF $ Switch selectE iE controlD calibs identity

switchDynF :: (MonadFree (Graph t k e d) m, Ord s) => e s -> e i -> d c -> Map s (d c -> e i -> GraphDSL t k e d (e o)) -> m (e o)
switchDynF selectE iE controlD calibs = liftF $ DynSwitch selectE iE controlD calibs identity

composeF :: MonadFree (Graph t k e d) m => e b -> e c -> (b -> c -> r) -> m (e r)
composeF aE bE f = liftF $ Compose aE bE f identity

--------------------------------------------------------------------------------
-- validation synth
--------------------------------------------------------------------------------
