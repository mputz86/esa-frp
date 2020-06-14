{-# LANGUAGe BlockArguments #-}
{-# LANGUAGe DeriveFunctor #-}
{-# LANGUAGe FlexibleContexts #-}
{-# LANGUAGe FlexibleInstances #-}
{-# LANGUAGe GADTs #-}
{-# LANGUAGe StandaloneDeriving #-}
{-# LANGUAGe TypeFamilies #-}
{-# LANGUAGe UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module       :  Parameters.Model
-- Copyright   :  Paolo Veronelli, Matthias Putz, 2019
-- License      :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A model to express liv Event t parameter refinement
module Parameters.Model where

import Control.Monad.Free
import qualified Data.Map as M
import Protolude
import Data.Dependent.Map (DMap)
import Reflex

type Cable k = DMap k IntMap -- ^ collect th Event t outputs of different types
---------------------------------------------------------------------------------
-- functor Graph t definition form a fre Event t monad
--------------------------------------------------------------------------------


-- | a DSL to represent Graph t of synthetic parameters
data Graph t input output a where
  Input :: 
    -- | polling action 
    IO r -> 
    -- | introduce r events 
    (Event t r -> a) -> 
    Graph t input output a
      
  FanCable :: 
    -- | signal 
     Event t (Cable input) ->
    -- | introduce  
    (EventSelectorG t input IntMap  -> a) -> 
    Graph t input output a
  FanInCable :: 
    -- | signal 
    EventSelectorG t input IntMap ->
    -- | type key
    input r -> 
    -- | name mapping 
    (Text -> Int) -> 
    -- | introduce an event of type r 
    ((Text -> Event t r) -> a) -> 
    Graph t input output a
  HoldEvent :: 
    -- | boot value
    r -> 
    -- | event to hold 
    Event t r -> 
    -- | introduce a dynamic of type r
    (Dynamic t r -> a) -> 
    Graph t input output a
  Output ::
    -- | parameter name
    Int ->
    -- | first in value
    r ->
    -- | output event
    Event t r ->
    -- | output typ Event t key
    output r ->
    a ->
    Graph t input output a
  Validate ::
    -- | dynamic gate
    Dynamic t Bool ->
    -- | th Event t event
    Event t r ->
    -- | introduc Event t an Event t if passes
    (Event t r -> a) ->
    Graph t input output a
  -- | introduc Event t a syntetic parameter depending on 2 others
  Switch ::
    Ord s => -- switch key
    -- | evnt switch key
    Event t s ->
    -- | input signal
    Event t i ->
    -- | control dynamic
    Dynamic t c ->
    -- | map of switches
    M.Map s (c -> i -> o) ->
    -- | introduc Event t switch output
    (Event t o -> a) ->
    Graph t input output a
  DynSwitch ::
    Ord s => -- switch key
    -- | Event t switch key
    Event t s ->
    -- | input signal
    Event t i ->
    -- | control dynamic
    Dynamic t c ->
    -- | map of switches
    M.Map s (Dynamic t c ->  Event t i -> GraphDSL t input output  (Event t o)) ->
    -- | introduce dynamically switched event
    (Event t o -> a) ->
    Graph t input output a
  Compose ::
    -- | left signal
     Event t b ->
    -- | right signal
     Event t c ->
    -- | composing function
    (b -> c -> r) ->
    -- | introduc Event t composed signal
    ( Event t r -> a) ->
    Graph t input output a

deriving instance Functor (Graph t input output)

type GraphDSL t input output a = Free (Graph t input output) a

--------------------------------------------------------------------------------
-- Graph t compositional verbs
--------------------------------------------------------------------------------

input :: MonadFree (Graph t input output) m => IO r -> m (Event t r)
input action = liftF $ Input action identity

fanCable :: MonadFree (Graph t input output) m => Event t (Cable input) -> m (EventSelectorG t input IntMap)
fanCable event = liftF $ FanCable event  identity

fanInCable :: MonadFree (Graph t input output) m => EventSelectorG t input IntMap -> input r -> (Text -> Int) -> m (Text -> Event t r)
fanInCable cable key mapping  = liftF $ FanInCable cable key mapping  identity

holdEvent :: MonadFree (Graph t input output) m => r -> Event t r -> m (Dynamic t r)
holdEvent r0 event = liftF $ HoldEvent r0 event identity 

output :: MonadFree (Graph t input output) m => Int -> r -> Event t r -> output r -> m ()
output name r0 iE tag' = liftF $ Output name r0 iE tag' ()

validate :: MonadFree (Graph t input output) m => Dynamic t Bool -> Event t r -> m (Event t r)
validate gate' iE = liftF $ Validate gate' iE identity

switchF :: (MonadFree (Graph t input output) m, Ord s) => Event t s -> Event t i -> Dynamic t c -> Map s (c -> i -> o) -> m (Event t o)
switchF selectE iE controlD calibs = liftF $ Switch selectE iE controlD calibs identity

switchDynF :: (MonadFree (Graph t input output) m, Ord s) => Event t s 
  -> Event t i -> Dynamic t c -> Map s (Dynamic t c -> Event t i -> GraphDSL t input output (Event t o)) -> m (Event t o)
switchDynF selectE iE controlD calibs = liftF $ DynSwitch selectE iE controlD calibs identity

composeF :: MonadFree (Graph t input output) m => Event t b -> Event t c -> (b -> c -> r) -> m (Event t r)
composeF aE bE f = liftF $ Compose aE bE f identity
--------------------------------------------------------------------------------
-- validation synth
--------------------------------------------------------------------------------
