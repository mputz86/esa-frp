{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

-- (Event, Dynamic)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Lens
import Control.Monad.Free
import qualified Data.Char as C
import Data.Constraint.Extras (Has')
import Data.Dependent.Map
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map.Lens
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show (GShow)
import qualified Data.IntMap as IntMap
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Parameters.Model
import Parameters.Reflex
import Protolude
import Reflex
import qualified Prelude
import qualified Data.Bimap as BM
import Data.Bimap (Bimap)
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------
-- event creation DSL, for testing
--------------------------------------------------------------------------------

unrollL :: [(UTCTime, x)] -> IO (IO x)
unrollL ys = do
  ch <- newTBChanIO 100
  link =<< async
    do
      forM_ ys \(time, x) -> do
        waitUTCTime time
        atomically $ writeTBChan ch x
  pure $ atomically $ readTBChan ch

waitUTCTime :: UTCTime -> IO a0
waitUTCTime = panic "not implemented"

--------------------------------------------------------------------------
-- one stream input
--------------------------------------------------------------------------

data InputEvent input = InputEvent
  { -- | millisecs
    inputEvent_timestamp :: UTCTime,
    -- | the parameter name , unique at least for the type
    inputEvent_paramName :: Text,
    -- | value
    inputEvent_value :: DSum input Identity
  }

deriving instance (GShow k, Has' Show k Identity) => Show (InputEvent k)

deriving instance
  ( GEq k,
    Has' Eq k Identity
  ) =>
  Eq (InputEvent k)

deriving instance
  ( GCompare k,
    Has' Eq k Identity,
    Has' Ord k Identity
  ) =>
  Ord (InputEvent k)

streamInput ::
  (Text -> Int) ->
  [InputEvent input] ->
  IO (IO (DMap input IntMap))
streamInput namemapping events = do
  now <- getCurrentTime
  unrollL $ do
    InputEvent {..} <- events
    pure $ case inputEvent_value of
      key :=> Identity v ->
        ( inputEvent_timestamp,
          DMap.singleton
            key
            ( IntMap.singleton
                (namemapping inputEvent_paramName)
                v
            )
        )


-- (Show, Eq, Ord)
--------------------------------------------------------------------------
-- ouput channel types
--------------------------------------------------------------------------

data OT a where
  OTInt :: OT Int
  OTMInt :: OT (Maybe Int)

deriveGEq ''OT
deriveGCompare ''OT

--------------------------------------------------------------------------
-- input channel types
--------------------------------------------------------------------------

data IT a where
  ITInt :: IT Int
  ITBool :: IT Bool
  ITText :: IT Text

deriveGEq ''IT
deriveGCompare ''IT

--------------------------------------------------------------------------------
-- example
--------------------------------------------------------------------------------
-- deriving (Real, Enum, Num, Ord, Eq, Show, Integral)
calibs :: Map Text (Int -> Int -> Int)
calibs =
  M.fromList
    [ ("shift-negate", (-)),
      ("shift", (+))
    ]

{-# ANN module ("HLint: ignore Use let" :: Text) #-}

mkGraph :: forall t.
  (Text -> Int) -> 
  [InputEvent IT] ->
  IO (GraphDSL t IT OT ())

mkGraph paramNameMapping inputEvents = do
  inputEventsA <- streamInput paramNameMapping inputEvents
  let graph :: Free (Graph t IT OT) ()
      graph = do
        -- controls
        signals <- input inputEventsA >>= fanCable 
        intSignals <- fanInCable signals ITInt paramNameMapping
        boolSignals <- fanInCable signals ITBool paramNameMapping
        textSignals <- fanInCable signals ITText paramNameMapping
        switchBranchEvent <- pure $  boolSignals "switch branch"
        switchCoeffDyn <- holdEvent 0 $ intSignals  "coeffD"
        validateDyn <-  holdEvent True $ boolSignals "validateOrNot" 
        rE <- pure $ intSignals "some"
        r2E <- pure $ intSignals "other stuff"
        vE <- validate validateDyn rE
        let branch1 ::
              Dynamic t Int ->
              Event t Int ->
              GraphDSL t IT OT (Event t Int)
            branch1 switchCoeffD rE = do
              -- restart the static switching :-) on every branch1 switch
              let switchingE = textSignals "function"
              switchF switchingE vE switchCoeffD calibs
            -- branch2 is a different closure (r2E vs vE i.e.)
            branch2 _ rE = composeF rE r2E (+)
        cE <-
          switchDynF switchBranchEvent rE switchCoeffDyn $
            M.fromList
              [ (True, branch1),
                (False, branch2)
              ]
        diffE <- composeF cE rE (\x y -> Just $ subtract x y)
        -- output
        output "raw" 0 rE OTInt
        output "validated" 0 vE OTInt
        output "calibrated" 0 cE OTInt
        output "absolute change" Nothing diffE OTMInt
        pure ()
  pure graph

prettyInputT name e = name <> ": " <> show e

prettyInput :: (Show a) => (Text -> Int) -> OT a -> Text -> DMap OT IntMap -> Text
prettyInput mapping k t m = prettyInputT t $ m ! k IM.! mapping t

reportT :: (Text -> Int) -> DMap OT IntMap -> [Text]
reportT mapping m =
  [ "----------value--------",
    prettyInput mapping OTInt "raw" m,
    prettyInput mapping OTInt "validated" m,
    prettyInput mapping OTInt "calibrated" m,
    prettyInput mapping OTMInt "absolute change" m
  ]

reportTM :: ReflexC t m => (Text -> Int) -> CableD t OT -> m ()
reportTM mapping poD = performEvent_ $ liftIO . mapM_ putText . reportT mapping <$> updated poD

test1 :: Map Text Int -> [InputEvent IT] -> IO ()
test1 bimap inputEvents = runNetwork 
    do G (mkGraph (bimap M.!) inputEvents, reportTM (bimap M.!)) 
    do threadDelay $ 10 * 10 ^ 6 

data OutputEvent output = OutputEvent
  { -- | the parameter name , unique at least for the type
    outputEvent_paramName :: Text,
    -- | value
    outputEvent_value :: DSum output Identity
  }

deriving instance (GShow k, Has' Show k Identity) => Show (OutputEvent k)

deriving instance
  ( GEq k,
    Has' Eq k Identity
  ) =>
  Eq (OutputEvent k)

deriving instance
  ( GCompare k,
    Has' Eq k Identity,
    Has' Ord k Identity
  ) =>
  Ord (OutputEvent k)

{- test2 :: Map Text Int -> [InputEvent IT] -> IO [OutputEvent OT]
test2 bimap inputEvents = runNetwork 
    do G (mkGraph (bimap M.!) inputEvents, _) 
    do threadDelay $ 10 * 10 ^ 6  -}



{- main :: IO ()
main = test1 -}
