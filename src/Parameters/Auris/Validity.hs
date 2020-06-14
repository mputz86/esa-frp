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

module Validity where

-- (Event, Dynamic)

import Control.Lens
import Data.Dependent.Map
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum ((==>))
import Data.GADT.Compare.TH
import qualified Data.IntMap as IntMap
import Data.TM.Parameter
-- import Data.TM.TMParameterDef
import Data.TM.Validity hiding (isValid)
import Data.TM.Value 
import Data.Text.Short
  ( ShortText,
  )
import General.Time
import Protolude

--------------------------------------------------------------------------
-- input channel types
--------------------------------------------------------------------------

data Signal a where
  SignalInt :: Signal Int64
  SignalUInt :: Signal Word64
  SignalDouble :: Signal Double
  SignalTime :: Signal SunTime
  SignalText :: Signal ShortText
  SignalOctet :: Signal ByteString
  SignalValidity :: Signal Bool

deriveGEq ''Signal
deriveGCompare ''Signal

--------------------------------------------------------------------------
-- one stream input, should move
--------------------------------------------------------------------------

data InputEvent = InputEvent
  { -- | millisecs
    inputEvent_timestamp :: SunTime,
    -- | the parameter name , unique at least for the type
    inputEvent_paramName :: ShortText,
    -- | value
    inputEvent_value :: DSum Signal Identity,
    inputEvent_validity :: Validity
  }

type Names = ShortText -> Int

convertTMValue ::
  -- | validity value, pass it in if this is a validity signal
  Maybe TMValue ->
  -- | the parameter to convert
  TMParameter ->
  InputEvent
convertTMValue validityValue TMParameter {..} = InputEvent {..}
  where
    inputEvent_timestamp = _pTime
    inputEvent_paramName = _pName
    inputEvent_value = case
      validityValue of 
        Nothing ->  case _tmvalValue _pValue of
          TMValInt x -> SignalInt ==> x
          TMValUInt x -> SignalUInt ==> x
          TMValDouble x -> SignalDouble ==> x
          TMValTime x -> SignalTime ==> x
          TMValString x -> SignalText ==> x
          TMValOctet x -> SignalOctet ==> x
        Just checked -> SignalValidity ==> _tmvalValue _pValue == _tmvalValue checked  
    inputEvent_validity = _tmvalValidity _pValue

convertInputEvent :: Names -> InputEvent -> (SunTime, DMap Signal IntMap)
convertInputEvent names InputEvent {..} = case inputEvent_value of
  key :=> Identity v ->
    ( inputEvent_timestamp,
      DMap.singleton
        key
        ( IntMap.singleton
            (names inputEvent_paramName)
            v
        )
    )

{- mkValidityGraph 
  :: TMParameterDef -> Producer TMParameter m () 
  -> GraphDSL t InputEvent InputEvent (Producer )
mkValidityGraph TMParameterDef {..} values -} 