{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameters.Reflex where

import           Control.Concurrent
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.Trans

import           Data.Dependent.Map
import           Data.GADT.Compare
import qualified Data.Map               as M

import           Parameters.Model


import           Protolude

import           Reflex
import           Reflex.Host.Basic
import qualified Data.IntMap as IM

import Reflex.Network


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
              , NotReady t m
              , Adjustable t m
              , PostBuild t m
              )


--------------------------------------------------------------------------------
-- acquisition function from a blocking IO
--------------------------------------------------------------------------------

-- | start a pulling thread that pushes into the network through the fresh
-- created Event
mkPullEvent
    :: (ReflexC t m)
    => Event t () -- ^ kill thread event
    -> IO a -- ^ blocking data acquisition
    -> m (Event t a) -- ^ event
mkPullEvent kE pull = do
    (xE, send) <- newTriggerEvent
    tid <- liftIO $ forkIO $ forever $ pull >>= send
    performEvent_ $ liftIO (killThread tid) <$ kE
    pure xE



--------------------------------------------------------------------------------
-- interpreter, it run the initiators and render the graph in reflex language
--------------------------------------------------------------------------------

type CableD t  k = Dynamic t (Cable  k) -- ^ the dynamic output
type CableE t  k = Event t (Cable  k) -- ^ the dynamic output

-- | compile the instruction graph to a reflex network
buildGraph
    :: forall a input  output t  m. (GCompare input, GCompare output, ReflexC t m)
    => Event t () -- ^ kill signal
    -> Free (Graph  t input output) a  -- ^ the graph
    -> m (a, CableD t output) -- ^ anything relevant out of the building (dynamics ?)
buildGraph _ (Pure x) = pure (x, mempty)
buildGraph kE (Free y) = case y of
    Input action  f -> do
        rE <- mkPullEvent kE action 
        buildGraph kE $ f rE
    FanCable signal f -> 
        buildGraph kE $ f $  fanG signal
    FanInCable signal typekey mapping f -> 
        buildGraph kE $ f $  selectInt (fanInt $ selectG signal typekey) . mapping 
    HoldEvent r0 rE f ->  do
        rD <- holdDyn  r0 rE
        buildGraph kE $ f rD
    Output name r0 rE h f -> do
        rD <- holdDyn  r0 rE
        (v,oD) <- buildGraph kE f
        pure $ (,) v $ insertWith (<>) h . IM.singleton name <$> rD <*> oD
    Validate bD rE f  -> buildGraph kE $ f $ gate (current bD) rE 
    Compose aE bE g f -> do 
        aD <- holdDyn Nothing $ Just <$> aE
        bD <- holdDyn Nothing $ Just <$> bE
        let cD = (\ma mb -> g <$> ma <*> mb) <$> aD <*> bD 
        buildGraph kE $ f $ fmapMaybe identity $ updated cD 
    Switch sE iE cD grs f -> do 
        let s t = case M.lookup t grs of
                Just g -> attachWith g (current cD) iE 
                _ -> never
        rE <- switchHold never $ s <$> sE 
        (x,outputD') <- buildGraph kE $ f rE
        pure (x, outputD') 
    DynSwitch sE iE cD grs (f :: Event t o -> GraphDSL  t input output a) -> do 
        let  
            s t = case M.lookup t grs of
                Just g -> fst <$>  buildGraph kE (g cD iE) 
                _ -> pure never :: m (Event t o)
        rE <- switch . current  <$>  networkHold (pure never) (s <$> sE)
        (x,outputD') <- buildGraph kE $ f rE
        pure (x, outputD') 

-- | wrap up a generic graph and output handler over any t and m to be passed as
-- argument
newtype G input output = G
    (   forall t  m
    .   ( ReflexC t m, Applicative (Dynamic t))
     => ( IO (Free (Graph  t input output) ()) , CableD t output -> m ())
    )


runNetwork :: (GCompare input, GCompare output)
           => G input output  -- ^ a graph forall quantified on 't'
           -> IO ()  -- ^ kill (blocking action)
           -> IO ()
runNetwork g k = do
    print "Run with Reflex"
    basicHostWithQuit 100 $ do
        case g of 
            G (iog, output) -> do
                g <- liftIO iog
                (kE, kIO) <- newTriggerEvent
                liftIO $ forkIO $ k >>= kIO
                (_, o) <- buildGraph kE g 
                output o
                pure kE

