{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Host.Headless where

import qualified Control.Concurrent as C
import Control.Concurrent.BoundedChan (newBoundedChan, readChan)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Dependent.Sum ((==>), DSum (..))
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Reflex hiding (EventTriggerRef, NotReady, TriggerInvocation, newEventWithTriggerRef, runTriggerEventT)
import qualified Reflex as R
import Reflex.Host.Class
import qualified Reflex.Spider.Internal as RI
import Reflex.NotReady.Bounded as B
import Reflex.TriggerEvent.Bounded
import Prelude

type MonadHeadlessApp t m =
  ( Adjustable t m,
    MonadFix m,
    MonadHold t m,
    MonadIO (HostFrame t),
    MonadIO (Performable m),
    MonadIO m,
    MonadRef (HostFrame t),
    B.NotReady t m,
    R.NotReady t m,
    PerformEvent t m,
    PostBuild t m,
    PrimMonad (HostFrame t),
    Ref (HostFrame t) ~ IORef,
    Ref m ~ IORef,
    Reflex t,
    ReflexHost t,
    TriggerEvent t m
  )

instance B.NotReady (RI.SpiderTimeline x) (RI.SpiderHostFrame x) where
  notReadyUntil _ = pure ()
  notReady = pure ()

-- | Run a headless FRP network. Inside the action, you will most probably use
-- the capabilities provided by the 'TriggerEvent' and 'PerformEvent' type
-- classes to interface the FRP network with the outside world. Useful for
-- testing. Each headless network runs on its own spider timeline.
runHeadlessApp ::
  -- | Bound for input messages.
  Int ->
  -- | The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  (forall t m. MonadHeadlessApp t m => m (Event t ())) ->
  IO ()
runHeadlessApp n guest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline.
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO $ newBoundedChan n
    -- events' :: _ <- liftIO $ C.newChan
    -- Run the "guest" application, providing the appropriate context. We'll
    -- pure the result of the action, and a 'FireCommand' that will be used to
    -- trigger events.
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT
        $ flip runPostBuildT postBuild
        $ flip runTriggerEventT events
        $ guest
    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef
    -- When there is a subscriber to the post-build event, fire the event.
    for_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ pure ()
    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent result
    -- The main application loop. We wait for new events and fire those that
    -- have subscribers. If we detect a shutdown request, the application
    -- terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> pure False
            Just _ -> pure True
      if or stop
        then pure ()
        else loop
  where
    -- Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs ::
      MonadIO m =>
      FireCommand t m ->
      [DSum (EventTriggerRef t) TriggerInvocation] ->
      ReadPhase m a ->
      m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO
        $ for ers
        $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          pure $ fmap (==> a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      pure a
