module Webb.Channel.Internal.Channel where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Webb.Channel.Data.CMaybe (CMaybe)
import Webb.Channel.Data.CMaybe as CMaybe
import Webb.Channel.Internal.Receiver as Receiver
import Webb.Channel.Internal.Sender as Sender
import Webb.Channel.Internal.State (CState)
import Webb.Channel.Internal.State as State



{- Channel internal functions.
-}

type Channel = CState

-- Sending a value may hang, because if the buffer is full, we
-- have to wait, in order, for the send to complete.
-- We fail to send when the channel is closed; otherwise, we wait
-- and see whether we succeed.
send :: forall a. Channel -> a -> Aff Boolean
send chan a = do
  isOpen <- State.isOpen chan
  if isOpen then do
    s <- Sender.new chan
    success <- Sender.send s a
    if success then do
      pure true
    else do
      Sender.wait s a
  else do
    pure false
  

-- Receiving a value may hang, because if no value is present to
-- take, then we have to wait for a value.
receive :: forall a. Channel -> Aff (CMaybe a)
receive chan = do 
  r <- Receiver.new chan
  mvalue <- Receiver.receive r
  case mvalue of
    -- Did we succeed in receiving a value immediately?
    Just value -> do
      pure $ CMaybe.Open value
    Nothing -> do
      mvalue' <- Receiver.wait r
      case mvalue' of
        -- Did we receive a value later?
        Nothing -> do 
          pure CMaybe.Closed
        Just value -> do 
          pure $ CMaybe.Open value
