module Webb.Channel.Internal.Channel where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Webb.Channel.Internal.State (CState)



{- Channel internal functions.
-}

type Channel = CState

-- Sending a value may hang, because if the buffer is full, we
-- have to wait, in order, for the send to complete.
send :: forall a. Channel -> a -> Aff Unit
send chan a = do
  s <- Sender.new
  success <- Sender.send s
  
  -- Do we succeed in sending immediately?
  unless success do
    Sender.wait s

-- Receiving a value may hang, because if no value is present to
-- take, then we have to wait for a value.
receive :: forall a. Channel -> Aff (Maybe a)
receive chan = do 
  r <- Receiver.new
  mvalue <- Receiver.receive r
  case mvalue of
    Just value -> do
      pure $ Just value
    Nothing -> do
      Receiver.wait r
