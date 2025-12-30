module Webb.Channel.Internal.Channel where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Webb.Channel.Internal.State (CState)



{- Channel internal functions.
-}

type Channel a = CState a

-- Sending a value may hang, because if the buffer is full, we
-- have to wait, in order, for the send to complete.
send :: forall a. Channel a -> a -> Aff Unit
send chan a = pure unit

-- Receiving a value may hang, because if no value is present to
-- take, then we have to wait for a value.
receive :: forall a. Channel a -> Aff (Maybe a)
receive chan = pure Nothing
