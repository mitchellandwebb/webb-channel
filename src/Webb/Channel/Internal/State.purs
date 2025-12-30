module Webb.Channel.Internal.State where

import Prelude
import Webb.State.Prelude

import Effect.Class (class MonadEffect)
import Webb.Channel.Data.Buffer (Buffer)
import Webb.Channel.Data.Buffer as Buffer
import Webb.Monad.Prelude (notM)
import Webb.Mutex (Mutex, newMutex)
import Webb.Mutex as Mutex



type ChannelState a = 
  { buffer :: ShowRef (Buffer a)
  , open :: ShowRef Boolean
  }

type CState a = ChannelState a
  
newState :: forall m a. MonadEffect m => 
  Buffer.BufferSize -> m (ChannelState a)
newState size' = do
  buffer <- newShowRef $ Buffer.newBuffer size'
  open <- newShowRef $ true
  pure { buffer, open }
  
isOpen :: forall m a. MonadEffect m => CState a -> m Boolean
isOpen this = do aread this.open

isClosed :: forall m a. MonadEffect m => CState a -> m Boolean
isClosed = notM <<< isOpen

size :: forall m a. MonadEffect m => CState a -> m Int
size this = Buffer.size <: this.buffer
  
hasSenders :: forall m a. MonadEffect m => CState a -> m Boolean
hasSenders this = Mutex.isLocked this.sendQueue

hasReceivers :: forall m a. MonadEffect m => CState a -> m Boolean
hasReceivers this = Mutex.isLocked this.sendQueue