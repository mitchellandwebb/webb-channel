module Webb.Channel.Internal.Sender where

import Prelude
import Webb.State.Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Webb.Channel.Data.ReceiveItem as RItem
import Webb.Channel.Data.ReceiveQueue as RQueue
import Webb.Channel.Data.SendItem (voided)
import Webb.Channel.Data.SendItem as SItem
import Webb.Channel.Data.SendQueue as SQueue
import Webb.Channel.Internal.State (ChannelState, CState)
import Webb.Channel.Internal.State as State
import Webb.Monad.Prelude (forceMaybe', onCancel, (||=))
import Webb.Result as Result

{- A sender of values into the channel. -}


newtype Sender = S ChannelState

derive instance Newtype Sender _

new :: forall m. MonadEffect m => CState -> m Sender
new state = do pure $ wrap state

getThis :: forall m. MonadEffect m => Sender -> m CState
getThis s = do pure $ unwrap s

-- Send the value into to the next waiting receiver, and remove the 
-- receiver from the receiver queue. Fail if there is no receiver, or
-- if other senders are already waiting. Return success flag
send :: forall a. Sender -> a -> Aff Boolean
send s a = do
  ifM (hasSenders ||= noReceiver) (do 
    pure false
  ) (do 
    r <- nextReceiver 
    deliver r $ voided a
    pure true
  )
  
  where
  hasSenders = do
    this <- getThis s
    State.hasSenders this
    
  noReceiver = do 
    this <- getThis s
    RQueue.isEmpty <: this.receivers
    
  nextReceiver = do
    this <- getThis s
    mitem <- RQueue.first <: this.receivers
    forceMaybe' "Missing a first item" mitem
    
  deliver ritem value = do
    let result = RItem.result ritem
    Result.return result value
  

-- Enqueue the value. If the buffer is full, wait until it is delivered; 
-- otherwise, return immediately.
wait :: forall a. Sender -> a -> Aff Boolean
wait s a = do
  item <- buildItem
  wasFull <- queueIsFull
  enqueue item
  if wasFull then do
    -- The sender waits to see if closure prevented delivery, since
    -- the value did not make it into the buffer.
    onCancel (remove item) do
      await item
  else do
    pure false

  where 
  buildItem = do
    this <- getThis s
    result <- Result.newResult
    id <- State.nextId this
    let item = SItem.newItem' a id result
    pure item
    
  queueIsFull = do
    this <- getThis s
    SQueue.isFull <: this.senders

  enqueue item = do
    this <- getThis s
    SQueue.addLast item :> this.senders
    
  await item = do 
    let result = SItem.result item
    Result.await result
    
  remove item = do
    this <- getThis s
    let id = SItem.id item
    SQueue.removeId id :> this.senders


