module Test.Internal.ChannelSpec where

import Test.Prelude

import Data.Foldable as Fold
import Webb.Channel.Data.SendQueue as SQueue
import Webb.Channel.Internal.Channel as Chan
import Webb.Channel.Internal.State as State
import Webb.Monad.Prelude (delayInt, launch_)


spec :: Spec Unit
spec = describe "Channel internals" do
  it "can send to receiver" do
    ref <- newShowRef 0
    chan <- new 0
    send chan ref 10
    receive chan ref
    
    delayInt 5
    aread ref ?= 10
    
  where
  new i = do
    state <- State.newState (SQueue.Finite i)
    pure state
    
  send chan _ref i = do
    launch_ do
      void $ Chan.send chan i
      
  receive chan ref = do
    launch_ do 
      mval <- Chan.receive chan
      Fold.for_ mval \val -> do
        (val + _) :> ref



