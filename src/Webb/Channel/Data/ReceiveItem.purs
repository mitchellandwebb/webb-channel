module Webb.Channel.Data.ReceiveItem where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Webb.Channel.Data.Id (Id)
import Webb.Result (Result)


{- An item based on an attempt to receive a value. -}

newtype ReceiveItem = R
  { id :: Id
  , result :: Result Void -- Receive a delivered value.
  }
  
type RItem = ReceiveItem
  
instance Eq ReceiveItem where
  eq (R a) (R b) = a.id == b.id
instance Ord ReceiveItem where
  compare (R a) (R b) = compare a.id b.id
derive instance Newtype ReceiveItem _

instance Show ReceiveItem where
  show (R s) = show { id: s.id }
  
newItem :: Id -> Result Void -> ReceiveItem
newItem id' result' = wrap { id: id', result: result' }

id :: ReceiveItem -> Id
id = unwrap >>> _.id

result :: ReceiveItem -> Result Void
result = unwrap >>> _.result