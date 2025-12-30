module Webb.Channel.Data.Buffer where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))
import Webb.Array as Array
import Webb.State.Prelude (views)

{- The channel's buffer. It is _sized_. -}

newtype Buffer a = B
  { array :: Array a
  , size :: BufferSize
  }

_array :: forall a r. Lens' { array :: a | r } a
_array = prop (Proxy :: Proxy "array")

_size :: forall a r. Lens' { size :: a | r } a
_size = prop (Proxy :: Proxy "size")
  
derive instance Eq a => Eq (Buffer a)
derive instance Ord a => Ord (Buffer a)
derive newtype instance Show a => Show (Buffer a)
derive instance Newtype (Buffer a) _
  
data BufferSize = Finite Int | Infinite
derive instance Eq BufferSize
derive instance Ord BufferSize
derive instance Generic BufferSize _
instance Show BufferSize where show = genericShow

newBuffer :: forall a. BufferSize -> Buffer a
newBuffer size' = fromArray size' []

toArray :: forall a. Buffer a -> Array a
toArray = unwrap >>> _.array

fromArray :: forall a. BufferSize -> Array a -> Buffer a
fromArray size' arr = wrap { array: arr, size: size' }

-- Before we add, we have to check if the buffer is full.
isFull :: forall a. Buffer a -> Boolean
isFull (B s) = case s.size of
  Infinite -> false
  Finite n -> Array.size s.array < n
  
isEmpty :: forall a. Buffer a -> Boolean
isEmpty = unwrap >>> views _array (Array.isEmpty)
  
size :: forall a. Buffer a -> Int
size = unwrap >>> views _array (Array.size)
  
addLast :: forall a. a -> Buffer a -> Buffer a
addLast a = modify $ over _array (Array.addLast a)

first :: forall a. Buffer a -> Maybe a
first = unwrap >>> views _array (Array.first)

drop :: forall a. Int -> Buffer a -> Buffer a
drop n = modify $ over _array (Array.drop n)