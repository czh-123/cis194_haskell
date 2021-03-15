{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

--https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
instance Semigroup Size where
  Size a <> Size b = Size (a+b)

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd