-- https://ocharles.org.uk/blog/guest-posts/2013-12-21-24-days-of-hackage-contravariant.html
module ContravariantEx where

import           Data.Set

{-

(->) :: * -> * -> *
(->) r :: * -> *
((->) r) is the functorial structure

((->) r) a :: *

instance Functor ((->) r) where
  fmap f g = f . g
  fmap (a -> b) (r -> a) =  f . g


arrowG . arrowF
(Int -> String) . (Int -> Int) = (Int -> String)
(Int -> String) <$> (Int -> Int) = (Int -> String)
(Int -> String) <$> (((->) Int) Int) = Int -> String


f <$> (((->) Int) Int) = ((->) Int) f Int
(Int -> String) <$> (((->) Int) Int) = ((->) Int) (Int -> String) Int
((->) Int) (Int -> String) Int = ((->) Int) String
((->) Int) String =  Int -> String

((->) Int) String =  (Int -> String) . (a -> Int)



`r` inside `((->) r) a` is part of the functorial structure

so functors are limited to map `a` to `b`


-}
arrowG :: Int -> String
arrowG = show

arrowF :: Int -> Int
arrowF = (* 2)

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- Don't have functor instance
newtype Op b a =
  Op (a -> b)

{-
contramap (b -> a) (Op  (a -> b))
contramap f (Op g) = g . f
contramap (b -> a) (Op (a -> b)) =  (a -> b) (b -> a)

(a -> b)  (b -> a) = b -> b

-}
instance Contravariant (Op b) where
  contramap f (Op g) = Op (g . f)

--Op Int [a] = Op ([a] -> Int)
listLength :: Op Int [a]
listLength = Op Prelude.length

{-
 newtype Op b a = Op (a -> b)
 newtype Op Int a = Op (a -> Int)

 listLength :: Op Int [a] = Op ([a] -> Int)

 setToList :: Opt Int (Set a)  = contramap (toList) (listLength)

 contramap (toList) (Op ([a] -> Int)) = Op $ ([a] -> Int) . toList
 contramap (toList) (Op ([a] -> Int)) = Op $ ([a] -> Int) . ((Set a) -> [a])
 contramap (toList) (Op ([a] -> Int)) = Op $ (Set a) -> Int
-}
setToList :: Op Int (Set a)
setToList = contramap (toList) listLength
