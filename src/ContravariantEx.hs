-- https://ocharles.org.uk/blog/guest-posts/2013-12-21-24-days-of-hackage-contravariant.html
module ContravariantEx where

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

-}
arrowG :: Int -> String
arrowG = show

arrowF :: Int -> Int
arrowF = (* 2)

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
