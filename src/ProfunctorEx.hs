-- https://ocharles.org.uk/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html
module ProfunctorEx where

import           Data.Char

class Profunctor p where
  lmap :: (c -> a) -> p a b -> p c b
  rmap :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

instance Profunctor (->) where
  lmap h g = g . h --  (toUpper :: String -> String )  ->  (String -> String) = (String -> String) . toUpper :: String -> String
  rmap f g = f . g --  (head :: String -> Char) -> (String -> String)  = head . (String -> String) :: String -> Char
  dimap h f g = f . g . h -- (toUpper :: String -> String ) -> (head :: String -> Char) -> (String -> String) = head . (String -> String) . toUpper :: String -> Char

--toUpper comes from Data.Char
stringToString :: String -> String
stringToString = id

upperHead :: String -> Char
upperHead = dimap (fmap toUpper) (head) stringToString

halfOfTriple :: Int -> Double
halfOfTriple = dimap (* 3) ((/ 2) . fromIntegral) (id)
