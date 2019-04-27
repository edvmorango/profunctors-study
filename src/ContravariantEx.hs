-- https://ocharles.org.uk/blog/guest-posts/2013-12-21-24-days-of-hackage-contravariant.html
module ContravariantEx where

import           Data.Char
import           Data.Set

{-

  TLDR;
  Basically with  Contravariant it's possible to fix a Output for a polymorphic input

  Op b a = (a -> b)

  Op String a (a -> String)
  Op String Int (Int -> String)
  Op String SomeKind (SomeKind -> String)

  Behavior a = Behavior (a -> IO (Behavior a))

  Behavior String = Behavior (String -> IO (Behavior String))

  runBehavior :: Behavior a -> [a] -> IO ()


  ---------------------------------------------------------------------

  runBehavior :: Behavior a -> [a] -> IO ()

  [String] -> IO ()
  runBehavior = printer ["ab"]  -- ab

  [String] -> IO ()
  runBehavior = contramap ((fmap toUpper) printer) ["ab"] -- AB

  [Int] -> IO ()
  runBehavior = contramap ((show) printer)) [1] -- 1

  [String] -> IO ()
  runBehavior = contramap ((head) printer) ["abc"] -- a

-}
--------------------------------------------------------------------------------------------------------------------------------------------
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

------------------------- SIMPLE ACTORS ------------------------------
data Behavior a =
  Behavior (a -> IO (Behavior a))

instance Contravariant Behavior where
  contramap f (Behavior g) = Behavior $ \a -> fmap (contramap f) (g (f a))

runBehavior :: Behavior a -> [a] -> IO ()
runBehavior _ [] = return ()
runBehavior (Behavior f) (a:as) = do
  newBehavior <- f a
  runBehavior newBehavior as

printer :: Behavior String
printer = Behavior (\s -> putStrLn s >> return printer)

messages :: [String]
messages = ["Hello", "world", "Haskell", "is", "great"]

defaultPrinterBehavior :: IO ()
defaultPrinterBehavior = runBehavior printer messages

{-

 Behavior a = Behavior (a -> IO (Behavior a))
 Behavior String = Behavior (String -> IO (Behavior String))

 printer :: Behavior String
 printer = Behavior (s :: String ->  putStrLn s >> return printer)


 contramap f (Behavior g) = Behavior $ \a -> fmap (contramap f) (g (f a))

 upperPrinter :: Behavior String
 upperPrinter = contramap (\s -> toUpper <$> s) printer
 upperPrinter = contramap (fmap toUpper) (Behavior (s ->  putStrLn s >> return printer))
 upperPrinter = contramap f (Behavior g)

 contramp  f (Behavior (\s -> putStrLn s >> printer))

 contramap f (Behavior g) = Behavior $ \s fmap (contramap f) (g (f s))
 contramap (fmap toUpper) (Behavior (\s :: String -> putStrLn s >> printer)) = Behavior $ \s fmap (contramap  (fmap toUpper)) ((s :: String ->   putStrLn s >> printer )   ((fmap toUpper) s))

 contramp fUpper ...

 Behavior $\ "string" ->
                         fmap (contramap fUpper) ((\s -> putStrLn s >> printer) (fUpper "string"))
                         fmap (contramap fUpper) ((\s -> putStrLn s >> printer) "STRING")
                         fmap (contramap fUpper) (\"STRING" -> putStrLn "STRING" >> printer)
                         fmap (contramap fUpper) (Behavior "STRING")
                         fmap (contramap fUpper) (Behavior "STRING")


 `Behavior` will always be ready to be "contramapped" again?

-}
upperPrinter :: Behavior String
upperPrinter = contramap (\s -> toUpper <$> s) printer

upperPrinterBehavior :: IO ()
upperPrinterBehavior = runBehavior upperPrinter messages

numberPrinter :: Behavior String
numberPrinter = contramap (\s -> show s) printer

numberPrinterBehavior :: IO ()
numberPrinterBehavior = runBehavior numberPrinter [1, 2, 3, 4, 5]
