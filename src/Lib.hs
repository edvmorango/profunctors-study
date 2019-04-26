{-# OPTIONS_GHC -w #-}

module Lib
  ( someFunc
  ) where

import qualified ContravariantEx as CE

someFunc :: IO ()
someFunc = putStrLn "someFunc"
