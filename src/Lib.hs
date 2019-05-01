{-# OPTIONS_GHC -w #-}

module Lib
  ( someFunc
  ) where

import qualified ContravariantEx as CE
import qualified ProfunctorEx    as PE

someFunc :: IO ()
someFunc = putStrLn "someFunc"
