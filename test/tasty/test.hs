-- |
-- Module      : test
-- Copyright   : (c) Justus Sagemüller 2020
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 


import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC
import Data.Path.Monotone

main :: IO ()
main = do
  defaultMain $ testGroup "Tests"
   []


