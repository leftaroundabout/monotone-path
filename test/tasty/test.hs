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

import qualified Data.Vector.Unboxed as V
import Data.Semigroup
import Control.Arrow ((&&&))



 
main :: IO ()
main = do
  defaultMain $ testGroup "Tests"
   [ testProperty "No decreasing interval in monotone path"
       $ \(MonotonePath pth) -> decreasingIntervals pth === []
   , testProperty "Localise single decreasing interval"
       $ \(MonotonePath asc) (PositivePath descδs) (NonNegativePath reAscδs)
           -> let desc = V.scanl (-) (V.last asc) descδs
                  reAsc = V.scanl (+) (V.last desc) reAscδs
                  testPath = asc <> V.tail desc <> V.tail reAsc
              in ((xMin&&&xMax) <$> decreasingIntervals testPath)
                     === [(V.length asc - 1, V.length asc + V.length desc - 2)]
   , testProperty "Monotone path already monotone"
       $ \pth -> projectMonotone_lInftymin (getMonotonePath pth) === pth
   , testProperty "Monotonicisation is projection"
       $ \pth -> let monotn = projectMonotone_lInftymin (V.fromList pth)
                 in projectMonotone_lInftymin (getMonotonePath monotn) === monotn
   , testProperty "After monotonicisation really monotone"
       $ \pth -> let MonotonePath monotn = projectMonotone_lInftymin (V.fromList pth)
                 in QC.counterexample ("Result: "++show monotn)
                     $ V.length monotn > 0
                      ==> (V.and $ V.zipWith (<=) monotn (V.tail monotn))
   ]


