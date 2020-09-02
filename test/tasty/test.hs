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
   , testGroup "Monotone path already monotone"
    [ testProperty "derivative-clipping"
       $ \pth -> projectMonotone_derivativeClipping (getMonotonePath pth) ≈≈≈ pth
    , testProperty "interval-growing"
       $ \pth -> projectMonotone_lInftymin (getMonotonePath pth) === pth
    ]
   , testGroup "Monotonicisation is projection"
    [ testProperty "derivative-clipping"
       $ \pth -> let monotn = projectMonotone_derivativeClipping (V.fromList pth)
                 in projectMonotone_lInftymin (getMonotonePath monotn) ≈≈≈ monotn
    , testProperty "interval-growing"
       $ \pth -> let monotn = projectMonotone_lInftymin (V.fromList pth)
                 in projectMonotone_lInftymin (getMonotonePath monotn) === monotn
    ]
   , testGroup "After monotonicisation really monotone"
    [ testProperty "derivative-clipping"
       $ \(QC.NonEmpty pth)
              -> let MonotonePath monotn
                        = projectMonotone_derivativeClipping (V.fromList pth)
                 in QC.counterexample ("Result: "++show monotn)
                     . V.and $ V.zipWith (<=) monotn (V.tail monotn)
    , testProperty "interval-growing"
       $ \(QC.NonEmpty pth)
              -> let MonotonePath monotn = projectMonotone_lInftymin (V.fromList pth)
                 in QC.counterexample ("Result: "++show monotn)
                     . V.and $ V.zipWith (<=) monotn (V.tail monotn)
    ]
   , testGroup "Interval-growing is better"
    [ testProperty "l^∞"
       $ \(QC.NonEmpty pthl)
               -> let pth = V.fromList pthl
                      MonotonePath drvClipd = projectMonotone_derivativeClipping pth
                      MonotonePath iGrown = projectMonotone_lInftymin pth
                      refDeviation = V.maximum . V.zipWith (\r c -> abs $ c-r) pth
                  in QC.counterexample ("drvClipd="++show drvClipd
                                        ++": dev="++show (refDeviation drvClipd)
                                        ++", iGrown="++show iGrown
                                        ++": dev="++show (refDeviation iGrown))
                      $ refDeviation iGrown <= refDeviation drvClipd
    ]
   ]


(≈≈≈) :: MonotonePath Double -> MonotonePath Double -> QC.Property
MonotonePath p ≈≈≈ MonotonePath q
 | V.and $ V.zipWith (\x y -> abs (x-y) < 1e-9) p q
     = p===p
 | otherwise
     = p===q
