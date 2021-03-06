-- |
-- Module      : test
-- Copyright   : (c) Justus Sagemüller 2020
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies #-}

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
   , testGroup "Noise robustness"
    [ testProperty "interval-growing, l^1"
       $ noiseRobust projectMonotone_lInftymin (V.sum . V.map abs)
    , testProperty "interval-growing, l^2"
       $ noiseRobust projectMonotone_lInftymin (V.sum . V.map (^2))
    , testProperty "interval-growing, l^∞"
       $ noiseRobust projectMonotone_lInftymin (V.maximum . V.map abs)
     -- The derivative-clipping algorithm is _not_ noise-robust, but note that
     -- QuickCheck may take ≳100 tests to find a counterexample.
    , testProperty "derivative-clipping, l^1"
       $ QC.expectFailure
          . noiseRobust projectMonotone_derivativeClipping (V.sum . V.map abs)
    , testProperty "derivative-clipping, l^2"
       $ QC.expectFailure
          . noiseRobust projectMonotone_derivativeClipping (V.sum . V.map (^2))
    , testProperty "derivative-clipping, l^∞"
       $ QC.expectFailure
          . noiseRobust projectMonotone_derivativeClipping (V.maximum . V.map abs)
    ]
   , testGroup "Comparison of monotonizers"
     $ let drvClip = ("drvClipd", projectMonotone_derivativeClipping)
           iGrow   = ("iGrown", projectMonotone_lInftymin)
    in [ testProperty "    ‖iGrow‖ ≤ ‖drvClip‖  (l^∞)"
             . (iGrow`betterThan`drvClip)
             $ fmap V.maximum . V.zipWith (\r c -> abs $ c-r)
       , testProperty "NOT ‖iGrow‖ ≥ ‖drvClip‖  (l^∞)"
             . QC.expectFailure
             . (drvClip`betterThan`iGrow)
             $ fmap V.maximum . V.zipWith (\r c -> abs $ c-r)
--     , testProperty "NOT ‖iGrow‖ ≤ ‖drvClip‖  (l^2)  (but usually)"
--           . QC.expectFailure
--           . (iGrow`betterThan`drvClip)
--           $ fmap V.sum . V.zipWith (\r c -> (c-r)^2)
       , testProperty "NOT ‖iGrow‖ ≥ ‖drvClip‖  (l^2)"
             . QC.expectFailure
             . (drvClip`betterThan`iGrow)
             $ fmap V.sum . V.zipWith (\r c -> (c-r)^2)
--     , testProperty "NOT ‖iGrow‖ ≤ ‖drvClip‖  (l^1)"
--           . QC.expectFailure
--           . (iGrow`betterThan`drvClip)
--           $ fmap V.sum . V.zipWith (\r c -> abs $ c-r)
       , testProperty "NOT ‖iGrow‖ ≥ ‖drvClip‖  (l^1)"
             . QC.expectFailure
             . (drvClip`betterThan`iGrow)
             $ fmap V.sum . V.zipWith (\r c -> abs $ c-r)
       ]
   ]

-- A path whose endpoint is greater or equal to the start point (but
-- is not necessarily monotone in between).
newtype LegalPath a = LegalPath {getLegalPath :: Path a}
instance (Show a, V.Unbox a) => Show (LegalPath a) where
  show (LegalPath pth) = "LegalPath"++show pth

instance (a~Double) => QC.Arbitrary (LegalPath a) where
  arbitrary = do
    QC.NonEmpty pthl <- QC.arbitrary
    let pth = V.fromList pthl
    return . LegalPath $ if V.head pth > V.last pth
     then V.cons (V.minimum pth) pth
     else pth
  shrink (LegalPath pth)
     = LegalPath <$> filter (\pth' -> not (V.null pth') && V.head pth' <= V.last pth')
        (V.fromList <$> QC.shrink (V.toList pth))


betterThan :: (String, MonotoneProjector Double)
          -> (String, MonotoneProjector Double)
        -> (Path Double -> Path Double -> Double)
        -> LegalPath Double -> QC.Property
((mtc₀n, mtc₀)`betterThan`(mtc₁n, mtc₁)) metric (LegalPath pth)
      = QC.counterexample ( mtc₀n++"="++show mpth₀
                     ++": dev="++show (metric pth mpth₀)
                     ++", "++mtc₁n++"="++show mpth₁
                     ++": dev="++show (metric pth mpth₁) )
             $ metric pth mpth₀ <= metric pth mpth₁
 where [mpth₀, mpth₁] = getMonotonePath . ($pth) <$> [mtc₀, mtc₁]

noiseRobust :: MonotoneProjector Double
          -> (Path Double -> Double)
          -> MonotonePath Double
          -> QC.Property
noiseRobust projector metric (MonotonePath orig)
             = QC.forAll (V.fromList
                           <$> QC.vectorOf (V.length orig) QC.arbitrary)
        $ \noise -> V.length orig > 1 ==>
                    let perturbed = V.zipWith (+) orig noise
                        MonotonePath reMonotonized
                             = projector perturbed
                    in QC.counterexample ("Result: "++show reMonotonized)
                        $ metric (V.zipWith (-) orig reMonotonized)
                             <= metric noise*1.5

(≈≈≈) :: MonotonePath Double -> MonotonePath Double -> QC.Property
MonotonePath p ≈≈≈ MonotonePath q
 | V.and $ V.zipWith (\x y -> abs (x-y) < 1e-9) p q
     = p===p
 | otherwise
     = p===q
