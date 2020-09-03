-- |
-- Module      : Data.Path.Monotone
-- Copyright   : (c) Justus Sagemüller 2020
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies       #-}

module Data.Path.Monotone where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Test.QuickCheck as QC
import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Monad (forM_)
import Control.Arrow ((&&&))


type Path a = V.Vector a

newtype NonNegativePath a = NonNegativePath { getNonNegativePath :: Path a }
  deriving (Eq, Show)
newtype PositivePath a = PositivePath { getPositivePath :: Path a }
  deriving (Eq, Show)

instance (a~Double) => QC.Arbitrary (NonNegativePath a) where
  arbitrary = NonNegativePath . V.fromList . map QC.getNonNegative
                <$> QC.listOf1 QC.arbitrary
instance (a~Double) => QC.Arbitrary (PositivePath a) where
  arbitrary = PositivePath . V.fromList . map QC.getPositive
                <$> QC.listOf1 QC.arbitrary

-- | (Non-strictly) increasing path, i.e. a path that is nowhere decreasing:
--   two subsequent elements will be equal or the second greater.
newtype MonotonePath a = MonotonePath { getMonotonePath :: Path a }
  deriving (Eq, Show)

shrinkList1 :: (a -> [a]) -> [a] -> [[a]]
shrinkList1 f l = filter (\l' -> not (null l') && length l' < length l)
                    $ QC.shrinkList f l

instance (a~Double) => QC.Arbitrary (MonotonePath a) where
  arbitrary = MonotonePath . V.tail . V.scanl (+) 0 . getNonNegativePath <$> QC.arbitrary
  shrink (MonotonePath pth) = MonotonePath . V.fromList
              <$> shrinkList1 pure (V.toList pth)


-- | Strictly decreasing path, i.e. of two subsequent elements the first will
--   always be greater.
newtype DecreasingPath a = DecreasingPath { getDecreasingPath :: Path a }
  deriving (Eq, Show)

instance (a~Double) => QC.Arbitrary (DecreasingPath a) where
  arbitrary = DecreasingPath . V.tail . V.scanl (-) 0 . getPositivePath <$> QC.arbitrary
  shrink (DecreasingPath pth) = DecreasingPath . V.fromList
              <$> shrinkList1 pure (V.toList pth)


data Extremum a = LocalMin a | LocalMax a
data IntervalWRange x y = IntervalWRange
       { xMin, xMax :: x
       , yMin, yMax :: y }
  deriving (Eq, Show)

-- | Obtain all the disjoint ranges \((i_0,i_1)\) such that \(p_{i_0} > p_{i_1}\).
decreasingIntervals :: Path Double -> [IntervalWRange Int Double]
decreasingIntervals pth = go 0
 where go i
         | i >= l-1    = []
         | pth V.! i > pth V.! (i+1)
         , i' <- decrEnd (i+1)
                       =  IntervalWRange i i' (pth V.! i') (pth V.! i) : go i'
         | otherwise   = go (i+1)
       decrEnd i
         | i >= l-1                   = i
         | pth V.! i > pth V.! (i+1)  = decrEnd (i+1)
         | otherwise                  = i

       l = V.length pth


-- | Given an interval in which the path passes from a local maximum \(y_\text{max}\)
--   to a local minimum \(y_\text{min}\), extend it on both sides up to the points
--   where the non-monotonicity can be “ironed out” by replacing it (continuously)
--   with a constant segment, valued at the (\(\ell^\infty\)) mean between \(y_\text{min}\)
--   and \(y_\text{max}\).
growDecreasingIntv :: Path Double -> (Int,Int)
                    -> IntervalWRange Int Double -> IntervalWRange Int Double
growDecreasingIntv pth (illim,irlim) (IntervalWRange il ir ymin ymax)
                   = IntervalWRange (goL il) (goR ir) ymin ymax
 where goL i
        | i <= illim          = i
        | pth V.! (i-1) > ym  = goL $ i-1
        | otherwise           = i
       goR i
        | i >= irlim          = i
        | pth V.! (i+1) < ym  = goR $ i+1
        | otherwise           = i
       ym = (ymin+ymax) / 2
       len = V.length pth

mergeOverlappingIntvs :: [(IntervalWRange Int Double, a)]
                      -> [(IntervalWRange Int Double, [a])]
mergeOverlappingIntvs = go . sortBy (comparing $ xMin.fst)
                 -- Is it necessary to pre-sort the list? Testing suggest it isn't,
                 -- but in contrived examples (third interval much larger than second)
                 -- it certainly might be needed.
 where go [] = []
       go ((IntervalWRange xl₀ xr₀ yb₀ yt₀, a) : ivs)
                  = case break (\(irw',_) -> xMin irw' > xr₀+1
                                   || yMin irw'+yMax irw' > yb₀ + yt₀) $ go ivs of
        (overlapping, rest)
           -> let yb = minimum $ yb₀ : (yMin.fst<$>overlapping)
                  yt = maximum $ yt₀ : (yMax.fst<$>overlapping)
                  xr = maximum $ xr₀ : (xMax.fst<$>overlapping)
              in (IntervalWRange xl₀ xr yb yt, a : (snd=<<overlapping)) : rest

type MonotoneProjector a = Path a -> MonotonePath a

-- | Given a path \(p\), find a monotone path \(q\) such that \(\max_{x\in I} |p_x-q_x|\) is
--   minimal (i.e., this is a projection using the \(\ell^\infty\)-distance). Note
--   that this minimum is in general not unique.
projectMonotone_lInftymin :: MonotoneProjector Double
projectMonotone_lInftymin pth = MonotonePath
    $ V.create (do
        pthSt <- V.thaw pth
        forM_ (growAndMerge $ decreasingIntervals pth)
           $ \(IntervalWRange xl xr yb yt) -> do
          let ym = (yb+yt)/2
          forM_ [xl..xr] $ \x -> VM.write pthSt x ym
        return pthSt
       )
 where growAndMerge ivs
         | all (null . tail . snd) merged
             = fst <$> merged
         | otherwise
             = growAndMerge [ IntervalWRange xl xr yb yt
                            | (_, subIvs) <- merged
                            , let yb = minimum $ yMin<$>subIvs
                                  yt = maximum $ yMax<$>subIvs
                                  xl = minimum $ xMin<$>subIvs
                                  xr = maximum $ xMax<$>subIvs ]
        where grown = zipWith3 (\lb m rb -> (growDecreasingIntv pth (lb,rb) m, m))
                               (0 : (xMax<$>init ivs))
                               ivs
                               ((xMin<$>tail ivs) ++ [V.length pth-1])
              merged = mergeOverlappingIntvs grown


projectMonotone_derivativeClipping :: MonotoneProjector Double
projectMonotone_derivativeClipping pth
  | V.null pth  = MonotonePath V.empty
  | y₀ < ye     = MonotonePath $ V.map (\y -> y₀ + (y-y₀)*rescalingFactor) reIntgd
  | otherwise   = MonotonePath $ V.map (\_ -> (y₀+ye)/2) pth
 where y₀ = V.head pth
       ye = V.last pth
       derivsClipped = V.map (max 0) $ V.zipWith (-) (V.tail pth) pth
       reIntgd = V.scanl' (+) y₀ derivsClipped
       rescalingFactor = (ye-y₀) / (V.last reIntgd - V.head reIntgd)
