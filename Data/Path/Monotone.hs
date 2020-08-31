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

-- | Obtain all the disjoint ranges @(𝑖₀,𝑖₁)@ such that @pth ! i₀ > pth ! i₁@.
decreasingIntervals :: Path Double -> [IntervalWRange Int Double]
decreasingIntervals pth = go 0
 where go i
         | i >= l-1    = []
         | pth V.! i > pth V.! (i+1)
         , i' <- decrEnd (i+1)
                       =  IntervalWRange i i' (pth V.! i) (pth V.! i') : go i'
         | otherwise   = go (i+1) 
       decrEnd i
         | i >= l-1                   = i
         | pth V.! i > pth V.! (i+1)  = decrEnd (i+1)
         | otherwise                  = i
       
       l = V.length pth
         


growDecreasingIntv :: Path Double -> IntervalWRange Int Double -> IntervalWRange Int Double
growDecreasingIntv pth (IntervalWRange il ir ymin ymax)
                   = IntervalWRange (goL il) (goR ir) ymin ymax
 where goL 0 = 0
       goL i
        | pth V.! (i-1) > ym  = goL $ i-1
        | otherwise           = i-1
       goR i
        | i >= len-1          = i
        | pth V.! (i+1) < ym  = goR $ i+1
        | otherwise           = i+1
       ym = (pth V.! il + pth V.! ir) / 2
       len = V.length pth

mergeOverlappingIntvs :: [(IntervalWRange Int Double, a)]
                      -> [(IntervalWRange Int Double, [a])]
mergeOverlappingIntvs [] = []
mergeOverlappingIntvs ((IntervalWRange xl₀ xr₀ yb₀ yt₀, a) : ivs)
                  = case break ((>xr₀+1).xMin.fst) $ mergeOverlappingIntvs ivs of
       (overlapping, rest)
           -> let yb = minimum $ yb₀ : (yMin.fst<$>overlapping)
                  yt = maximum $ yt₀ : (yMax.fst<$>overlapping)
                  xr = maximum $ xr₀ : (xMax.fst<$>overlapping)
              in (IntervalWRange xl₀ xr yb yt, a : (snd=<<overlapping)) : rest

projectMonotone_l¹min :: Path Double -> MonotonePath Double
projectMonotone_l¹min pth = MonotonePath
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
        where grown = (growDecreasingIntv pth &&& id)<$>ivs
              merged = mergeOverlappingIntvs grown

