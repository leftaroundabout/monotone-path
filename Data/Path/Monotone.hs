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
import qualified Test.QuickCheck as QC

type Path a = V.Vector a

newtype NonNegativePath a = NonNegativePath { getNonNegativePath :: Path a }
  deriving (Eq, Show)
newtype PositivePath a = PositivePath { getPositivePath :: Path a }
  deriving (Eq, Show)

instance (a~Double) => QC.Arbitrary (NonNegativePath a) where
  arbitrary = NonNegativePath . V.fromList . map QC.getNonNegative <$> QC.arbitrary
instance (a~Double) => QC.Arbitrary (PositivePath a) where
  arbitrary = PositivePath . V.fromList . map QC.getPositive <$> QC.arbitrary

-- | (Non-strictly) increasing path, i.e. a path that is nowhere decreasing:
--   two subsequent elements will be equal or the second greater.
newtype MonotonePath a = MonotonePath { getMonotonePath :: Path a }
  deriving (Eq, Show)

instance (a~Double) => QC.Arbitrary (MonotonePath a) where
  arbitrary = MonotonePath . V.tail . V.scanl (+) 0 . getNonNegativePath <$> QC.arbitrary
  shrink (MonotonePath pth) = MonotonePath . V.fromList
              <$> QC.shrinkList pure (V.toList pth)


-- | Strictly decreasing path, i.e. of two subsequent elements the first will
--   always be greater.
newtype DecreasingPath a = DecreasingPath { getDecreasingPath :: Path a }
  deriving (Eq, Show)

instance (a~Double) => QC.Arbitrary (DecreasingPath a) where
  arbitrary = DecreasingPath . V.tail . V.scanl (-) 0 . getPositivePath <$> QC.arbitrary
  shrink (DecreasingPath pth) = DecreasingPath . V.fromList
              <$> QC.shrinkList pure (V.toList pth)


data Extremum a = LocalMin a | LocalMax a
type Interval a = (a,a)

-- | Obtain all the disjoint ranges @(𝑖₀,𝑖₁)@ such that @pth ! i₀ > pth ! i₁@.
decreasingIntervals :: Path Double -> [Interval Int]
decreasingIntervals pth = groupIntvs $ V.ifoldr (classifyExtremum . (+1)) [] trios
 where classifyExtremum i (l,m,r)
         | m<l && m<r   = (LocalMin i :)
         | m>l && m>r   = (LocalMax i :)
         | otherwise    = id
       trios = V.zip3 pth (V.drop 1 pth) (V.drop 2 pth)

       groupIntvs (LocalMax i₀ : LocalMin i₁ : es) = (i₀, i₁) : groupIntvs es
       groupIntvs (              LocalMin i₁ : es) = (0 , i₁) : groupIntvs es
       groupIntvs (LocalMax i₀ : []              ) = [(i₀, V.length pth - 1)]
       groupIntvs [] = []
       groupIntvs _ = error "Order violation"

