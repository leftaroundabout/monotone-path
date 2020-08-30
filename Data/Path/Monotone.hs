-- |
-- Module      : Data.Path.Monotone
-- Copyright   : (c) Justus Sagemüller 2020
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 

module Data.Path.Monotone where

import qualified Data.Vector.Unboxed as V

type Path a = V.Vector a

newtype MonotonePath a = MonotonePath { getMonotonePath :: Path a }


