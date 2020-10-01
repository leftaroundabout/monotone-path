{-# LANGUAGE TupleSections           #-}

import Graphics.Dynamic.Plot.R2
import Data.Path.Monotone
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Cairo as Dia
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Data.Default.Class
import Text.Printf
import Control.Lens ((&), (.~))

examplePath :: Path Double
examplePath = V.fromList [f t | t <- [0, 1/n .. 1]]
 where f t = (tanh (d + s 3 / 7 - s 4 / 2.5 + s 8 / 5 - s 9 /7) + 1)/2
        where s ν = sin $ pi*ν*t
              d = atanh $ t*2 - 1
       n = 200

pathPlot :: Path Double -> DynamicPlottable
pathPlot pth = lineSegPlot $ V.ifoldr
           (\i v r -> (fromPathIndex pth i, v) : r) [] pth

fromPathIndex :: Path Double -> Int -> Double
fromPathIndex pth = \i -> fromIntegral i/fromIntegral n
 where n = V.length pth

main :: IO ()
main = do
  mapM_ (\(i,plts) -> do
      preRd <- plotPrerender (def & xResV.~640 & yResV.~240
                                  & prerenderScaling .~ OutputCoordsScaling) plts
      Dia.renderCairo (printf "/tmp/monotoniseEx%i.eps" i)
                      (Dia.mkSizeSpec $ Dia.r2 (Just 640, Just 240))
                      preRd
     ) $ zip [0::Int ..]
      [ [ pathPlot examplePath ]
      , [ plotMultiple
              [ pathPlot examplePath
              , shapePlot $ mconcat
                 [ Dia.fromVertices (Dia.p2
                    <$> [(l,b), (r,b), (r,t), (l,t), (l,b)] )
                   <> (Dia.fromVertices (Dia.p2
                    <$> [(l,m), (r,m)] )
                    & Dia.lwO 3 )
                 | IntervalWRange li ri b t <- decreasingIntervals examplePath
                 , let [l, r] = fromPathIndex examplePath <$> [li,ri]
                 , let m = (t+b)/2 ]
              ] ]
      , [ plotMultiple
              [ pathPlot examplePath
              , shapePlot . mconcat $
                 [ Dia.fromVertices (Dia.p2
                    <$> [(l,b), (r,b), (r,t), (l,t), (l,b)] )
                    & Dia.lwO 0.5
                 | IntervalWRange li ri b t <- decreasingIntervals examplePath
                 , let [l, r] = fromPathIndex examplePath <$> [li,ri]
                 , let m = (t+b)/2 ] ++
                 [ Dia.fromVertices (Dia.p2
                    <$> [(l,m), (r,m)] )
                    & Dia.lwO 3
                 | IntervalWRange li ri b t
                     <- growDecreasingIntv examplePath (0,maxBound)
                      <$> decreasingIntervals examplePath
                 , let [l, r] = fromPathIndex examplePath <$> [li,ri]
                 , let m = (t+b)/2 ]
              ] ]
      , [ plotMultiple
              [ pathPlot examplePath
              , shapePlot $ mconcat
                 [ Dia.fromVertices (Dia.p2
                    <$> [(l,m), (r,m)] )
                    & Dia.lwO 3
                 | (IntervalWRange li ri b t, _)
                     <- mergeOverlappingIntvs
                       $ (,()) . growDecreasingIntv examplePath (0,maxBound)
                      <$> decreasingIntervals examplePath
                 , let [l, r] = fromPathIndex examplePath <$> [li,ri]
                 , let m = (t+b)/2 ]
              ] ]
      , [ pathPlot . getMonotonePath $ projectMonotone_lInftymin examplePath ]
      ]
  return ()
