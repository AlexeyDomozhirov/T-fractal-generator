{-# LANGUAGE TypeApplications #-}
module Fractal.Type (
    FractalPattern, FractalSettings(..), defFractalSettings,
    largestFractalRect, largestPatternRectsSize 
) where

-------------------------------------------------------------------------------
import Data.Matrix    qualified as Mtrx
import Data.Bifunctor qualified as BiFunc

-------------------------------------------------------------------------------
import Utils          ( PointN, Power(..), Color, invColor,
                        RectN, numToNat, runPower, pointToPointN )

-------------------------------------------------------------------------------
type FractalPattern = Mtrx.Matrix Color

data FractalSettings = MkFractalSettings
    { layer :: FractalPattern, size :: Power, start :: PointN  }

-------------------------------------------------------------------------------
largestFractalRect :: FractalSettings -> RectN
largestFractalRect (MkFractalSettings ptr pow st) = (st,
     (runPower pow * (numToNat @Int $ Mtrx.ncols ptr),
      runPower pow * (numToNat @Int $ Mtrx.nrows ptr)))

largestPatternRectsSize :: FractalSettings -> PointN
largestPatternRectsSize fr_set = BiFunc.bimap (`div` psx) (`div` psy)
                                 . snd . largestFractalRect $ fr_set
      where (psx, psy) = pointToPointN (Mtrx.ncols $ layer fr_set, Mtrx.nrows $ layer fr_set)
           
-------------------------------------------------------------------------------
defFractalSettings :: FractalSettings
defFractalSettings = MkFractalSettings {
      layer = Mtrx.matrix 4 4 (const invColor)
    , size = 2 :^: 7
    , start = (0, 0) }

-------------------------------------------------------------------------------
