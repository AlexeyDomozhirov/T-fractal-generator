{-# LANGUAGE TupleSections #-}
module Fractal.Generator ( genFractal, Fractal ) where
 
-------------------------------------------------------------------------------
import Numeric.Natural   qualified as Nat
import SDL               qualified
import Data.Matrix       qualified as Mtrx
import Data.List         qualified as List
import Data.Bifunctor    qualified as BiFunc

-------------------------------------------------------------------------------
import Fractal.Type      ( FractalSettings(..), FractalPattern, largestFractalRect )
import Utils             ( Color, RectN, PointN, Power(..),
                           numToNat, pointNToPoint, pointToPointN )
            
-------------------------------------------------------------------------------
divRect' :: (PointN -> a) -> RectN -> PointN -> [(RectN, a)]
divRect' f ((px, py), (sx, sy)) (div_x, div_y)
    | sx `rem` div_x /= 0 || sy `rem` div_y /= 0 = []
    | otherwise = BiFunc.first (,nrs) <$> [f' (x, y)
                    | x <- [1..div_x], y <- [1..div_y] ]
    where nrs@(nrsx, nrsy) = (sx `div` div_x, sy `div` div_y)
          f' p@(x, y) = ((px + nrsx * (x - 1), py + nrsy * (y - 1)), f p)

divRect :: RectN -> Nat.Natural -> [RectN]
divRect rect div_cnt = fst <$> divRect' (const ()) rect (div_cnt, div_cnt)

usePatternOnSubFractal :: RectN -> FractalPattern -> Fractal'
usePatternOnSubFractal rect ptn = divRect' (flip (uncurry Mtrx.getElem) ptn . f)
                        rect (pointToPointN (Mtrx.ncols ptn, Mtrx.nrows ptn))
    where f (x, y) = pointNToPoint (y, x)

-------------------------------------------------------------------------------
type Fractal' = [(RectN, Color)] --local intermediate representation 

type Fractal = [([RectN], Color)]

genFractal' :: FractalPattern -> Nat.Natural -> RectN -> Fractal'
genFractal' ptn _ cr@(_ ,(sx, _))
    | sx <= (numToNat . Mtrx.ncols $ ptn) = usePatternOnSubFractal cr ptn
genFractal' ptn div_cnt cr = usePatternOnSubFractal cr ptn ++
      mconcat (genFractal' ptn div_cnt <$> divRect cr div_cnt)

genFractal :: FractalSettings -> Fractal
genFractal fr_set@(MkFractalSettings {layer=ptn, size=bs :^: _}) =
    divFractalByColors . rmInvRectsFromFractal . 
    genFractal' ptn bs $ largestFractalRect fr_set

-------------------------------------------------------------------------------
divFractalByColors :: Fractal' -> Fractal
divFractalByColors fr = (\col -> (f col fr, col)) <$> uniq_col
    where uniq_col = List.nub . (snd <$>) $ fr 
          f col = (fst <$>) . filter ((==col) . snd)

rmInvRectsFromFractal :: Fractal' -> Fractal'
rmInvRectsFromFractal = filter (not . isInv . snd)
    where isInv (SDL.V4 _ _ _ 0) = True
          isInv _ = False

-------------------------------------------------------------------------------
