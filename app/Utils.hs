module Utils (
      numToNat, natToNum,
      Power(..), runPower,
      Point, PointN, pointNToPoint, pointToPointN,
      Rect, RectN, rectNToSDLRect,
      Color, invColor, whiteColor 
) where

-------------------------------------------------------------------------------
import Numeric.Natural   qualified as Nat
import SDL               qualified
import Data.Word         qualified as Word
import Data.Bifunctor    qualified as BiFunc
import Foreign.C         qualified as C

-------------------------------------------------------------------------------
numToNat :: (Integral a) => a -> Nat.Natural
numToNat = fromInteger . toInteger

natToNum :: (Num a) => Nat.Natural -> a
natToNum = fromInteger . toInteger
 
-------------------------------------------------------------------------------
data Power = Nat.Natural :^: Nat.Natural

runPower :: Power -> Nat.Natural
runPower (b :^: p) = b ^ p

-------------------------------------------------------------------------------
type Point n = (n, n) 

type PointN = Point Nat.Natural

pointNToPoint :: (Num a) => PointN -> Point a
pointNToPoint = BiFunc.bimap natToNum natToNum

pointToPointN :: (Integral a) => Point a -> PointN
pointToPointN = BiFunc.bimap numToNat numToNat

  -------------------------------------------------------------------------------
type Rect n = ((n, n), (n, n))

type RectN = Rect Nat.Natural

rectNToSDLRect :: RectN -> SDL.Rectangle C.CInt
rectNToSDLRect ((px, py), (sx, sy)) = SDL.Rectangle
    (SDL.P $ SDL.V2 (natToNum px) (natToNum py)) 
    (SDL.V2 (natToNum sx) (natToNum sy))

-------------------------------------------------------------------------------
type Color = SDL.V4 Word.Word8

invColor :: Color
invColor = SDL.V4 0 0 0 0

whiteColor :: Color
whiteColor = SDL.V4 255 255 255 255

-------------------------------------------------------------------------------
