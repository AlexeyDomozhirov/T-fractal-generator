module Fractal.SDL ( genSDLFractal, SDLFractal ) where

-------------------------------------------------------------------------------
import Data.Vector.Storable qualified as Vect
import SDL                  qualified
import Foreign.C            qualified as C
import Data.Bifunctor       qualified as BiFunc

-------------------------------------------------------------------------------
import Fractal.Generator    ( Fractal, genFractal )
import Fractal.Type         ( FractalSettings )
import Utils                ( Color, rectNToSDLRect )

-------------------------------------------------------------------------------
type SDLFractal = [(Vect.Vector (SDL.Rectangle C.CInt), Color)]

-------------------------------------------------------------------------------
fractalToSDL :: Fractal -> SDLFractal
fractalToSDL = (BiFunc.first (Vect.fromList . (rectNToSDLRect <$>)) <$>)

genSDLFractal :: FractalSettings -> SDLFractal
genSDLFractal = fractalToSDL . genFractal

-------------------------------------------------------------------------------
