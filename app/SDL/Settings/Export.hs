module SDL.Settings.Export ( exportAppSettings ) where

-------------------------------------------------------------------------------
import Numeric                qualified as Num 
import SDL                    qualified
import Data.Matrix            qualified as Matr
import Control.Monad.IO.Class qualified as IOMon

-------------------------------------------------------------------------------
import Utils                  ( Power(..), Color )
import SDL.Settings.Type      ( AppSettings(..) )
import Fractal.Type           ( FractalSettings(..) )

-------------------------------------------------------------------------------
exportAppSettings :: (IOMon.MonadIO m) => AppSettings -> m ()
exportAppSettings = IOMon.liftIO . print . showAppSettings

showAppSettings :: AppSettings -> String
showAppSettings (MkAppSettings (MkFractalSettings layer (base :^: power) _) vis_color) =
  show ptn_x ++ "," ++ show ptn_y ++ " " ++ show base ++ "^" ++ show power
  ++ " " ++ rgbaToHex vis_color ++ " " ++ ptn 
  where (ptn_x, ptn_y) = (Matr.nrows layer, Matr.ncols layer)
        ptn = map (\x -> if x == vis_color then '1' else '0') $ Matr.toList layer

-------------------------------------------------------------------------------
rgbaToHex :: Color -> String
rgbaToHex (SDL.V4 r g b a) = "#" ++ toHex r ++ toHex g ++ toHex b ++ toHex a
  where toHex n = let hex = Num.showHex n ""
                  in if length hex == 1 then "0" ++ hex else hex

-------------------------------------------------------------------------------
