module SDL.Settings.Type ( AppSettings(..), invertFractalPatternColor, defAppSettings ) where

-------------------------------------------------------------------------------
import Data.Matrix       qualified as Mat
import SDL               qualified 

-------------------------------------------------------------------------------
import Fractal.Type      ( FractalSettings (..), defFractalSettings )
import Utils             ( Color, PointN, invColor, pointNToPoint )

-------------------------------------------------------------------------------
data AppSettings = MkAppSettings {
  fractalSettings :: FractalSettings, visibleColor :: Color }

-------------------------------------------------------------------------------
defAppSettings :: AppSettings
defAppSettings = MkAppSettings {
  fractalSettings = defFractalSettings,
  visibleColor = SDL.V4 58 240 58 35 }

-------------------------------------------------------------------------------
invertFractalPatternColor :: PointN -> AppSettings -> AppSettings
invertFractalPatternColor pos app_set = app_set { fractalSettings =
          (fractalSettings app_set) { layer = f . layer . fractalSettings $ app_set} }  
    where cur_col = uncurry Mat.unsafeGet (pointNToPoint pos) 
                    $ layer . fractalSettings $ app_set
          new_col = if cur_col == visibleColor app_set then invColor else visibleColor app_set
          f = Mat.unsafeSet new_col (pointNToPoint pos)

-------------------------------------------------------------------------------
