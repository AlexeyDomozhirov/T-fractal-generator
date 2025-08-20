{-# LANGUAGE FlexibleContexts #-}
module SDL.Draw ( draw ) where

-------------------------------------------------------------------------------
import Control.Monad.State     qualified as STMon
import Control.Monad.IO.Class  qualified as IOMon
import Control.Monad           qualified as ContMon
import Data.Foldable           qualified as Fold
import SDL                     qualified
import Data.Bifunctor          qualified as BiFunc
import Data.Matrix             qualified as Matr
import Data.Vector.Storable    qualified as Vect

-------------------------------------------------------------------------------
import SDL.State               ( AppState(..) )
import Fractal.Type            ( layer, largestPatternRectsSize )
import Utils                   ( pointToPointN, rectNToSDLRect, natToNum, invColor )
import SDL.Settings.Type       ( fractalSettings, AppSettings (..) )

-------------------------------------------------------------------------------
drawBorder :: (STMon.MonadState AppState m, IOMon.MonadIO m) => m ()
drawBorder = do
  (rx, ry) <- STMon.gets (largestPatternRectsSize . fractalSettings . appSettings)
  
  let rects_temp = BiFunc.bimap (pointToPointN . BiFunc.bimap (+(rx `div` 4)) (+(ry `div` 4)))  pointToPointN <$>
                       [((0, 0), (rx `div` 2, 1)), ((rx `div` 2, 0), (1, ry `div` 2)),
                        ((0, 0), (1, ry `div` 2)), ((0, ry `div` 2), (rx `div` 2, 1))]

  vis_col <- STMon.gets (visibleColor . appSettings)
  layer <- STMon.gets (layer . fractalSettings . appSettings)
  let (lx, ly) = (Matr.nrows layer, Matr.ncols layer) 

  renderer <- STMon.gets renderer
  Fold.traverse_ (uncurry (f renderer vis_col rects_temp)) [(pointToPointN (x * natToNum rx, y * natToNum ry),
                                                     Matr.getElem (y + 1) (x + 1) layer)
                                                     | x <- [0..Matr.ncols layer - 1], y <- [0..Matr.nrows layer - 1]] 
  where f renderer vis_col rects_temp (mx, my) cur_col = do 
          IOMon.liftIO $ SDL.rendererDrawColor renderer SDL.$=
             if cur_col == vis_col then SDL.V4 255 255 255 255 else invColor
          SDL.fillRects renderer (Vect.fromList $
             rectNToSDLRect . BiFunc.first (BiFunc.bimap (+ mx) (+ my)) <$> rects_temp)
          
-------------------------------------------------------------------------------
draw :: (STMon.MonadState AppState m, IOMon.MonadIO m) => m ()
draw = do
    renderer <- STMon.gets renderer

    IOMon.liftIO $ SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.fillRect renderer Nothing --fill screen

    STMon.gets borderMode >>= flip ContMon.when drawBorder

    STMon.gets fractal >>= Fold.traverse_ (f renderer)
    
    where f renderer (rects, col) = do
            SDL.rendererDrawColor renderer SDL.$= col
            SDL.fillRects renderer rects

-------------------------------------------------------------------------------
