{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module SDL.Event ( handleEvent ) where

-------------------------------------------------------------------------------
import Control.Monad.State.Class qualified as STMon
import Control.Monad.IO.Class    qualified as IOMon
import SDL                       qualified
import Data.Bifunctor            qualified as BiFunc
import Data.Tuple                qualified as Tupl
import Control.Monad             qualified as ContMon

-------------------------------------------------------------------------------
import Fractal.Type              ( largestPatternRectsSize )
import SDL.Settings.Type         ( AppSettings(..), invertFractalPatternColor )
import SDL.State                 ( AppState (..) )
import Utils                     ( numToNat )
import Fractal.SDL               ( genSDLFractal )
import SDL.Settings.Export       ( exportAppSettings )

-------------------------------------------------------------------------------
handleEvent :: (STMon.MonadState AppState m, IOMon.MonadIO m) => SDL.EventPayload -> m ()
handleEvent SDL.QuitEvent = STMon.modify \x -> x { quit = True }
handleEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _
             SDL.Pressed False (SDL.Keysym _ (SDL.Keycode 113) _))) = --q key
                            STMon.modify \x -> x { quit = True }
handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _
             SDL.Pressed _ SDL.ButtonLeft _ (SDL.P (SDL.V2 cx cy)))) = do
                app_set <- STMon.gets appSettings
                
                let new_app_set = flip invertFractalPatternColor app_set . Tupl.swap 
                          . BiFunc.bimap ((+1) . (numToNat cx `div`)) ((+1) . (numToNat cy `div`))
                          . largestPatternRectsSize . fractalSettings $ app_set 

                dyn_redraw <- STMon.gets dynamicRedraw
                STMon.modify (\x -> x {
                    appSettings = new_app_set })
                ContMon.when dyn_redraw (STMon.modify (\x -> x {
                    fractal = genSDLFractal (fractalSettings new_app_set) }))                   
handleEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _
             SDL.Pressed False (SDL.Keysym _ (SDL.Keycode 32) _))) = --space key
             STMon.modify \x -> x { borderMode = not . borderMode $ x}
handleEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _
             SDL.Pressed False (SDL.Keysym _ (SDL.Keycode 100) _))) = do --d key
               STMon.gets dynamicRedraw >>= flip ContMon.unless (STMon.modify (\x -> x {
                    fractal = genSDLFractal (fractalSettings . appSettings $ x)}))
               STMon.modify \x -> x { dynamicRedraw = not . dynamicRedraw $ x}
handleEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _
             SDL.Pressed False (SDL.Keysym _ (SDL.Keycode 101) _))) = --e key
               STMon.gets appSettings >>= exportAppSettings
handleEvent _ = return ()

-------------------------------------------------------------------------------

