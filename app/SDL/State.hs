module SDL.State ( AppState(..), defAppState ) where

-------------------------------------------------------------------------------
import SDL                qualified

-------------------------------------------------------------------------------
import SDL.Settings.Type ( AppSettings, fractalSettings )
import Fractal.SDL       ( SDLFractal,  genSDLFractal )

-------------------------------------------------------------------------------
data AppState = MkAppState
    { appSettings :: AppSettings,
      fractal :: SDLFractal,
      quit :: Bool, borderMode :: Bool,
      dynamicRedraw :: Bool, 
      renderer :: SDL.Renderer  }

-------------------------------------------------------------------------------
defAppState :: AppSettings -> SDL.Renderer -> AppState
defAppState app_set renderer = MkAppState {
    appSettings = app_set,
    fractal = genSDLFractal . fractalSettings $ app_set,
    borderMode = False, renderer = renderer, quit = False,
    dynamicRedraw = True } 

-------------------------------------------------------------------------------
