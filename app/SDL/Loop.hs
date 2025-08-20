{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module SDL.Loop ( appLoop, initApp ) where
-------------------------------------------------------------------------------
import Control.Monad.State    qualified as STMon
import Control.Monad.IO.Class qualified as IO
import Foreign.C.Types        qualified as C
import SDL                    qualified 
import Control.Monad          qualified as ContMon
import Data.Foldable          qualified as Fold
import Data.Text              qualified as Text

-------------------------------------------------------------------------------
import SDL.Event              ( handleEvent )
import SDL.Draw               ( draw )
import Fractal.Type           ( largestFractalRect )
import Utils                  ( pointNToPoint )
import SDL.State              ( AppState (..), defAppState )
import SDL.Settings.Type      ( AppSettings (..) )

-------------------------------------------------------------------------------
appLoop' :: (STMon.MonadState AppState m, IO.MonadIO m) => m ()
appLoop' = do
  events <- (SDL.eventPayload <$>) <$> SDL.pollEvents
  Fold.traverse_ handleEvent events
  STMon.gets (not . quit) >>= flip ContMon.when (redraw >> appLoop') 
    where redraw = do
            renderer <- STMon.gets renderer
            SDL.clear renderer >> draw >> SDL.present renderer 

appLoop :: AppState -> IO ()
appLoop app_state = STMon.runStateT appLoop' app_state >>= const (return ())

-------------------------------------------------------------------------------
initApp :: AppSettings -> IO AppState
initApp app_set = do
        SDL.initialize [SDL.InitEvents, SDL.InitVideo]
        win <- SDL.createWindow (Text.pack "T fractal generator") SDL.defaultWindow
        SDL.showWindow win
        SDL.windowSize win SDL.$= (f . pointNToPoint @C.CInt .
             snd . largestFractalRect . fractalSettings $ app_set)
        render <- SDL.createRenderer win (-1) SDL.defaultRenderer
        SDL.rendererDrawBlendMode render SDL.$= SDL.BlendAlphaBlend
        SDL.showWindow win
        return $ defAppState app_set render
        where f (x, y) = SDL.V2 x y

-------------------------------------------------------------------------------
