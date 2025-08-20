module Main where

-------------------------------------------------------------------------------
import System.Environment qualified as Env
import SDL                qualified 
import Control.Monad      qualified as ContMon 

-------------------------------------------------------------------------------
import SDL.Loop           ( initApp, appLoop )
import SDL.Settings.Parse ( parseDataFromEnv )
import SDL.Settings.Type  ( defAppSettings )

-------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- Env.getArgs
    app_set <- case parseDataFromEnv args of
      Nothing -> ContMon.unless (null args) (print msg) >> return defAppSettings
      (Just app_set') -> return app_set'
    initApp app_set >>= appLoop >> SDL.quit
      where msg = "args parse error, fallback to default settings..." 

-------------------------------------------------------------------------------
