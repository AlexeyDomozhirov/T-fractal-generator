module SDL.Settings.Parse ( parseDataFromEnv ) where

-------------------------------------------------------------------------------
import SDL               qualified
import Data.Matrix       qualified as Mtrx
import Data.List         qualified as List
import Numeric           qualified as Num
import Text.Read         qualified as Read
import Data.Maybe        qualified as Mayb

-------------------------------------------------------------------------------
import Utils             ( Color, invColor, Power (..), Point )
import SDL.Settings.Type ( AppSettings(..) )
import Fractal.Type      ( FractalSettings (..), FractalPattern )

-------------------------------------------------------------------------------
parseDataFromEnv :: [String] -> Maybe AppSettings
parseDataFromEnv args
   | length args == 3 = parseDataFromEnv' (\_ (ptn_x, ptn_y) -> Just $
                                             Mtrx.matrix ptn_x ptn_y (const invColor)) args
   | length args == 4 = parseDataFromEnv' (f (args !! 3)) (take 3 args)
   where f l col (ptn_x, ptn_y) | not (all (`elem` "01") l) || length l /= ptn_x * ptn_y = Nothing
                                | otherwise = Just $ Mtrx.fromList ptn_x ptn_y $
                                                     map (\x -> if x == '1' then col else invColor) l
parseDataFromEnv _ = Nothing

parseDataFromEnv' :: (Color -> Point Int -> Maybe FractalPattern) -> [String] -> Maybe AppSettings
parseDataFromEnv' toPtn [ptn_size, win_size, vis_col_hex] = do
  vis_col_rgba   <- hexToRGBA vis_col_hex
  ptn            <- f ',' ptn_size >>= toPtn vis_col_rgba
  (base, power)  <- f '^' win_size
  return MkAppSettings {
    fractalSettings = MkFractalSettings {
        layer = ptn,
        size = base :^: power,
        start = (0, 0) },
    visibleColor = vis_col_rgba }
    where f sym str = do
            ind <- List.elemIndex sym str
            (,) <$> (Read.readMaybe . take ind $ str)
                <*> (f' (drop ind str) >>= Read.readMaybe)
          f' [] = Nothing
          f' (_:xs) = Just xs
parseDataFromEnv' _ _ = Nothing

-------------------------------------------------------------------------------
hexToRGBA :: String -> Maybe Color
hexToRGBA ['#', r',r'', g',g'', b',b'', a',a''] =
          SDL.V4 <$> f r' r'' <*> f g' g'' <*> f b' b'' <*> f a' a''
  where f c' c'' = (fst <$>) . Mayb.listToMaybe . Num.readHex $ c':[c'']
hexToRGBA _ = Nothing

-------------------------------------------------------------------------------
