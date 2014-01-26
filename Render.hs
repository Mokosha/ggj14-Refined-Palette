module Render (
  loadBGColorMap, loadTiles, loadSounds
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.Lambency as LR
import qualified Graphics.UI.Lambency as L

import qualified Data.Map as Map
import Data.Word

import System.FilePath
import Paths_refined_palette

import Prelude hiding (Left, Right)

import Board
--------------------------------------------------------------------------------

loadMaterial :: FilePath -> IO (LR.Material)
loadMaterial path = do
  (Just tex) <- LR.loadTextureFromPNG path
  return $ LR.createTexturedMaterial tex

coloredQuad :: (Word8, Word8, Word8, Word8) -> IO (LR.RenderObject)
coloredQuad c = LR.createSolidTexture c >>=
                (LR.createRenderObject LR.quad . LR.createTexturedMaterial)

texturedQuad :: FilePath -> IO (LR.RenderObject)
texturedQuad path = loadMaterial path >>= (LR.createRenderObject LR.quad)

type ColorMap = Map.Map GameColor LR.RenderObject
loadBGColorMap :: IO (ColorMap)
loadBGColorMap = do
  yellow <- getDataFileName ("flower-yellow-blue-pattern" <.> "png") >>= texturedQuad
  orange <- getDataFileName ("strange-fruit-orange-green-pattern" <.> "png") >>= texturedQuad
  return $ Map.fromList [
    (Yellow, yellow),
    (Orange, orange),
    (Green, yellow), -- FIXME
    (Blue, orange), -- FIXME
    (Red, yellow), -- FIXME
    (Purple, orange)] -- FIXME

loadSounds :: IO (Map.Map GameColor L.SoundObject)
loadSounds = do
  primSound <- getDataFileName ("SongAlphaPlus" <.> "wav") >>= L.loadSound
  secondSound <- getDataFileName ("SongBetaPlatinumDeluxe" <.> "wav") >>= L.loadSound
  return $ Map.fromList (
    [(c, primSound) | c <- [Red, Yellow, Blue]] ++
    [(c, secondSound) | c <- [Green, Orange, Purple]])

loadTiles :: IO (Map.Map Tile LR.RenderObject)
loadTiles = do
  solid <- coloredQuad (32, 32, 32, 255) -- texturedQuad $ "solid" <.> "png" -- SolidTile
  blank <- empty
  yellowBlock <- coloredQuad (255, 255, 0, 255) -- BlockTile Yellow
  blueBlock <- coloredQuad (0, 0, 255, 255) -- BlockTile Blue
  redBlock <- coloredQuad (255, 0, 0, 255) -- BlockTile Red
  greenBlock <- coloredQuad (0, 255, 0, 255) -- BlockTile Green
  purpleBlock <- coloredQuad (128, 0, 128, 255) -- BlockTile Purple
  orangeBlock <- coloredQuad (255, 128, 0, 255) -- BlockTile Orange
  let blockMapping :: GameColor -> LR.RenderObject
      blockMapping Yellow = yellowBlock
      blockMapping Red = redBlock
      blockMapping Blue = blueBlock
      blockMapping Green = greenBlock
      blockMapping Purple = purpleBlock
      blockMapping Orange = orangeBlock
  key <- coloredQuad (255, 255, 255, 255) -- texturedQuad $ "key" <.> "png" -- KeyTile
  end <- coloredQuad (128, 128, 0, 255) -- EndTile
  return $ Map.fromList $ concat [
    [(SolidTile, solid), (BlankTile, blank)],
    genMapping blockMapping,
    [(KeyTile, key), (EndTile Open, end), (EndTile Closed, end)],
    [(PlayerTile c s, blockMapping c) | c <- gameColors, s <- standableTiles]
   ]
    
  where
    empty = LR.createBasicRO [] [] Map.empty
      
    genMapping :: (GameColor -> LR.RenderObject) -> [(Tile, LR.RenderObject)]
    genMapping fn = [(BlockTile c s, fn c) | s <- standableTiles, c <- gameColors]
