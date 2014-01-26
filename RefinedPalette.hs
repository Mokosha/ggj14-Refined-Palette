module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import qualified Data.Map as Map

import Paths_refined_palette

import Constants
import Board
import Render
import Game
---------------------------------------------------------------------------------

gameCam :: LR.Camera
gameCam = LR.mkOrthoCamera
          (Vec3 0 0 15) (mkNormal $ neg vec3Z) (mkNormal vec3Y)
          0 screenSizeXf screenSizeYf 0 0.1 1000.0

initGame :: IO (LR.Game)
initGame = do
  tiles <- loadTiles
  bgs <- loadBGColorMap
  boardFiles <- mapM getDataFileName [
    "EASY01.txt",
    "EASY02.txt",
    "EASY03.txt",
    "HARD02.txt",
    "HARD03.txt"]
  levels <- mapM loadBoard boardFiles
--  music <- loadSounds
  light <- LR.createNoLight
  let gw = game levels tiles bgs Map.empty
  return $ LR.Game { LR.staticGameState = (gameCam, [light], []),
                     LR.mainCamera = LR.mkFixedCam gameCam,
                     LR.dynamicLights = [],
                     LR.gameObjects = [gw] }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Refined Palette"
  g <- initGame
  case m of
    (Just win) -> L.run win g
    Nothing -> return ()
  L.destroyWindow m
