module Game (game) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.Lambency as LR
import qualified Graphics.UI.Lambency as L

import qualified Graphics.UI.GLFW as GLFW

import Data.Traversable (sequenceA)
import Data.Vect.Float
import qualified Data.Map as Map

import Control.Monad.RWS
import Control.Wire as W

import Prelude hiding (Right, Left, (.))
import qualified Prelude as P (Either(..))

import Board
import Constants
--------------------------------------------------------------------------------

getRenderTile :: BoardPosition -> Tile -> Map.Map Tile LR.RenderObject -> LR.GameObject
getRenderTile pos t tiles = LR.mkStaticObject (tiles Map.! t) (boardPosToXForm pos)

getRenderGate :: Gate -> Map.Map Tile LR.RenderObject -> LR.GameObject
getRenderGate (gpos, dir, color) tiles =
  LR.mkStaticObject (tiles Map.! (BlockTile color BlankTile)) (boardWallToXForm gpos dir)

getBackground :: GameColor -> Map.Map GameColor LR.RenderObject -> LR.GameObject
getBackground color bgMap =
  LR.mkStaticObject (bgMap Map.! color) fullScreenXForm
  where fullScreenXForm =
          LR.nonuniformScale (Vec3 halfScreenXf halfScreenYf 1) $
          LR.translate (Vec3 halfScreenXf halfScreenYf (-1)) $
          LR.identity

clamp01 :: Float -> Float
clamp01 f
  | f < 0 = 0
  | f > 1 = 1
  | otherwise = f

smoothstep :: Float -> Float -> Float -> Float
smoothstep a b t = let
  ct = clamp01 t
  nt = ct*ct*ct*(ct*(ct*6 - 15) + 10)
  in (a * (1.0 - nt)) + (b * nt)

smoothstepV :: Vec3 -> Vec3 -> Float -> Vec3
smoothstepV (Vec3 ax ay az) (Vec3 bx by bz) t =
  Vec3 (smoothstep ax bx t) (smoothstep ay by t) (smoothstep az bz t)

animTime :: Float
animTime = 0.4

animWire :: (Board, [Gate]) -> [BlockAnim] ->
            Map.Map Tile LR.RenderObject ->
            Map.Map GameColor LR.RenderObject ->
            LR.GameWire a [LR.GameObject]
animWire (board, gates) anims tiles bgs = let
  notAnim :: BoardPosition -> Bool
  notAnim pos = not $ any (\(s, _, _) -> s == pos) anims
  
  objWire :: Float -> BlockAnim -> LR.GameWire a LR.GameObject
  objWire curTime (start, end, t) = let
    (sx, sy) = getTileCenter start
    (ex, ey) = getTileCenter end
    lerpVal = curTime / animTime
    xform = LR.nonuniformScale boardTileScale $
            LR.translate (smoothstepV (Vec3 sx sy 1) (Vec3 ex ey 1) lerpVal) $
            LR.identity
    in
     W.mkPure (\(W.Timed dt ()) _ ->
                (P.Right $ LR.mkStaticObject (tiles Map.! t) xform,
                 objWire (curTime + dt) (start, end, t)))

  staticWire :: BoardPosition -> Tile -> LR.GameWire a LR.GameObject
  staticWire pos tile = W.mkConst $ P.Right $ getRenderTile pos tile tiles

  boardTiles = board2List board

  in
   (sequenceA $
    (map (objWire 0) anims) ++
    (map (\(pos, t) -> staticWire pos t)
     (filter (\(pos, _) -> notAnim pos) boardTiles)) ++
    (map (W.mkConst . P.Right . (flip getRenderGate tiles)) gates))
   >>> W.for animTime

renderObjects :: (Board, [Gate]) ->
                 Map.Map Tile LR.RenderObject ->
                 [LR.GameObject]
renderObjects (board, gates) tiles = let
  in map (\f -> f tiles) $
     (map (uncurry getRenderTile) (board2List board)) ++
     (map getRenderGate) gates

-- Key pressed -> Can player move?
-- -- Yes? -> Switch to animation wire that inhibits after
-- -- -- --   the appropriate amount of time and return
-- -- -- --   to the game wire
-- -- No?  -> Do nothing

game :: [(Board, [Gate])] -> -- Initial board, gates, and next boards
        Map.Map Tile LR.RenderObject -> -- Tile objects
        Map.Map GameColor LR.RenderObject -> -- Background
        Map.Map GameColor L.SoundObject -> -- Music
        LR.GameWire a [LR.GameObject]

game [] _ _ _ = W.mkEmpty -- inhibit forevver
game ((board, gates) : next) tiles bg music = let

  runGame :: LR.GameWire a [LR.GameObject]
  runGame = W.mkGenN $ \_ -> do
    ipt <- get
    let nbs = map nextBoards $ filter (flip L.isKeyPressed ipt . dir2Key) dirs
    return $ case nbs of
      [] -> (P.Right frameObjs, game ((board, gates) : next) tiles bg music)
      ((d, anims) : _) ->
        (P.Right frameObjs,
         (animWire (board, gates) anims tiles bg W.--> game d tiles bg music))

  dirs :: [Direction]
  dirs = possibleDirections board gates

  dir2Key :: Direction -> GLFW.Key
  dir2Key Down = GLFW.Key'Down
  dir2Key Up = GLFW.Key'Up
  dir2Key Left = GLFW.Key'Left
  dir2Key Right = GLFW.Key'Right

  nextBoards :: Direction -> ([(Board, [Gate])], [BlockAnim])
  nextBoards dir = let
    (newBoard, anims) = movePlayer board dir gates
    in
      case (playerTile newBoard) of
        (_, PlayerTile playerColor (EndTile Open)) -> (next, anims)
        _ -> ((newBoard, gates) : next, anims)

  (_, PlayerTile playerColor underTile) = playerTile board

  frameObjs = renderObjects (board, gates) tiles -- ++ [getBackground playerColor]

  in
   W.mkGenN $ \_ -> do
     ipt <- get
     let nbs = map nextBoards $ filter (flip L.isKeyPressed ipt . dir2Key) dirs
     return $ case nbs of
       [] -> (P.Right frameObjs, game ((board, gates) : next) tiles bg music)
       ((d, anims) : _) -> (P.Right frameObjs,
                   (animWire (board, gates) anims tiles bg W.--> game d tiles bg music))
