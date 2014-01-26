module Board (
  GameColor(..), gameColors,
  Tile(..), EndTileState(..), standableTiles, BlockAnim,
  Direction(..), RFState(..),
  Board, BoardPosition, loadBoard,
  boardPosToXForm, boardWallToXForm, boardTileScale, boardWallScale,
  board2List, playerTile,
  Gate,
  possibleDirections, movePlayer,
  getTileCenter, getBorderCenter,
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.Lambency as LR
import qualified Data.Vector as V
import Data.Vect.Float

import Control.Monad.Reader

import Prelude hiding (Left, Right)

import Constants

import Debug.Trace
--------------------------------------------------------------------------------

data GameColor = Red | Purple | Blue | Green | Yellow | Orange
               deriving (Show, Eq, Ord)
type BoardPosition = (Int, Int)

data EndTileState = Open | Closed
                  deriving (Show, Eq, Ord)

data Tile = SolidTile
          | BlankTile
          | KeyTile
          | EndTile EndTileState
          | BlockTile GameColor Tile
          | PlayerTile GameColor Tile
          deriving (Show, Eq, Ord)

-- Tiles you can stand on
standableTiles :: [Tile]
standableTiles = [BlankTile, KeyTile, EndTile Open, EndTile Closed]

type Board = V.Vector (V.Vector Tile)

mapBoard :: (Tile -> Tile) -> Board -> Board
mapBoard fn = V.map (\v -> V.map fn v)

anyBoard :: (Tile -> Bool) -> Board -> Bool
anyBoard fn = V.any (\v -> V.any fn v)

allBoard :: (Tile -> Bool) -> Board -> Bool
allBoard fn = V.all (\v -> V.all fn v)

data Direction = Up | Down | Left | Right
               deriving (Show, Eq, Ord)

type Gate = (BoardPosition, Direction, GameColor)

data RFState = BoardState Board
             | GateState Gate
             | GameColorState GameColor
               
gameColors :: [GameColor]
gameColors = [Red , Purple , Blue , Green , Yellow , Orange]

-- Player Color through Gate gives new Player Color
throughGate :: GameColor -> GameColor -> GameColor
throughGate Red Blue = Purple
throughGate Blue Red = Purple
throughGate Red Yellow = Orange
throughGate Yellow Red = Orange
throughGate Blue Yellow = Green
throughGate Yellow Blue = Green
throughGate Orange Purple = Red
throughGate Purple Orange = Red
throughGate Orange Green = Yellow
throughGate Green Orange = Yellow
throughGate Purple Green = Blue
throughGate Green Purple = Blue
throughGate c c'
  | c == c' = c
  | otherwise = error "Invalid gate movement!"
-- | otherwise = c
-- !DEBUG!

isPrimaryColor :: GameColor -> Bool
isPrimaryColor Red = True
isPrimaryColor Blue = True
isPrimaryColor Yellow = True
isPrimaryColor _ = False

oppositeDir :: Direction -> Direction
oppositeDir Up = Down
oppositeDir Down = Up
oppositeDir Left = Right
oppositeDir Right = Left

getNeighborPos :: BoardPosition -> Direction -> BoardPosition
getNeighborPos (tx, ty) Down
  | ty == boardSizeY - 1 = (tx, ty)
  | otherwise = (tx, ty + 1)
getNeighborPos (tx, 0) Up = (tx, 0)
getNeighborPos (tx, ty) Up = (tx, ty-1)
getNeighborPos (0, ty) Left = (0, ty)
getNeighborPos (tx, ty) Left = (tx - 1, ty)
getNeighborPos (tx, ty) Right
  | tx == boardSizeX - 1 = (tx, ty)
  | otherwise = (tx + 1, ty)

board2List :: Board -> [(BoardPosition, Tile)]
board2List b = do
  (i', v) <- zip [0,1..] (V.toList b)
  (i, (j, t)) <- zip (repeat i') $ zip [0,1..] (V.toList v)
  return ((j, i), t)

queryGates :: [Gate] -> BoardPosition -> Direction -> Maybe GameColor
queryGates [] _ _ = Nothing
queryGates ((bp', d', color) : rest) bp d =
  if (bp == bp' && d == d') ||
     ((getNeighborPos bp' d') == bp && (oppositeDir d' == d))
  then Just color
  else queryGates rest bp d

queryBoard :: Board -> BoardPosition -> Tile
queryBoard b (tx, ty) = (b V.! ty) V.! tx

setBoardTile :: BoardPosition -> Tile -> Board -> Board
setBoardTile (tx, ty) t b = let
  setRow :: V.Vector Tile -> Int -> V.Vector Tile
  setRow row idx = row V.// [(idx, t)]
  in
   b V.// [(ty, setRow (b V.! ty) tx)]

getNeighbor :: Board -> BoardPosition -> Direction -> Tile
getNeighbor b p d = queryBoard b $ getNeighborPos p d

boundary :: BoardPosition -> Direction -> Bool
boundary (_, y) Down = boardSizeY - 1 == y
boundary (_, y) Up = 0 == y
boundary (x, _) Left = 0 == x
boundary (x, _) Right = boardSizeX - 1 == x

isPlayer :: Tile -> Bool
isPlayer (PlayerTile _ _) = True
isPlayer _ = False

keysExist :: Board -> Bool
keysExist = let
  keyExists :: Tile -> Bool
  keyExists KeyTile = True
  keyExists (BlockTile _ t) = keyExists t
  keyExists (PlayerTile _ t) = keyExists t
  keyExists _ = False
  in anyBoard keyExists

keysCovered :: Board -> Bool
keysCovered = let
  covered :: Tile -> Bool
  covered KeyTile = False
  covered _ = True
  in allBoard covered

openExit :: Board -> Board
openExit = let
  openExits :: Tile -> Tile
  openExits (EndTile Closed) = EndTile Open
  openExits t = t
  in mapBoard openExits

closeExit :: Board -> Board
closeExit = let
  closeExits :: Tile -> Tile
  closeExits (EndTile Open) = EndTile Closed
  closeExits t = t
  in mapBoard closeExits

playerTile :: Board -> (BoardPosition, Tile)
playerTile b = head $ filter (isPlayer . snd) (board2List b)

type BlockAnim = (BoardPosition, BoardPosition, Tile)
movePlayer :: Board -> Direction -> [Gate] -> (Board, [BlockAnim])
movePlayer b dir gates = let
  (pos, PlayerTile pc underTile) = playerTile b
  newColor =
    case queryGates gates pos dir of
      Nothing -> pc
      Just c -> throughGate pc c

  moveBlock :: Board -> BoardPosition -> (Board, [BlockAnim])
  moveBlock b' bp = let
    curTile = queryBoard b' bp
    npos' = getNeighborPos bp dir
    neighbor = queryBoard b' npos
    blockAnim = (bp, npos', curTile)
    in
     case curTile of
       BlockTile gc under ->
         case neighbor of
           BlockTile _ u -> let
             (nextBoard, nextAnims) = moveBlock b' npos'
             in (setBoardTile bp under $
                 setBoardTile npos' (BlockTile gc u) $
                 nextBoard, blockAnim : nextAnims)
           t -> (setBoardTile bp under $
                 setBoardTile npos' (BlockTile gc t) b', [blockAnim])
       _ -> (b', [])

  npos :: BoardPosition
  npos = getNeighborPos pos dir

  playerAnim :: BlockAnim
  playerAnim = (pos, npos, PlayerTile pc underTile)

  (newBoard, newAnims) = case queryBoard b npos of
     BlockTile _ u -> let
       (nextBoard, nextAnims) = moveBlock b npos
       in
        (setBoardTile pos underTile $
         setBoardTile npos (PlayerTile newColor u) $
         nextBoard, playerAnim : nextAnims)
                      
     t -> (setBoardTile pos underTile $
           setBoardTile npos (PlayerTile newColor t) b,
           [playerAnim])
  in
   (if keysExist newBoard && (not $ keysCovered newBoard)
    then closeExit newBoard
    else openExit newBoard, newAnims)

possibleDirections :: Board -> [Gate] -> [Direction]
possibleDirections b gates = let
  (pos, PlayerTile pc _) = playerTile b

  canGameGo :: Direction -> Bool
  canGameGo dir = let
    neighborPos :: BoardPosition
    neighborPos = getNeighborPos pos dir
    
    neighborTile :: Tile
    neighborTile = queryBoard b neighborPos
    
    canGoThroughGate :: Bool
    canGoThroughGate = case queryGates gates pos dir of
      Nothing -> True
      Just c -> (isPrimaryColor c) == (isPrimaryColor pc)

    canPushBlock :: GameColor -> BoardPosition -> Bool
    canPushBlock color bp = let
      canPushBlockThroughGate :: Bool
      canPushBlockThroughGate = 
        case queryGates gates bp dir of
          Nothing -> True
          Just c -> (isPrimaryColor c) == (isPrimaryColor color)

      canPushBlockOnBoard :: Bool
      canPushBlockOnBoard = (not $ boundary bp dir) && (
        case getNeighbor b bp dir of
          SolidTile -> False
          BlockTile c _ -> canPushBlock c (getNeighborPos bp dir)
          _ -> True)

      in canPushBlockThroughGate && canPushBlockOnBoard

    canStep :: Bool
    canStep = case neighborTile of
      SolidTile -> False
      BlockTile c _ -> if (c == pc)
                       then canPushBlock c (getNeighborPos pos dir)
                       else False
      _ -> True

    in
     canGoThroughGate && canStep

  canGo :: Direction -> Bool
  canGo dir = let
    atBoundary = boundary pos dir
    result = (not atBoundary) && (canGameGo dir)
    in 
     result

  in
   filter canGo [Up, Down, Left, Right]

boardTileScale :: Vec3
boardTileScale = (Vec3 (tileSizeXf * 0.5) (tileSizeYf * 0.5) 1)

boardPosToXForm :: BoardPosition -> LR.Transform
boardPosToXForm pos = let
  (x, y) = getTileCenter pos
  in LR.nonuniformScale boardTileScale $
     LR.translate (Vec3 x y 0) $
     LR.identity

boardWallScale :: Direction -> Vec3
boardWallScale dir = let
  isHoriz :: Direction -> Bool
  isHoriz Left = True
  isHoriz Right = True
  isHoriz _ = False
  
  vScale = if isHoriz dir then (tileSizeXf * 0.5) else (tileSizeXf * 0.05)
  hScale = if isHoriz dir then (tileSizeXf * 0.05) else (tileSizeXf * 0.5)
  in
   Vec3 hScale vScale 1

boardWallToXForm :: BoardPosition -> Direction -> LR.Transform
boardWallToXForm pos dir = let
  (x, y) = getBorderCenter pos dir
  in LR.nonuniformScale (boardWallScale dir) $
     LR.translate (Vec3 x y 0) $
     LR.identity

everyOther :: [a] -> [a]
everyOther list = let
  everyEven :: [a] -> Int -> [a]
  everyEven [] _ = []
  everyEven (x : xs) n
    | odd n = everyEven xs (n+1)
    | otherwise = x : everyEven xs (n+1)
  in everyEven list 1

loadBoard :: FilePath -> IO (Board, [Gate])
loadBoard path = do
  contents <- readFile path
  let result = parseLines $ lines contents
  print $ "Loaded: " ++ path
  return result
  where
    defaultColor :: GameColor
    defaultColor = Yellow

    intToTile :: Int -> Tile
    intToTile 0 = SolidTile
    intToTile 1 = BlankTile
    intToTile 2 = PlayerTile defaultColor BlankTile
    intToTile 3 = EndTile Closed
    intToTile 4 = error "Switch??"
    intToTile 5 = KeyTile
    intToTile 6 = BlockTile Red BlankTile
    intToTile 7 = BlockTile Blue BlankTile
    intToTile 8 = BlockTile Yellow BlankTile
    intToTile 9 = BlockTile Green BlankTile
    intToTile 10 = BlockTile Orange BlankTile
    intToTile 11 = BlockTile Purple BlankTile
    intToTile _ = error "Unsupported tile!"

    int2Color :: Int -> GameColor
    int2Color 0 = Red
    int2Color 1 = Blue
    int2Color 2 = Yellow
    int2Color 3 = Green
    int2Color 4 = Orange
    int2Color 5 = Purple
    int2Color _ = error "Unsupported color!"

    int2Dir :: Int -> Direction
    int2Dir 0 = Up
    int2Dir 1 = Right
    int2Dir 2 = Down
    int2Dir 3 = Left
    int2Dir _ = error "Unsupported direction!"

    cvtToBoard :: [String] -> Board
    cvtToBoard lns = 
      V.fromList $ map V.fromList $
      map ((map intToTile) . (map read) . words) $ everyOther lns
    
    parseLines :: [String] -> (Board, [Gate])
    parseLines = parseColor . parseGates . parseBoard

    parseBoard :: [String] -> (Board, [String])
    parseBoard (_ : rest) = (cvtToBoard (take 20 rest), drop 20 rest)
    parseBoard _ = error "Board parse error"

    parseGates :: (Board, [String]) -> (Board, [Gate], [String])
    parseGates (board, _ : _ : _ : gates) = let
      parseGate :: String -> Gate
      parseGate str = let
        (colStr,rest) = splitAt 1 str
        col = read colStr
        [row, dirInt, colorInt] = map read (words $ tail rest)
        in
--         trace ("Parsed gate: " ++ (show ((col, row), int2Dir dirInt, int2Color colorInt)))
        ((col, row), int2Dir dirInt, int2Color colorInt)
      
      gatesSoFar :: [String] -> [Gate] -> ([Gate], [String])
      gatesSoFar (l : lns) gs =
--        trace ("Parsing gate: " ++ (show l)) $
        if l == "" then (gs, lns) else gatesSoFar lns (parseGate l : gs)
      gatesSoFar lns gs = error $ "Gate parse error! <" ++ (show lns) ++ "," ++ (show gs) ++ ">"
      in
       (\(x, (y, z)) -> (x, y, z)) (board, gatesSoFar gates [])
       
    parseGates _ = error "Gates parse error!"

    parseColor :: (Board, [Gate], [String]) -> (Board, [Gate])
    parseColor (b, gs, lns) = let
      (pos, PlayerTile _ u) = playerTile b
      findColor :: [String] -> GameColor
      findColor [] = error "Cannot find initial player color!"
      findColor (l : rest) = if length l > 0 && head l `elem` "012345"
                             then (int2Color . read . (: []) . head) l
                             else findColor rest
      in
       (setBoardTile pos (PlayerTile (findColor lns) u) b, gs)

getTileCenter :: BoardPosition -> (Float, Float)
getTileCenter (x, y) = let
  xf = fromIntegral x
  yf = fromIntegral (boardSizeY - y - 1)
  in
   ((tileSizeXf * 0.5) + xf * tileSizeXf, (tileSizeYf * 0.5) + yf * tileSizeYf)

getBorderCenter :: BoardPosition -> Direction -> (Float, Float)
getBorderCenter pos dir = let
  dir2Offset :: Direction -> (Float, Float)
  dir2Offset Up = (0, tileSizeYf * 0.5)
  dir2Offset Down = (0, -tileSizeYf * 0.5)
  dir2Offset Left = (-tileSizeXf * 0.5, 0)
  dir2Offset Right = (tileSizeXf * 0.5, 0)
  (ox, oy) = dir2Offset dir
  (xf, yf) = getTileCenter pos
  in
   (xf + ox, oy + yf)
