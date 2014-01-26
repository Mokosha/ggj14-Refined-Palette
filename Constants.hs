module Constants (
  screenSizeX,
  screenSizeXf,
  halfScreenX,
  halfScreenXf,
  screenSizeY,
  screenSizeYf,
  halfScreenY,
  halfScreenYf,
  boardSizeX,
  boardSizeY,
  tileSizeX,
  tileSizeY,
  tileSizeXf,
  tileSizeYf
) where

--------------------------------------------------------------------------------

screenSizeX :: Int
screenSizeX = 500

screenSizeY :: Int
screenSizeY = 500

halfScreenX :: Int
halfScreenX = screenSizeX `div` 2

halfScreenY :: Int
halfScreenY = screenSizeY `div` 2

halfScreenXf :: Float
halfScreenXf = fromIntegral halfScreenX

halfScreenYf :: Float
halfScreenYf = fromIntegral halfScreenY

screenSizeXf :: Float
screenSizeXf = fromIntegral screenSizeX

screenSizeYf :: Float
screenSizeYf = fromIntegral screenSizeY

boardSizeX :: Int
boardSizeX = 10

boardSizeY :: Int
boardSizeY = 10

tileSizeX :: Int
tileSizeX = screenSizeX `div` boardSizeX

tileSizeY :: Int
tileSizeY = screenSizeY `div` boardSizeY

tileSizeXf :: Float
tileSizeXf = fromIntegral tileSizeX 

tileSizeYf :: Float
tileSizeYf = fromIntegral tileSizeY 

-- The time it takes to move the player from one block to
-- another
-- playerMovementTime :: Float
-- playerMovementTime = 0.5

