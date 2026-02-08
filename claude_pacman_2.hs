{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (minimumBy, find)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Map as M
import System.Console.ANSI
import System.Exit (exitSuccess)
import System.Random

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Direction = DUp | DDown | DLeft | DRight | DNone deriving (Eq, Show)

data GhostMode = Chase | Scatter | Frightened deriving (Eq, Show)

data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

data Ghost = Ghost
  { ghostType :: GhostType
  , ghostPos  :: Pos
  , ghostDir  :: Direction
  , ghostMode :: GhostMode
  } deriving (Show)

data Game = Game
  { pacmanPos     :: Pos
  , pacmanDir     :: Direction
  , nextDir       :: Direction
  , ghosts        :: [Ghost]
  , pellets       :: S.Set Pos
  , powerPellets  :: S.Set Pos
  , walls         :: S.Set Pos
  , score         :: Int
  , lives         :: Int
  , level         :: Int
  , frightenTimer :: Int
  , modeTimer     :: Int
  , currentMode   :: GhostMode
  , eatGhostCombo :: Int
  , rng           :: StdGen
  } deriving (Show)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

frameDelay :: Int
frameDelay = 100000  -- 100ms per frame

frightenedDuration :: Int
frightenedDuration = 60  -- 6 seconds at 10 fps

pacmanStartPos :: Pos
pacmanStartPos = (14, 23)

blinkyStartPos :: Pos
blinkyStartPos = (14, 11)

pinkyStartPos :: Pos
pinkyStartPos = (14, 14)

inkyStartPos :: Pos
inkyStartPos = (12, 14)

clydeStartPos :: Pos
clydeStartPos = (16, 14)

-- Mode timing (in frames): Scatter, Chase, Scatter, Chase, etc.
modeTiming :: [Int]
modeTiming = [70, 200, 70, 200, 50, 200, 50, maxBound]

--------------------------------------------------------------------------------
-- Original Pac-Man Maze
--------------------------------------------------------------------------------

mazeWidth :: Int
mazeWidth = 28

mazeHeight :: Int
mazeHeight = 31

-- Classic Pac-Man maze layout
mazeLayout :: [String]
mazeLayout =
  [ "############################"
  , "#............##............#"
  , "#.####.#####.##.#####.####.#"
  , "#O####.#####.##.#####.####O#"
  , "#.####.#####.##.#####.####.#"
  , "#..........................#"
  , "#.####.##.########.##.####.#"
  , "#.####.##.########.##.####.#"
  , "#......##....##....##......#"
  , "######.##### ## #####.######"
  , "     #.##### ## #####.#     "
  , "     #.##          ##.#     "
  , "     #.## ###--### ##.#     "
  , "######.## #      # ##.######"
  , "      .   #      #   .      "
  , "######.## #      # ##.######"
  , "     #.## ######## ##.#     "
  , "     #.##          ##.#     "
  , "     #.## ######## ##.#     "
  , "######.## ######## ##.######"
  , "#............##............#"
  , "#.####.#####.##.#####.####.#"
  , "#.####.#####.##.#####.####.#"
  , "#O..##.......  .......##..O#"
  , "###.##.##.########.##.##.###"
  , "###.##.##.########.##.##.###"
  , "#......##....##....##......#"
  , "#.##########.##.##########.#"
  , "#.##########.##.##########.#"
  , "#..........................#"
  , "############################"
  ]

parseMaze :: [String] -> (S.Set Pos, S.Set Pos, S.Set Pos)
parseMaze rows = foldl processRow (S.empty, S.empty, S.empty) indexedRows
  where
    indexedRows = zip [0..] rows
    processRow (ws, ps, pps) (y, row) = 
      foldl (processCell y) (ws, ps, pps) (zip [0..] row)
    processCell y (ws, ps, pps) (x, c) = case c of
      '#' -> (S.insert (x, y) ws, ps, pps)
      '.' -> (ws, S.insert (x, y) ps, pps)
      'O' -> (ws, ps, S.insert (x, y) pps)
      _   -> (ws, ps, pps)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

initialGame :: StdGen -> Game
initialGame gen = Game
  { pacmanPos     = pacmanStartPos
  , pacmanDir     = DLeft
  , nextDir       = DLeft
  , ghosts        = initialGhosts
  , pellets       = ps
  , powerPellets  = pps
  , walls         = ws
  , score         = 0
  , lives         = 3
  , level         = 1
  , frightenTimer = 0
  , modeTimer     = modeTiming !! 0
  , currentMode   = Scatter
  , eatGhostCombo = 0
  , rng           = gen
  }
  where
    (ws, ps, pps) = parseMaze mazeLayout
    initialGhosts =
      [ Ghost Blinky blinkyStartPos DLeft Scatter
      , Ghost Pinky pinkyStartPos DUp Scatter
      , Ghost Inky inkyStartPos DUp Scatter
      , Ghost Clyde clydeStartPos DUp Scatter
      ]

--------------------------------------------------------------------------------
-- Main Loop
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  clearScreen
  hideCursor
  gen <- getStdGen
  gameLoop (initialGame gen)
  showCursor

gameLoop :: Game -> IO ()
gameLoop game = do
  render game
  
  -- Check game over
  if lives game <= 0
    then do
      setCursorPosition 15 8
      putStrLn "GAME OVER"
      threadDelay 3000000
      showCursor
      exitSuccess
    else pure ()
  
  -- Check level complete
  game' <- if S.null (pellets game) && S.null (powerPellets game)
    then do
      threadDelay 2000000
      gen' <- newStdGen
      return $ nextLevel (initialGame gen') (score game) (lives game) (level game + 1)
    else return game
  
  -- Get input
  input <- pollInput
  threadDelay frameDelay
  
  -- Update and continue
  gameLoop (updateGame input game')

nextLevel :: Game -> Int -> Int -> Int -> Game
nextLevel game s l lvl = game { score = s, lives = l, level = lvl }

pollInput :: IO Char
pollInput = do
  ready <- hReady stdin
  if ready then getChar else return ' '

--------------------------------------------------------------------------------
-- Update Logic
--------------------------------------------------------------------------------

updateGame :: Char -> Game -> Game
updateGame input game = game8
  where
    -- Update next direction from input
    game1 = updateNextDir input game
    
    -- Update mode timer and ghost modes
    game2 = updateModes game1
    
    -- Move Pac-Man
    game3 = movePacman game2
    
    -- Check pellet collection
    game4 = collectPellet game3
    
    -- Move ghosts
    game5 = moveGhosts game4
    
    -- Check collisions
    game6 = checkCollisions game5
    
    -- Update frightened timer
    game7 = updateFrightenTimer game6
    
    -- Reset eat combo if not frightened
    game8 = if frightenTimer game7 == 0
            then game7 { eatGhostCombo = 0 }
            else game7

updateNextDir :: Char -> Game -> Game
updateNextDir c game = game { nextDir = newDir }
  where
    newDir = case c of
      'w' -> DUp
      's' -> DDown
      'a' -> DLeft
      'd' -> DRight
      'q' -> error "Quit"
      _   -> nextDir game

updateModes :: Game -> Game
updateModes game@Game{..}
  | frightenTimer > 0 = game  -- Don't change modes when frightened
  | modeTimer <= 0 = game { currentMode = newMode, modeTimer = newTimer, ghosts = updatedGhosts }
  | otherwise = game { modeTimer = modeTimer - 1 }
  where
    modeIndex = getModeIndex currentMode 0 modeTiming
    nextModeIndex = modeIndex + 1
    newMode = if even nextModeIndex then Scatter else Chase
    newTimer = if nextModeIndex < length modeTiming
               then modeTiming !! nextModeIndex
               else maxBound
    updatedGhosts = map (\g -> g { ghostMode = newMode }) ghosts
    
    getModeIndex _ idx [] = idx
    getModeIndex mode idx (t:ts)
      | idx == 0 && mode == Scatter = 0
      | idx == 1 && mode == Chase = 1
      | otherwise = getModeIndex mode (idx + 1) ts

movePacman :: Game -> Game
movePacman game@Game{..} = game { pacmanPos = newPos, pacmanDir = newDir }
  where
    -- Try to turn to nextDir first
    tryPos = moveInDir pacmanPos nextDir
    canTurn = not (tryPos `S.member` walls)
    
    -- If can't turn, continue in current direction
    continuePos = moveInDir pacmanPos pacmanDir
    canContinue = not (continuePos `S.member` walls)
    
    (newPos, newDir) = if canTurn
                       then (tryPos, nextDir)
                       else if canContinue
                            then (continuePos, pacmanDir)
                            else (pacmanPos, pacmanDir)

collectPellet :: Game -> Game
collectPellet game@Game{..}
  | pacmanPos `S.member` pellets = 
      game { pellets = S.delete pacmanPos pellets, score = score + 10 }
  | pacmanPos `S.member` powerPellets =
      game { powerPellets = S.delete pacmanPos powerPellets
           , score = score + 50
           , frightenTimer = frightenedDuration
           , ghosts = map frightenGhost ghosts
           , eatGhostCombo = 0
           }
  | otherwise = game
  where
    frightenGhost g = g { ghostMode = Frightened, ghostDir = reverseDir (ghostDir g) }

moveGhosts :: Game -> Game
moveGhosts game@Game{..} = game { ghosts = map (moveGhost game) ghosts }

moveGhost :: Game -> Ghost -> Ghost
moveGhost game ghost@Ghost{..} = ghost { ghostPos = newPos, ghostDir = newDir }
  where
    target = getTargetPos game ghost
    possibleDirs = getPossibleDirections ghostPos (walls game) ghostDir
    
    -- In Frightened mode, choose random direction
    (newDir, newPos) = if ghostMode == Frightened
                       then pickRandomDir possibleDirs
                       else pickBestDir possibleDirs target
    
    pickBestDir dirs tgt = 
      let scored = map (\d -> (d, distance (moveInDir ghostPos d) tgt)) dirs
          best = minimumBy (comparing snd) scored
      in (fst best, moveInDir ghostPos (fst best))
    
    pickRandomDir dirs = 
      let (idx, _) = randomR (0, length dirs - 1) (rng game)
          dir = dirs !! idx
      in (dir, moveInDir ghostPos dir)

getTargetPos :: Game -> Ghost -> Pos
getTargetPos Game{..} Ghost{..} = case ghostMode of
  Frightened -> pacmanPos  -- Dummy, not used
  Scatter -> scatterTarget ghostType
  Chase -> chaseTarget ghostType pacmanPos pacmanDir (find isBlinky ghosts)
  where
    isBlinky (Ghost Blinky _ _ _) = True
    isBlinky _ = False

-- Original Pac-Man scatter corners
scatterTarget :: GhostType -> Pos
scatterTarget Blinky = (25, 0)   -- Top right
scatterTarget Pinky  = (2, 0)    -- Top left
scatterTarget Inky   = (27, 30)  -- Bottom right
scatterTarget Clyde  = (0, 30)   -- Bottom left

-- Original Pac-Man ghost AI targeting
chaseTarget :: GhostType -> Pos -> Direction -> Maybe Ghost -> Pos
chaseTarget Blinky pacPos _ _ = pacPos  -- Directly targets Pac-Man
chaseTarget Pinky pacPos dir _ = moveInDirN (moveInDirN (moveInDirN (moveInDirN pacPos dir) dir) dir) dir  -- 4 tiles ahead
chaseTarget Inky pacPos dir mbBlinky = 
  let twoAhead = moveInDirN (moveInDirN pacPos dir) dir
      blinkyPos = maybe pacPos ghostPos mbBlinky
      (dx, dy) = (fst twoAhead - fst blinkyPos, snd twoAhead - snd blinkyPos)
  in (fst twoAhead + dx, snd twoAhead + dy)
chaseTarget Clyde pacPos _ _ = pacPos  -- Simplified Clyde behavior

getPossibleDirections :: Pos -> S.Set Pos -> Direction -> [Direction]
getPossibleDirections pos walls currentDir = 
  filter (\d -> d /= reverseDir currentDir && canMove d) [DUp, DDown, DLeft, DRight]
  where
    canMove d = not (moveInDir pos d `S.member` walls)

checkCollisions :: Game -> Game
checkCollisions game@Game{..} = 
  case find (\g -> ghostPos g == pacmanPos) ghosts of
    Nothing -> game
    Just ghost -> if frightenTimer > 0
                  then eatGhost game ghost
                  else loseLife game

eatGhost :: Game -> Ghost -> Game
eatGhost game@Game{..} ghost = 
  game { score = score + points
       , eatGhostCombo = eatGhostCombo + 1
       , ghosts = map resetIfEaten ghosts
       }
  where
    points = 200 * (2 ^ eatGhostCombo)
    resetIfEaten g = if ghostPos g == pacmanPos
                     then g { ghostPos = getGhostStart (ghostType g), ghostMode = currentMode }
                     else g

getGhostStart :: GhostType -> Pos
getGhostStart Blinky = blinkyStartPos
getGhostStart Pinky = pinkyStartPos
getGhostStart Inky = inkyStartPos
getGhostStart Clyde = clydeStartPos

loseLife :: Game -> Game
loseLife game@Game{..} = game { lives = lives - 1, pacmanPos = pacmanStartPos, pacmanDir = DLeft, nextDir = DLeft }

updateFrightenTimer :: Game -> Game
updateFrightenTimer game@Game{..}
  | frightenTimer > 0 = 
      let newTimer = frightenTimer - 1
          newGhosts = if newTimer == 0
                      then map (\g -> g { ghostMode = currentMode }) ghosts
                      else ghosts
      in game { frightenTimer = newTimer, ghosts = newGhosts }
  | otherwise = game

--------------------------------------------------------------------------------
-- Movement Helpers
--------------------------------------------------------------------------------

moveInDir :: Pos -> Direction -> Pos
moveInDir (x, y) DUp    = (x, y - 1)
moveInDir (x, y) DDown  = (x, y + 1)
moveInDir (x, y) DLeft  = (x - 1, y)
moveInDir (x, y) DRight = (x + 1, y)
moveInDir pos   DNone   = pos

moveInDirN :: Pos -> Direction -> Pos
moveInDirN = moveInDir

reverseDir :: Direction -> Direction
reverseDir DUp = DDown
reverseDir DDown = DUp
reverseDir DLeft = DRight
reverseDir DRight = DLeft
reverseDir DNone = DNone

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

render :: Game -> IO ()
render game@Game{..} = do
  setCursorPosition 0 0
  
  forM_ [0..mazeHeight-1] $ \y -> do
    forM_ [0..mazeWidth-1] $ \x -> do
      putChar (getChar (x, y))
    putChar '\n'
  
  putStrLn $ "Score: " ++ show score ++ "  Lives: " ++ replicate lives '♥' ++ "  Level: " ++ show level
  
  where
    getChar pos
      | pos == pacmanPos = getPacmanChar pacmanDir
      | Just ghost <- find (\g -> ghostPos g == pos) ghosts = getGhostChar ghost
      | pos `S.member` walls = '█'
      | pos `S.member` pellets = '·'
      | pos `S.member` powerPellets = 'O'
      | otherwise = ' '
    
    getPacmanChar DLeft  = 'C'
    getPacmanChar DRight = 'C'
    getPacmanChar DUp    = 'C'
    getPacmanChar DDown  = 'C'
    getPacmanChar DNone  = 'C'
    
    getGhostChar Ghost{..} = if frightenTimer > 0
                             then if frightenTimer < 20 then 'X' else 'x'
                             else case ghostType of
                               Blinky -> 'B'
                               Pinky  -> 'P'
                               Inky   -> 'I'
                               Clyde  -> 'C'
