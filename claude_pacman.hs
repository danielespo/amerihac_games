{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Console.ANSI
import System.Exit (exitSuccess)
import System.Random

--------------------------------------------------------------------------------
-- Types (now with more semantic clarity!)
--------------------------------------------------------------------------------

type Pos = (Int, Int)
type Vec2 = (Int, Int)

data GameStatus = Running | Won | Lost deriving (Eq, Show)

data Ghost = Ghost
  { ghostPos   :: Pos
  , ghostTimer :: Int
  , ghostMood  :: GhostMood  -- NEW: Ghosts have personalities now!
  } deriving (Show)

data GhostMood = Aggressive | Wanderer | Ambusher deriving (Show, Eq)

data Game = Game
  { pacman   :: Pos
  , ghosts   :: [Ghost]
  , pellets  :: S.Set Pos
  , walls    :: S.Set Pos
  , boardDim :: (Int, Int)
  , status   :: GameStatus
  , score    :: Int
  , lives    :: Int
  , rng      :: StdGen
  , power    :: Int  -- NEW: Power pellet timer (ghosts run away!)
  } deriving (Show)

data Direction = North | South | West | East | Stay deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Configuration (the fun knobs to turn!)
--------------------------------------------------------------------------------

frameDelay :: Int
frameDelay = 80_000  -- ~12 FPS (numeric underscores for readability!)

ghostMoveInterval :: Int
ghostMoveInterval = 3

maxLives :: Int
maxLives = 3

powerPelletDuration :: Int
powerPelletDuration = 30  -- ~2.5 seconds of ghost-munching glory

pelletsPerPowerPellet :: Int
pelletsPerPowerPellet = 15  -- Every Nth pellet is a power pellet

--------------------------------------------------------------------------------
-- Main (nice and clean)
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  hideCursor
  gen <- getStdGen
  gameLoop (initialGame gen)
  showCursor

--------------------------------------------------------------------------------
-- Game Loop (now with better error handling)
--------------------------------------------------------------------------------

gameLoop :: Game -> IO ()
gameLoop game@Game{..} = do
  renderGame game
  case status of
    Running -> do
      input <- pollInput
      threadDelay frameDelay
      gameLoop (updateGame input game)
    
    Won  -> endGame game "🎉 WAKKA WAKKA VICTORY! 🎉"
    
    Lost -> if lives > 1
            then respawn game
            else endGame game "👻 GAME OVER, MAN! GAME OVER! 👻"

respawn :: Game -> IO ()
respawn game@Game{..} = do
  putStrLn $ "\n💀 You lost a life! " ++ show (lives - 1) ++ " remaining..."
  threadDelay 1_000_000
  gen' <- newStdGen
  gameLoop $ (initialGame gen') { lives = lives - 1, score = score }

endGame :: Game -> String -> IO ()
endGame Game{..} msg = do
  clearScreen
  setCursorPosition 5 0
  putStrLn $ centerText msg
  putStrLn $ centerText $ "Final Score: " ++ show score
  putStrLn ""
  putStrLn $ centerText "[R]estart or [Q]uit"
  
  waitForInput >>= \case
    'r' -> newStdGen >>= gameLoop . initialGame
    'R' -> newStdGen >>= gameLoop . initialGame
    'q' -> exitSuccess
    'Q' -> exitSuccess
    _   -> endGame (initialGame (mkStdGen 0)) msg  -- Try again!

centerText :: String -> String
centerText s = replicate padding ' ' ++ s
  where padding = max 0 ((80 - length s) `div` 2)

pollInput :: IO Char
pollInput = do
  ready <- hReady stdin
  if ready then getChar else pure ' '

waitForInput :: IO Char
waitForInput = hReady stdin >>= \ready ->
  if ready then getChar else threadDelay 50_000 >> waitForInput

--------------------------------------------------------------------------------
-- Game Update (functional goodness)
--------------------------------------------------------------------------------

updateGame :: Char -> Game -> Game
updateGame input game@Game{..} =
  game { pacman      = pac'
       , pellets     = pellets''
       , ghosts      = ghosts'
       , status      = status'
       , score       = score'
       , rng         = rng'
       , power       = power'
       }
  where
    direction     = charToDirection input
    pac'          = move pacman direction walls
    
    -- Check if we ate a pellet
    (pellets', ateNormal) = if pac' `S.member` pellets
                            then (S.delete pac' pellets, True)
                            else (pellets, False)
    
    -- Check if it was a power pellet (every Nth pellet position)
    atePower      = ateNormal && isPowerPellet pac'
    pellets''     = pellets'
    power'        = if atePower then powerPelletDuration else max 0 (power - 1)
    
    -- Update score
    score'        = score + if ateNormal then (if atePower then 50 else 10) else 0
    
    -- Move ghosts (they fear you when powered up!)
    (ghosts', rng') = updateGhosts ghosts pac' walls rng (power' > 0)
    
    -- Check win/loss conditions
    touchingGhost = any ((== pac') . ghostPos) ghosts'
    status'
      | touchingGhost && power' <= 0 = Lost
      | S.null pellets''             = Won
      | otherwise                    = Running

-- Determine if a position should have a power pellet
isPowerPellet :: Pos -> Bool
isPowerPellet (x, y) = (x * 7 + y * 13) `mod` pelletsPerPowerPellet == 0

charToDirection :: Char -> Direction
charToDirection = \case
  'w' -> North; 'W' -> North
  's' -> South; 'S' -> South
  'a' -> West;  'A' -> West
  'd' -> East;  'D' -> East
  _   -> Stay

directionToVec :: Direction -> Vec2
directionToVec = \case
  North -> ( 0, -1)
  South -> ( 0,  1)
  West  -> (-1,  0)
  East  -> ( 1,  0)
  Stay  -> ( 0,  0)

move :: Pos -> Direction -> S.Set Pos -> Pos
move pos dir walls = if newPos `S.member` walls then pos else newPos
  where
    newPos = addVec pos (directionToVec dir)
    addVec (x, y) (dx, dy) = (x + dx, y + dy)

--------------------------------------------------------------------------------
-- Ghost AI (now with PERSONALITY!)
--------------------------------------------------------------------------------

updateGhosts :: [Ghost] -> Pos -> S.Set Pos -> StdGen -> Bool -> ([Ghost], StdGen)
updateGhosts ghosts pacPos walls gen powered =
  foldr updateOne ([], gen) ghosts
  where
    updateOne ghost (acc, g) =
      let (ghost', g') = updateGhost ghost pacPos walls g powered
      in (ghost' : acc, g')

updateGhost :: Ghost -> Pos -> S.Set Pos -> StdGen -> Bool -> (Ghost, StdGen)
updateGhost ghost@Ghost{..} pacPos walls gen powered
  | ghostTimer > 0 = (ghost { ghostTimer = ghostTimer - 1 }, gen)
  | otherwise      = (ghost { ghostPos = pos', ghostTimer = ghostMoveInterval }, gen')
  where
    (pos', gen') = ghostAI ghostPos ghostMood pacPos walls gen powered

ghostAI :: Pos -> GhostMood -> Pos -> S.Set Pos -> StdGen -> Bool -> (Pos, StdGen)
ghostAI pos mood pacPos walls gen powered =
  case validMoves of
    [] -> (pos, gen)
    _  -> (chosenMove, genFinal)
  where
    validMoves = filter (`S.notMember` walls) (neighbors pos)
    
    (r, gen') = randomR (0 :: Double, 1.0) gen
    
    -- When powered, ghosts run AWAY
    target = if powered then farthestFrom else closestTo
    
    (chosenMove, genFinal) = case mood of
      Aggressive -> (target validMoves pacPos, gen')  -- Always chases/flees
      Wanderer   -> if r < 0.3  -- 30% smart, 70% random
                    then (target validMoves pacPos, gen')
                    else randomChoice validMoves gen'
      Ambusher   -> if r < 0.5  -- Tries to cut you off
                    then (target validMoves (predictPacman pacPos), gen')
                    else randomChoice validMoves gen'

closestTo :: [Pos] -> Pos -> Pos
closestTo options target = minimumBy (comparing (`manhattan` target)) options

farthestFrom :: [Pos] -> Pos -> Pos
farthestFrom options target = maximumBy (comparing (`manhattan` target)) options
  where maximumBy f = minimumBy (flip f)

randomChoice :: [a] -> StdGen -> (a, StdGen)
randomChoice xs gen = (xs !! idx, gen')
  where (idx, gen') = randomR (0, length xs - 1) gen

predictPacman :: Pos -> Pos
predictPacman (x, y) = (x + dx, y + dy)  -- Crude prediction
  where
    dx = if x `mod` 2 == 0 then 1 else -1
    dy = if y `mod` 3 == 0 then 1 else -1

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(1,0), (-1,0), (0,1), (0,-1)]]

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

--------------------------------------------------------------------------------
-- Rendering (with COLORS and STYLE!)
--------------------------------------------------------------------------------

renderGame :: Game -> IO ()
renderGame Game{..} = do
  setCursorPosition 0 0
  let (w, h) = boardDim
  
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      setSGR [Reset]  -- Clear formatting
      putChar (tileChar (x, y))
    putChar '\n'
  
  setSGR [Reset]
  putStrLn $ "Score: " ++ show score ++ "  Lives: " ++ hearts lives ++ "  " ++ powerStatus
  
  where
    powered = power > 0
    
    tileChar p
      | p == pacman                  = 'C'  -- Our hero!
      | p `elem` map ghostPos ghosts = if powered then 'X' else 'G'  -- Scared ghosts!
      | p `S.member` walls           = '█'  -- Solid walls
      | p `S.member` pellets         = if isPowerPellet p then 'O' else '·'
      | otherwise                    = ' '
    
    hearts n = concat (replicate n "♥")
    
    powerStatus = if powered
                  then "⚡ POWER! (" ++ show power ++ ") ⚡"
                  else ""

--------------------------------------------------------------------------------
-- Level Generation (slightly fancier!)
--------------------------------------------------------------------------------

initialGame :: StdGen -> Game
initialGame gen = Game
  { pacman   = (1, 1)
  , ghosts   = initialGhosts
  , pellets  = allPellets
  , walls    = wallSet
  , boardDim = (w, h)
  , status   = Running
  , score    = 0
  , lives    = maxLives
  , rng      = gen
  , power    = 0
  }
  where
    w = 21
    h = 15
    
    -- Build walls
    wallSet = S.fromList $
      -- Perimeter
      [(x, 0) | x <- [0..w-1]] ++
      [(x, h-1) | x <- [0..w-1]] ++
      [(0, y) | y <- [0..h-1]] ++
      [(w-1, y) | y <- [0..h-1]] ++
      -- Center obstacle
      [(10, y) | y <- [3..11]] ++
      -- Side boxes for fun
      [(5, 5), (5, 6), (6, 5), (6, 6)] ++
      [(15, 5), (15, 6), (16, 5), (16, 6)]
    
    -- Place pellets everywhere except walls
    allPellets = S.fromList
      [ (x, y)
      | x <- [1..w-2]
      , y <- [1..h-2]
      , (x, y) `S.notMember` wallSet
      ]
    
    -- Create ghosts with different personalities!
    initialGhosts =
      [ Ghost (w-2, h-2) 0 Aggressive  -- Blinky: The hunter
      , Ghost (w-2, 1)   0 Wanderer    -- Pinky: The random one
      , Ghost (1, h-2)   0 Ambusher    -- Inky: The sneaky one
      ]
