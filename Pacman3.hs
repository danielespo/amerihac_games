{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Timeout
import Control.Concurrent
import System.Random
import Data.List

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Status = Running | Won | Lost
  deriving (Eq)

data Game = Game
  { pacman  :: Pos
  , ghost   :: Pos
  , pellets :: [Pos]
  , walls   :: [Pos]
  , width   :: Int
  , height  :: Int
  , status  :: Status
  , ghostTimer :: Int
}

--------------------------------------------------------------------------------
-- Terminal Setup
--------------------------------------------------------------------------------

setupTerminal :: IO ()
setupTerminal = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  putStr "\ESC[2J"   -- Clear screen once at start

--------------------------------------------------------------------------------
-- Entry
--------------------------------------------------------------------------------

main :: IO ()
main = do
  setupTerminal
  gameLoop initialGame

--------------------------------------------------------------------------------
-- Game Loop
--------------------------------------------------------------------------------

frameDelay :: Int
frameDelay = 30000  -- ~33 FPS

endDelay :: Int
endDelay = 2000000

gameLoop :: Game -> IO ()
gameLoop = loop
  where
    loop g = do
      draw g
      case status g of
        Running -> do
          c <- pollInput
          threadDelay frameDelay
          loop (step g c)
        Won  -> finish "YOU WIN"
        Lost -> finish "GAME OVER"

    finish msg = do
      putStr "\ESC[H" -- Move cursor to top-left
      putStrLn msg
      threadDelay endDelay

pollInput :: IO Char
pollInput = do
  ready <- hReady stdin
  if ready then getChar else return ' '

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

ghostMoveInterval :: Int

ghostMoveInterval = 3 -- ghost gets a move every N frames

step :: Game -> Char -> Game
step g@Game{..} c =
  let p'  = tryMove pacman (dir c) walls
      ps' = delete p' pellets
      -- limit ghost movement so game easier
      (gh', timer') =
        if ghostTimer <= 0
           then (ghostStep p' ghost walls, ghostMoveInterval)
           else (ghost, ghostTimer - 1)
      st'
        | p' == ghost = Lost
        | null ps'    = Won
        | otherwise   = Running

  in g { pacman = p', pellets = ps', ghost = gh', status = st', ghostTimer = timer' }

dir :: Char -> Pos
dir 'w' = (0,-1)
dir 's' = (0, 1)
dir 'a' = (-1,0)
dir 'd' = (1, 0)
dir _   = (0, 0)

tryMove :: Pos -> Pos -> [Pos] -> Pos
tryMove (x,y) (dx,dy) walls =
  let p = (x+dx,y+dy)
  in if p `elem` walls then (x,y) else p

ghostStep :: Pos -> Pos -> [Pos] -> Pos
ghostStep pac g walls =
  minimumBy closer candidates
  where
    closer a b =
      compare (dist pac a) (dist pac b)

    candidates =
      filter (`notElem` walls)
      [ (fst g + dx, snd g + dy)
      | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]
      ]

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) =
  abs (x1-x2) + abs (y1-y2)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

draw :: Game -> IO ()
draw Game{..} = do
  putStr "\ESC[H"  -- Move cursor home without clearing entire screen
  mapM_ putStrLn
    [ [ tile (x,y) | x <- [0..width-1] ]
    | y <- [0..height-1]
    ]
  where
    tile p
      | p == pacman       = 'C'
      | p == ghost        = 'G'
      | p `elem` walls    = '#'
      | p `elem` pellets  = '.'
      | otherwise         = ' '

--------------------------------------------------------------------------------
-- Level Definition
--------------------------------------------------------------------------------

initialGame :: Game
initialGame =
  let w = 21
      h = 15
      wallList =
        [ (x,0) | x <- [0..w-1] ] ++
        [ (x,h-1) | x <- [0..w-1] ] ++
        [ (0,y) | y <- [0..h-1] ] ++
        [ (w-1,y) | y <- [0..h-1] ] ++
        [ (10,y) | y <- [3..11] ]
      pelletList =
        [ (x,y)
        | x <- [1..w-2], y <- [1..h-2]
        , (x,y) `notElem` wallList
        ]
  in Game
      { pacman  = (1,1)
      , ghost   = (w-2,h-2)
      , pellets = pelletList
      , walls   = wallList
      , width   = w
      , height  = h
      , status  = Running
      , ghostTimer = 0 -- can move on first frame
      }

