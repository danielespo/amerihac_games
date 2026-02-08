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
  }

--------------------------------------------------------------------------------
-- Entry
--------------------------------------------------------------------------------
setupTerminal :: IO ()
setupTerminal = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering


main :: IO ()
main = do
  setupTerminal
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  gameLoop initialGame

--------------------------------------------------------------------------------
-- Game Loop (as requested, refactored)
--------------------------------------------------------------------------------

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
      clearScreen
      putStrLn msg
      threadDelay endDelay

    pollInput :: IO Char
    pollInput = do
      ready <- hReady stdin
      if ready then getChar else return ' '

    frameDelay = 100000   -- 10 FPS
    endDelay   = 2000000

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

step :: Game -> Char -> Game
step g@Game{..} c =
  let p'  = tryMove pacman (dir c) walls
      ps' = delete p' pellets
      st'
        | p' == ghost = Lost
        | null ps'    = Won
        | otherwise   = Running
      gh' = ghostStep p' ghost walls
  in g { pacman = p', pellets = ps', ghost = gh', status = st' }

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
  clearScreen
  mapM_ putStrLn
    [ [ tile (x,y) | x <- [0..width-1] ]
    | y <- [0..height-1]
    ]
  where
    tile p
      | p == pacman        = 'C'
      | p == ghost         = 'G'
      | p `elem` walls    = '#'
      | p `elem` pellets  = '.'
      | otherwise          = ' '


clearScreen :: IO ()
clearScreen = putStr "\ESC[H"  -- Move cursor home only, no flicker

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
      }

