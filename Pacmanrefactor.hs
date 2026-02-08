{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import Control.Concurrent
import Data.List
import Data.Maybe
import System.Random (randomRIO)
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Status = Running | Won | Lost deriving (Eq, Show)

data Game = Game
  { pacman      :: Pos
  , ghosts      :: [Pos]
  , pellets     :: S.Set Pos
  , walls       :: S.Set Pos
  , width       :: Int
  , height      :: Int
  , status      :: Status
  , ghostTimers :: [Int]
  , score       :: Int
  , frameCount  :: Int
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
-- Main Loop
--------------------------------------------------------------------------------

frameDelay, endDelay :: Int
frameDelay = 100000   -- ~10 FPS
endDelay   = 2000000

main :: IO ()
main = setupTerminal >> gameLoop initialGame

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
        Won  -> finish "YOU WIN" g
        Lost -> finish "GAME OVER" g

    finish msg g = do
      putStr "\ESC[H"
      putStrLn $ msg ++ " Score: " ++ show (score g)
      putStrLn "Press r to restart or any other key to exit."
      c <- getChar
      if c == 'r' then gameLoop initialGame else return ()

pollInput :: IO Char
pollInput = do
  ready <- hReady stdin
  if ready then getChar else return ' '

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

ghostMoveInterval :: Int
ghostMoveInterval = 3

step :: Game -> Char -> Game
step g@Game{..} input =
  let pac'     = tryMove pacman (direction input) walls
      pellets' = S.delete pac' pellets
      score'   = score + if pac' `S.member` pellets then 10 else 0

      (ghosts', timers') = unzip $ zipWith moveGhost ghosts ghostTimers
      moveGhost gPos t
        | t <= 0    = let gNew = ghostStep pac' gPos walls ghosts in (gNew, ghostMoveInterval)
        | otherwise = (gPos, t - 1)

      status'
        | pac' `elem` ghosts' = Lost
        | S.null pellets'     = Won
        | otherwise           = Running

  in g { pacman = pac', pellets = pellets', ghosts = ghosts'
       , ghostTimers = timers', status = status', score = score'
       , frameCount = frameCount + 1 }

direction :: Char -> Pos
direction 'w' = (0,-1)
direction 's' = (0, 1)
direction 'a' = (-1,0)
direction 'd' = (1, 0)
direction _   = (0, 0)

tryMove :: Pos -> Pos -> S.Set Pos -> Pos
tryMove (x,y) (dx,dy) walls =
  let p = (x+dx, y+dy)
  in if p `S.member` walls then (x,y) else p

-- Ghost AI: moves toward Pacman if possible
ghostStep :: Pos -> Pos -> S.Set Pos -> [Pos] -> Pos
ghostStep pac g walls others =
  let candidates = filter (`notElem` others) $
                   filter (`S.notMember` walls) $
                   neighbors g
  in if null candidates then g
     else minimumBy (\a b -> compare (manhattan a pac) (manhattan b pac)) candidates

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x+dx,y+dy) | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]

manhattan :: Pos -> Pos -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

draw :: Game -> IO ()
draw Game{..} = do
  putStr "\ESC[H"
  mapM_ putStrLn [[tile (x,y) | x <- [0..width-1]] | y <- [0..height-1]]
  putStrLn $ "Score: " ++ show score
  where
    tile p
      | p == pacman       = 'C'
      | p `elem` ghosts   = 'G'
      | p `S.member` walls= '#'
      | p `S.member` pellets = '.'
      | otherwise         = ' '

--------------------------------------------------------------------------------
-- Level Definition
--------------------------------------------------------------------------------

initialGame :: Game
initialGame =
  let w = 21
      h = 15
      wallList = S.fromList $
                 [(x,0) | x <- [0..w-1]] ++
                 [(x,h-1) | x <- [0..w-1]] ++
                 [(0,y) | y <- [0..h-1]] ++
                 [(w-1,y) | y <- [0..h-1]] ++
                 [(10,y) | y <- [3..11]]
      pelletList = S.fromList [(x,y) | x <- [1..w-2], y <- [1..h-2], (x,y) `S.notMember` wallList]
      ghostList = [(w-2,h-2),(w-2,1)]
  in Game { pacman      = (1,1)
          , ghosts      = ghostList
          , pellets     = pelletList
          , walls       = wallList
          , width       = w
          , height      = h
          , status      = Running
          , ghostTimers = replicate (length ghostList) 0
          , score       = 0
          , frameCount  = 0
          }

