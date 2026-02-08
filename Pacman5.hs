{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import Control.Concurrent
import System.Random
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, head, (!!))
import System.Random (randomRIO)
import Data.List (sortOn)
import Data.List.NonEmpty (nonEmpty, toList)


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Status = Running | Won | Lost
  deriving (Eq)

data Game = Game
  { pacman      :: Pos
  , ghosts      :: [Pos]
  , pellets     :: [Pos]
  , walls       :: [Pos]
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
frameDelay = 100000  -- ~10 FPS

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
          g' <- step g c
          threadDelay frameDelay
          loop g'
        Won  -> finish "YOU WIN"
        Lost -> finish "GAME OVER"

    finish msg = do
      putStr "\ESC[H"
      putStrLn $ msg ++ " Score: " ++ show (score initialGame)
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
ghostMoveInterval = 3 -- ghost moves every N frames

step :: Game -> Char -> IO Game
step g@Game{..} c = do
  let p'  = tryMove pacman (dir c) walls
      ps' = delete p' pellets
      sc  = score + if p' `elem` pellets then 10 else 0

  ghs' <- sequence $ zipWith (\gPos t ->
                if t <= 0 then ghostStepRandom p' gPos walls ghosts
                else return gPos) ghosts ghostTimers

  let timers' = zipWith (\gPos t -> if t <= 0 then ghostMoveInterval else t - 1) ghosts ghostTimers

      st'
        | p' `elem` ghs' = Lost
        | null ps'        = Won
        | otherwise       = Running

  return g { pacman = p', pellets = ps', ghosts = ghs', ghostTimers = timers', status = st', score = sc, frameCount = frameCount + 1 }

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


type Pos = (Int, Int)

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)  -- example Manhattan distance

ghostStepRandom :: Pos -> Pos -> [Pos] -> [Pos] -> IO Pos
ghostStepRandom pac g walls others = do
  let candidates = filter (`notElem` walls) $
                   filter (`notElem` others) $
                   [(fst g + dx, snd g + dy) | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]
  case nonEmpty candidates of
    Nothing -> return g
    Just neCandidates -> do
      let lst = toList neCandidates
      r <- randomRIO (0,9 :: Int)
      let sorted = sortOn (dist pac) lst
      return $ if r < 8 then head sorted else lst !! (r `mod` length lst)


dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

draw :: Game -> IO ()
draw Game{..} = do
  putStr "\ESC[H"  -- Move cursor home
  mapM_ putStrLn
    [ [ tile (x,y) | x <- [0..width-1] ]
    | y <- [0..height-1]
    ]
  putStrLn $ "Score: " ++ show score
  where
    tile p
      | p == pacman       = 'C'
      | p `elem` ghosts   = 'G'
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
      ghostList = [(w-2,h-2),(w-2,1)]
  in Game
      { pacman      = (1,1)
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

