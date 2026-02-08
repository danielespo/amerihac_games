{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import System.Console.ANSI
import System.Exit
import System.Random

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
  , lives       :: Int
  , rng         :: StdGen
  }

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

frameDelay :: Int
frameDelay = 80 * 1000 -- ~12 FPS

ghostMoveInterval :: Int
ghostMoveInterval = 3

maxLives :: Int
maxLives = 3

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  gen <- getStdGen
  gameLoop (initialGame gen)

--------------------------------------------------------------------------------
-- Game Loop
--------------------------------------------------------------------------------

gameLoop :: Game -> IO ()
gameLoop g@Game{..} = do
  renderGame g
  case status of
    Running -> do
      input <- pollInput
      threadDelay frameDelay
      gameLoop (updateGame g input)
    Won  -> endGame g "YOU WIN!"
    Lost -> if lives > 1
            then do
              putStrLn "You lost a life! Press any key..."
              _ <- getChar
              gen' <- newStdGen
              let g' = (initialGame gen') { lives = lives - 1, score = score }
              gameLoop g'
            else endGame g "GAME OVER"

endGame :: Game -> String -> IO ()
endGame Game{..} msg = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ msg ++ " Score: " ++ show score
  putStrLn "Press r to restart or q to quit."
  c <- getChar
  case c of
    'r' -> do gen <- newStdGen
              gameLoop (initialGame gen)
    'q' -> exitSuccess
    _   -> return ()

pollInput :: IO Char
pollInput = do
  ready <- hReady stdin
  if ready then getChar else return ' '

--------------------------------------------------------------------------------
-- Game Update
--------------------------------------------------------------------------------

updateGame :: Game -> Char -> Game
updateGame g@Game{..} input =
  let pac'      = tryMove pacman (dir input) walls
      pellets'  = S.delete pac' pellets
      score'    = score + if pac' `S.member` pellets then 10 else 0

      (ghosts', timers', rng') = moveGhosts ghosts ghostTimers rng pac' walls

      status'
        | pac' `elem` ghosts' = Lost
        | S.null pellets'     = Won
        | otherwise           = Running
  in g { pacman = pac'
       , pellets = pellets'
       , ghosts  = ghosts'
       , ghostTimers = timers'
       , status = status'
       , score = score'
       , rng = rng'
       }

dir :: Char -> Pos
dir 'w' = (0,-1)
dir 's' = (0, 1)
dir 'a' = (-1,0)
dir 'd' = (1,0)
dir 'W' = (0,-1)
dir 'S' = (0, 1)
dir 'A' = (-1,0)
dir 'D' = (1,0)
dir _   = (0,0)

tryMove :: Pos -> Pos -> S.Set Pos -> Pos
tryMove (x,y) (dx,dy) walls =
  let p = (x+dx, y+dy)
  in if p `S.member` walls then (x,y) else p

moveGhosts :: [Pos] -> [Int] -> StdGen -> Pos -> S.Set Pos -> ([Pos],[Int],StdGen)
moveGhosts ghosts timers gen pac walls =
  let (newGhosts, newTimers, gen') = foldl go ([],[],gen) (zip ghosts timers)
      go (gs, ts, g) (pos,t)
        | t <= 0 =
            let (pos', g') = ghostAI pos pac walls g
            in (gs++[pos'], ts++[ghostMoveInterval], g')
        | otherwise = (gs++[pos], ts++[t-1], g)
  in (newGhosts,newTimers,gen')

ghostAI :: Pos -> Pos -> S.Set Pos -> StdGen -> (Pos, StdGen)
ghostAI g pac walls gen =
  let options = filter (`S.notMember` walls) $ neighbors g
      options' = if null options then [g] else options
      -- 50% chance to move toward pacman, else random
      (r, gen') = randomR (0 :: Int, 1) gen
      target = if r == 0
               then minimumBy (\a b -> compare (manhattan a pac) (manhattan b pac)) options'
               else let (idx, g'') = randomR (0, length options' - 1) gen' in (options' !! idx, g'')
  in target

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x+dx,y+dy) | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]

manhattan :: Pos -> Pos -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderGame :: Game -> IO ()
renderGame Game{..} = do
  setCursorPosition 0 0
  forM_ [0..height-1] $ \y -> do
    forM_ [0..width-1] $ \x -> putChar (tile (x,y))
    putChar '\n'
  putStrLn $ "Score: " ++ show score ++ " Lives: " ++ show lives
  where
    tile p
      | p == pacman       = 'C'
      | p `elem` ghosts   = 'G'
      | p `S.member` walls= '#'
      | p `S.member` pellets = '.'
      | otherwise         = ' '

--------------------------------------------------------------------------------
-- Level
--------------------------------------------------------------------------------

initialGame :: StdGen -> Game
initialGame gen =
  let w = 21
      h = 15
      walls = S.fromList $
        [(x,0) | x <- [0..w-1]] ++
        [(x,h-1) | x <- [0..w-1]] ++
        [(0,y) | y <- [0..h-1]] ++
        [(w-1,y) | y <- [0..h-1]] ++
        [(10,y) | y <- [3..11]]
      pellets = S.fromList [(x,y) | x <- [1..w-2], y <- [1..h-2], (x,y) `S.notMember` walls]
      ghosts = [(w-2,h-2),(w-2,1)]
  in Game { pacman      = (1,1)
          , ghosts      = ghosts
          , pellets     = pellets
          , walls       = walls
          , width       = w
          , height      = h
          , status      = Running
          , ghostTimers = replicate (length ghosts) 0
          , score       = 0
          , lives       = maxLives
          , rng         = gen
          }

