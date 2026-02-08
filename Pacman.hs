{-# LANGUAGE RecordWildCards #-}

import System.IO
import Control.Concurrent
import Control.Monad
import Data.List
import System.Timeout

type Pos = (Int,Int)

data Mode = Chase | Scatter | Frightened deriving (Eq,Show)

data Ghost = Ghost
  { gpos  :: Pos
  , ghome :: Pos
  , gtype :: Int
  }

data Game = Game
  { walls   :: [Pos]
  , pellets :: [Pos]
  , powers  :: [Pos]
  , pacman  :: Pos
  , ghosts  :: [Ghost]
  , score   :: Int
  , mode    :: Mode
  , timer   :: Int
  }

width, height :: Int
width = 21
height = 11

maze :: [String]
maze =
  [ "#####################"
  , "#o.......o#o.......o#"
  , "#.###.###.#.#.###.###.#"
  , "#.....................#"
  , "#.###.#.#####.#.###.#.#"
  , "#.....#...#...#.....#.#"
  , "#####.###.#.#.###.#####"
  , "#.........#.........#"
  , "#.###.###.#.#.###.###.#"
  , "#.....................#"
  , "#####################"
  ]

parseMaze :: ([Pos],[Pos],[Pos])
parseMaze = (walls,pellets,powers)
  where
    coords = [((x,y),c) | (y,row) <- zip [0..] maze
                        , (x,c)   <- zip [0..] row]
    walls   = [p | (p,'#') <- coords]
    pellets = [p | (p,'.') <- coords]
    powers  = [p | (p,'o') <- coords]

clear :: IO ()
clear = putStr "\ESC[2J\ESC[H"

draw :: Game -> IO ()
draw Game{..} = do
  clear
  forM_ [0..height-1] $ \y -> do
    forM_ [0..width-1] $ \x -> do
      let p = (x,y)
      putChar $ case () of
        _ | p == pacman          -> 'C'
          | p `elem` map gpos ghosts -> if mode == Frightened then 'g' else 'G'
          | p `elem` walls       -> '#'
          | p `elem` pellets    -> '.'
          | p `elem` powers     -> 'o'
          | otherwise            -> ' '
    putStrLn ""
  putStrLn $ "Score: " ++ show score ++ "   Mode: " ++ show mode

move :: Pos -> Char -> Pos
move (x,y) c = case c of
  'w' -> (x,y-1)
  's' -> (x,y+1)
  'a' -> (x-1,y)
  'd' -> (x+1,y)
  _   -> (x,y)

valid :: [Pos] -> Pos -> Pos -> Pos
valid ws old new
  | new `elem` ws = old
  | otherwise     = new

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

ghostTarget :: Game -> Ghost -> Pos
ghostTarget Game{..} Ghost{..} =
  case (mode,gtype) of
    (Scatter,_) -> ghome
    (Chase,0)   -> pacman
    (Chase,1)   -> let (x,y)=pacman in (x+4,y)
    (Chase,2)   -> let (x,y)=pacman in (x,y+4)
    (Chase,3)   -> if dist pacman gpos > 6 then pacman else ghome
    _           -> ghome

ghostMove :: Game -> Ghost -> Ghost
ghostMove game@Game{..} g@Ghost{..}
  | mode == Frightened = g { gpos = safest }
  | otherwise          = g { gpos = best }
  where
    ns = filter (`notElem` walls) (neighbors gpos)
    tgt = ghostTarget game g
    best = minimumBy (\a b -> compare (dist a tgt) (dist b tgt)) ns
    safest = maximumBy (\a b -> compare (dist a pacman) (dist b pacman)) ns

step :: Game -> Char -> Game
step game@Game{..} c =
  let p' = valid walls pacman (move pacman c)
      ateP = p' `elem` pellets
      ateO = p' `elem` powers

      pellets' = delete p' pellets
      powers'  = delete p' powers

      mode' = if ateO then Frightened else if timer == 0 then switch else mode
      timer' = if ateO then 60 else max 0 (timer-1)

      ghosts' = map (ghostMove game{pacman=p',mode=mode'}) ghosts

      (ghosts'', bonus) =
        if mode' == Frightened
           then let (dead,alive) = partition ((==p') . gpos) ghosts'
                 in (map (\g->g{gpos=ghome g}) dead ++ alive, 200 * length dead)
           else (ghosts',0)

      score' = score + (if ateP then 1 else 0)
                        + (if ateO then 10 else 0)
                        + bonus

  in game { pacman=p'
          , pellets=pellets'
          , powers=powers'
          , ghosts=ghosts''
          , score=score'
          , mode=mode'
          , timer=timer'
          }

  where
    switch = if mode == Chase then Scatter else Chase

gameOver :: Game -> Bool
gameOver Game{..} =
  pacman `elem` map gpos ghosts && mode /= Frightened

gameWin :: Game -> Bool
gameWin Game{..} = null pellets && null powers

initialGame :: Game
initialGame =
  let (ws,ps,os) = parseMaze
  in Game ws ps os (1,1)
       [ Ghost (19,9) (19,9) 0
       , Ghost (1,9)  (1,9)  1
       , Ghost (19,1) (19,1) 2
       , Ghost (10,5) (10,5) 3
       ]
       0 Chase 100

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
      putStrLn msg
      threadDelay endDelay

    pollInput =
      maybe ' ' id <$> timeout 0 getChar

    frameDelay = 100000   -- 10 FPS
    endDelay   = 2000000

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  gameLoop initialGame

