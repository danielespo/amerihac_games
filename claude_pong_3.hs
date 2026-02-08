{-# LANGUAGE OverloadedStrings #-}

-- Terminal Pong - Single Player vs AI
-- Run with: runghc pong.hs
-- Or compile: ghc -O2 pong.hs && ./pong

import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO
import Data.Char (toLower)
import Data.List (intercalate)

-- Game constants
gameWidth :: Int
gameWidth = 120

gameHeight :: Int
gameHeight = 40

paddleHeight :: Int
paddleHeight = 10

frameDelay :: Int
frameDelay = 10000  -- ~30 FPS

-- Game state
data GameState = GameState
    { ballX :: Float
    , ballY :: Float
    , ballVX :: Float
    , ballVY :: Float
    , paddle1Y :: Int
    , paddle2Y :: Int
    , score1 :: Int
    , score2 :: Int
    , gameOver :: Bool
    } deriving (Show)

initialState :: GameState
initialState = GameState
    { ballX = fromIntegral gameWidth / 2
    , ballY = fromIntegral gameHeight / 2
    , ballVX = -1.0
    , ballVY = 0.0
    , paddle1Y = gameHeight `div` 2 - paddleHeight `div` 2
    , paddle2Y = gameHeight `div` 2 - paddleHeight `div` 2
    , score1 = 0
    , score2 = 0
    , gameOver = False
    }

-- Update game state
updateGame :: GameState -> GameState
updateGame state
    | gameOver state = state
    | otherwise =
        let newX = ballX state + ballVX state
            newY = ballY state + ballVY state
            
            -- Ball collision with top/bottom
            (newY', newVY) = if newY <= 0
                             then (0, abs (ballVY state))
                             else if newY >= fromIntegral (gameHeight - 1)
                             then (fromIntegral (gameHeight - 1), -(abs (ballVY state)))
                             else (newY, ballVY state)
            
            -- Ball collision with paddles
            (newX', newVX', newVY', newScore1, newScore2) = 
                let p1Top = paddle1Y state
                    p1Bottom = paddle1Y state + paddleHeight - 1
                    p2Top = paddle2Y state
                    p2Bottom = paddle2Y state + paddleHeight - 1
                    ballYInt = round newY'
                    
                    -- Calculate where ball hits paddle (0.0 = top, 1.0 = bottom)
                    hitRatio paddleTop = (newY' - fromIntegral paddleTop) / fromIntegral (paddleHeight - 1)
                    -- Convert to angle influence (-1.0 to 1.0, where 0 is center)
                    angleInfluence ratio = (ratio - 0.5) * 2.0
                in
                -- Left paddle collision
                if newX <= 1.5 && newX >= 0.5 && ballYInt >= p1Top && ballYInt <= p1Bottom && ballVX state < 0
                then let ratio = hitRatio (fromIntegral p1Top)
                         angle = angleInfluence ratio
                         newVY'' = angle * 0.8  -- Adjust vertical velocity based on hit location
                     in (1.5, abs (ballVX state) * 1.03, newVY'', score1 state, score2 state)
                -- Right paddle collision  
                else if newX >= fromIntegral gameWidth - 2.5 && newX <= fromIntegral gameWidth - 1.5 
                        && ballYInt >= p2Top && ballYInt <= p2Bottom && ballVX state > 0
                then let ratio = hitRatio (fromIntegral p2Top)
                         angle = angleInfluence ratio
                         newVY'' = angle * 0.8
                     in (fromIntegral gameWidth - 2.5, -(abs (ballVX state)) * 1.03, newVY'', score1 state, score2 state)
                -- Ball out of bounds (left)
                else if newX < 0
                then (fromIntegral gameWidth / 2, 1.0, 0.0, score1 state, score2 state + 1)
                -- Ball out of bounds (right)
                else if newX >= fromIntegral gameWidth
                then (fromIntegral gameWidth / 2, -1.0, 0.0, score1 state + 1, score2 state)
                else (newX, ballVX state, newVY, score1 state, score2 state)
            
            -- Reset ball if scored
            (finalX, finalY, finalVX, finalVY) = 
                if newScore1 /= score1 state || newScore2 /= score2 state
                then (fromIntegral gameWidth / 2, fromIntegral gameHeight / 2, 
                      if newScore1 /= score1 state then -1.0 else 1.0, 0.0)
                else (newX', newY', newVX', newVY')
            
            -- AI for right paddle - better AI that tracks ball
            aiTarget = round (ballY state)
            aiPaddleCenter = paddle2Y state + paddleHeight `div` 2
            aiDelta = if ballVX state > 0  -- Only move when ball coming towards AI
                      then if aiTarget < aiPaddleCenter then -1
                           else if aiTarget > aiPaddleCenter then 1
                           else 0
                      else 0  -- Stay still when ball going away
            newPaddle2Y = max 0 (min (gameHeight - paddleHeight) (paddle2Y state + aiDelta))
            
            -- Check for game over
            isGameOver = newScore1 >= 10 || newScore2 >= 10
                
        in state { ballX = finalX
                 , ballY = finalY
                 , ballVX = finalVX
                 , ballVY = finalVY
                 , paddle2Y = newPaddle2Y
                 , score1 = newScore1
                 , score2 = newScore2
                 , gameOver = isGameOver
                 }

-- Move paddle
movePaddle :: Int -> Int -> Int
movePaddle currentY delta =
    max 0 (min (gameHeight - paddleHeight) (currentY + delta))

-- Render game state to a buffer (no flickering)
renderGame :: GameState -> String
renderGame state =
    let -- Create empty grid
        emptyRow = replicate gameWidth ' '
        grid = replicate gameHeight emptyRow
        
        -- Helper to set character at position
        setChar :: [[Char]] -> Int -> Int -> Char -> [[Char]]
        setChar g x y c 
            | x < 0 || x >= gameWidth || y < 0 || y >= gameHeight = g
            | otherwise = 
                let (rowsBefore, row:rowsAfter) = splitAt y g
                    (charsBefore, _:charsAfter) = splitAt x row
                in rowsBefore ++ [charsBefore ++ [c] ++ charsAfter] ++ rowsAfter
        
        -- Add left paddle
        gridWithP1 = foldl (\g y -> setChar g 0 y '█') grid 
                     [paddle1Y state .. paddle1Y state + paddleHeight - 1]
        
        -- Add right paddle
        gridWithPaddles = foldl (\g y -> setChar g (gameWidth - 1) y '█') gridWithP1
                          [paddle2Y state .. paddle2Y state + paddleHeight - 1]
        
        -- Add ball
        ballGridX = round (ballX state)
        ballGridY = round (ballY state)
        finalGrid = setChar gridWithPaddles ballGridX ballGridY '●'
        
        -- Add borders
        topBorder = "┌" ++ replicate gameWidth '─' ++ "┐"
        bottomBorder = "└" ++ replicate gameWidth '─' ++ "┘"
        borderedRows = map (\row -> "│" ++ row ++ "│") finalGrid
        
        -- Score and info
        scoreText = "Player: " ++ padLeft 2 (show (score1 state)) ++ " | AI: " ++ padLeft 2 (show (score2 state))
        gameStatus = if gameOver state 
                     then if score1 state > score2 state 
                          then "YOU WIN!" 
                          else "AI WINS!"
                     else "First to 10"
        controlText = "W/S: Move | Q: Quit"
        
        -- Combine everything
        lines' = [topBorder] ++ borderedRows ++ [bottomBorder, "", 
                  center (scoreText ++ " | " ++ gameStatus),
                  center controlText]
        
    in unlines lines'
    where
        padLeft n s = replicate (n - length s) ' ' ++ s
        center s = let spaces = (gameWidth + 2 - length s) `div` 2
                   in replicate spaces ' ' ++ s

-- Clear screen without ANSI (for better compatibility)
clearScr :: IO ()
clearScr = putStr "\ESC[2J\ESC[H"

-- Non-blocking input check
getNonBlockingChar :: IO (Maybe Char)
getNonBlockingChar = do
    ready <- hReady stdin
    if ready
        then do
            c <- getChar
            return (Just (toLower c))
        else return Nothing

-- Main game loop with double buffering
gameLoop :: IORef GameState -> IO ()
gameLoop stateRef = do
    -- Handle input
    maybeChar <- getNonBlockingChar
    case maybeChar of
        Just 'w' -> modifyIORef stateRef $ \s -> s { paddle1Y = movePaddle (paddle1Y s) (-1) }
        Just 's' -> modifyIORef stateRef $ \s -> s { paddle1Y = movePaddle (paddle1Y s) 1 }
        Just 'q' -> modifyIORef stateRef $ \s -> s { gameOver = True }
        _ -> return ()
    
    -- Update physics
    modifyIORef stateRef updateGame
    
    -- Render entire frame to buffer
    state <- readIORef stateRef
    let frame = renderGame state
    
    -- Clear and draw in one go (reduces flicker)
    clearScr
    putStr frame
    hFlush stdout
    
    -- Continue or end
    unless (gameOver state) $ do
        threadDelay frameDelay
        gameLoop stateRef

-- Main
main :: IO ()
main = do
    -- Setup terminal
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout (BlockBuffering (Just 4096))  -- Buffer output
    hSetEcho stdin False
    
    -- Hide cursor
    putStr "\ESC[?25l"
    
    clearScr
    
    -- Show welcome screen
    putStrLn ""
    putStrLn "  ╔════════════════════════════════════════╗"
    putStrLn "  ║         TERMINAL PONG v2.0            ║"
    putStrLn "  ║                                       ║"
    putStrLn "  ║  Player (Left) vs AI (Right)          ║"
    putStrLn "  ║  First to 10 points wins!             ║"
    putStrLn "  ║                                       ║"
    putStrLn "  ║  Hit the ball with the edge of        ║"
    putStrLn "  ║  your paddle to angle your shots!     ║"
    putStrLn "  ║                                       ║"
    putStrLn "  ║  Controls:                            ║"
    putStrLn "  ║    W - Move Up                        ║"
    putStrLn "  ║    S - Move Down                      ║"
    putStrLn "  ║    Q - Quit Game                      ║"
    putStrLn "  ║                                       ║"
    putStrLn "  ╚════════════════════════════════════════╝"
    putStrLn ""
    putStrLn "  Press any key to start..."
    hFlush stdout
    _ <- getChar
    
    -- Start game
    stateRef <- newIORef initialState
    gameLoop stateRef
    
    -- Cleanup
    putStr "\ESC[?25h"  -- Show cursor
    hSetEcho stdin True
    clearScr
    putStrLn "\nThanks for playing Terminal Pong!"
