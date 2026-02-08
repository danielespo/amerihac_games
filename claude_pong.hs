{-# LANGUAGE OverloadedStrings #-}

-- Terminal Pong - Single Player vs AI
-- Run with: runghc pong.hs
-- Or compile: ghc -O2 pong.hs && ./pong

import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO
import System.Console.ANSI
import System.Timeout
import Data.Char (toLower)

-- Game constants
gameWidth :: Int
gameWidth = 80

gameHeight :: Int
gameHeight = 24

paddleHeight :: Int
paddleHeight = 4

frameDelay :: Int
frameDelay = 50000  -- 50ms = ~20 FPS

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
    , ballVX = -0.8
    , ballVY = 0.4
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
            (newY', newVY) = if newY <= 0 || newY >= fromIntegral gameHeight - 1
                             then (max 0 (min (fromIntegral gameHeight - 1) newY), -(ballVY state))
                             else (newY, ballVY state)
            
            -- Ball collision with paddles
            (newX', newVX, newScore1, newScore2) = 
                let p1Top = paddle1Y state
                    p1Bottom = paddle1Y state + paddleHeight
                    p2Top = paddle2Y state
                    p2Bottom = paddle2Y state + paddleHeight
                    ballYInt = floor newY'
                in
                -- Left paddle collision
                if newX <= 2 && ballYInt >= p1Top && ballYInt < p1Bottom
                then (2, abs (ballVX state) * 1.05, score1 state, score2 state)
                -- Right paddle collision
                else if newX >= fromIntegral gameWidth - 3 && ballYInt >= p2Top && ballYInt < p2Bottom
                then (fromIntegral gameWidth - 3, -(abs (ballVX state)) * 1.05, score1 state, score2 state)
                -- Ball out of bounds (left)
                else if newX < 0
                then (fromIntegral gameWidth / 2, 0, score1 state, score2 state + 1)
                -- Ball out of bounds (right)
                else if newX >= fromIntegral gameWidth
                then (fromIntegral gameWidth / 2, 0, score1 state + 1, score2 state)
                else (newX, ballVX state, score1 state, score2 state)
            
            -- Reset ball if scored
            (finalX, finalY, finalVX, finalVY) = 
                if newScore1 /= score1 state || newScore2 /= score2 state
                then (fromIntegral gameWidth / 2, fromIntegral gameHeight / 2, 
                      if newScore1 /= score1 state then -0.8 else 0.8, 0.4)
                else (newX', newY', newVX, newVY)
            
            -- AI for right paddle (simple following logic)
            aiTarget = floor (ballY state)
            aiPaddleCenter = paddle2Y state + paddleHeight `div` 2
            aiDelta = if aiTarget < aiPaddleCenter - 1 then -1
                      else if aiTarget > aiPaddleCenter + 1 then 1
                      else 0
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

-- Render game state as string
renderGame :: GameState -> String
renderGame state =
    let grid = replicate gameHeight (replicate gameWidth ' ')
        
        -- Add paddles
        gridWithP1 = foldl (\g y -> updateGrid g 1 y '█') grid 
                     [paddle1Y state .. paddle1Y state + paddleHeight - 1]
        gridWithPaddles = foldl (\g y -> updateGrid g (gameWidth - 2) y '█') gridWithP1
                          [paddle2Y state .. paddle2Y state + paddleHeight - 1]
        
        -- Add ball
        ballGridY = floor (ballY state)
        ballGridX = floor (ballX state)
        gridWithBall = if ballGridX >= 0 && ballGridX < gameWidth && ballGridY >= 0 && ballGridY < gameHeight
                       then updateGrid gridWithPaddles ballGridX ballGridY '●'
                       else gridWithPaddles
        
        -- Convert to string
        rendered = unlines $ map id gridWithBall
        topBorder = "┌" ++ replicate gameWidth '─' ++ "┐"
        bottomBorder = "└" ++ replicate gameWidth '─' ++ "┘"
        scoreDisplay = "  Player: " ++ show (score1 state) ++ " | AI: " ++ show (score2 state) ++ 
                      if gameOver state 
                      then " | GAME OVER! " ++ (if score1 state > score2 state then "YOU WIN!" else "AI WINS!")
                      else " | First to 10 wins!"
        controls = "  Controls: W=Up | S=Down | Q=Quit"
        
    in "\n" ++ topBorder ++ "\n" ++ 
       unlines (map (\line -> "│" ++ line ++ "│") (lines rendered)) ++ 
       bottomBorder ++ "\n" ++ scoreDisplay ++ "\n" ++ controls ++ "\n"
    where
        updateGrid grid x y c =
            if y >= 0 && y < length grid && x >= 0 && x < gameWidth
            then let (before, row:after) = splitAt y grid
                     (beforeC, _:afterC) = splitAt x row
                 in before ++ [beforeC ++ [c] ++ afterC] ++ after
            else grid

-- Non-blocking getChar
getNonBlockingChar :: IO (Maybe Char)
getNonBlockingChar = timeout 0 getChar

-- Main game loop
gameLoop :: IORef GameState -> IO ()
gameLoop stateRef = do
    -- Get input
    maybeChar <- getNonBlockingChar
    case maybeChar of
        Just 'w' -> modifyIORef stateRef $ \s -> s { paddle1Y = movePaddle (paddle1Y s) (-1) }
        Just 's' -> modifyIORef stateRef $ \s -> s { paddle1Y = movePaddle (paddle1Y s) 1 }
        Just 'q' -> modifyIORef stateRef $ \s -> s { gameOver = True }
        _ -> return ()
    
    -- Update game state
    modifyIORef stateRef updateGame
    
    -- Render
    state <- readIORef stateRef
    clearScreen
    setCursorPosition 0 0
    putStr $ renderGame state
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
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen
    
    -- Show welcome message
    putStrLn "\n╔════════════════════════════════════════╗"
    putStrLn "║         TERMINAL PONG v1.0             ║"
    putStrLn "║                                        ║"
    putStrLn "║  You (Left) vs AI (Right)              ║"
    putStrLn "║  First to 10 points wins!              ║"
    putStrLn "║                                        ║"
    putStrLn "║  Controls:                             ║"
    putStrLn "║    W - Move Up                         ║"
    putStrLn "║    S - Move Down                       ║"
    putStrLn "║    Q - Quit Game                       ║"
    putStrLn "║                                        ║"
    putStrLn "╚════════════════════════════════════════╝\n"
    putStrLn "Press any key to start..."
    _ <- getChar
    
    -- Start game
    stateRef <- newIORef initialState
    gameLoop stateRef
    
    -- Cleanup
    showCursor
    hSetEcho stdin True
    clearScreen
    putStrLn "\nThanks for playing Terminal Pong!"
