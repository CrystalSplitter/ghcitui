{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (unless)
import Data.IORef (readIORef)
import qualified Data.Text.IO as TextIO
import System.IO as SIO

import BrickUI (launchBrick)
import qualified Daemon as D
import Tui (getSurroundingSrc, loadFileSrc)

main :: IO ()
main = launchBrick

launch :: IO ()
launch = do
    state <- D.startup "cabal repl" "."
    (state, _) <- D.exec state ":l app/Main.hs"
    fileRef <- loadFileSrc "app/Main.hs"
    let surroundingSrc windowSize D.InterpState{D.lineno} =
            do
                src <- readIORef fileRef
                case lineno of
                    Nothing -> pure []
                    Just l -> pure $ getSurroundingSrc src windowSize l
    state <- D.stepInto state "fibty 10"
    let loop s = do
            print s
            newWindow <- surroundingSrc 5 s
            newS <- D.step state
            mapM_ TextIO.putStrLn newWindow
            putStr "%% "
            SIO.hFlush SIO.stdout
            stdinLine <- getLine
            if stdinLine == "q"
                then pure ()
                else do
                    (newS, msgs) <- D.exec state stdinLine
                    mapM_ (putStrLn . ("OUT: " ++)) msgs
                    loop newS
    loop state
    D.quit state
    pure ()

fibty :: Int -> Int
fibty 1 = 0
fibty 2 = 1
fibty n =
    let left = fibty (n - 1)
        right = fibty (n - 2)
     in left + right

{-
--> fib 5
  Stopped in Main.fib, app/Main.hs:22:9-33
-}
