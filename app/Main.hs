module Main where

import BrickUI (launchBrick)

{-
Old code for reference.

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
-}

fibty :: Int -> Int
fibty 1 = 0
fibty 2 = 1
fibty n =
    let left = fibty (n - 1)
        right = fibty (n - 2)
     in left + right

main :: IO ()
main = launchBrick
