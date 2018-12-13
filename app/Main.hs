module Main where

import System.Environment 
import Control.Monad.Trans.Except

import BattleshipTypes
import MakeRequests

main :: IO ()
main = do
    args <- getArgs
    if null args then printArgs
        else let 
            url = head args
            gameState = GameState {myShips = tail args, myGuesses = []}
            in
                if last url == 'A'
                then runExceptT (postStart gameState url) >>= \x -> print x
                    else if last url == 'B'
                    then runExceptT (getGame gameState url) >>= \x -> print x
                    else printArgs

printArgs :: IO ()
printArgs = mapM_ putStrLn [
      "This bot can play as either player A or B (infers that from url) and you have to provide your ship position coordinates."
    , "Args: url shipPos1 shipPos2 ..."
    , "Example: \"stack exec Battleships-exe http://battleship.haskell.lt/game/test1/player/A A1 B1 C1 D1 E5 E6 E7\""
    ]
