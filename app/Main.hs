module Main where

import System.Environment 

import Move
import ListMoves

main :: IO ()
main = do
    args <- getArgs
    let json = head args
    if null args then  putStrLn "You must pass json as argument" else
        putStrLn $ nextMoveString json


nextMoveString :: String -> String
nextMoveString json = case getMoveList json >>= move of
    Left x -> x
    Right x -> case x of
        Nothing -> "No moves available"
        Just y -> show y