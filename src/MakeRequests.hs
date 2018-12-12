module MakeRequests 
    ( nameName
    ) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Network.HTTP

import BattleshipTypes
import PlayBattleships

nameName :: GameState -> String -> ExceptT String IO String
nameName gameState url = do
    response <- liftIO $ simpleHTTP (getRequest url) >>= getResponseBody
    if response == "No move available at the moment" then nameName gameState url
    else do
        let (playResult ,newState) = runState (runExceptT $ playBattleships response) gameState
        case playResult of -- I don't want to write this!!!
            Left error -> throwE error
            Right (coords, result) -> case coords of
                Nothing -> return "Victory!"
                Just justCoords -> if null $ myShips newState
                    then postDefeat response
                    else postTurn newState url response justCoords result

postDefeat :: String -> ExceptT String IO String
postDefeat _ = throwE "NOT IMPLEMENTED"

-- This is the longest one
postTurn :: GameState -> String -> String -> [String] -> SquareState -> ExceptT String IO String
postTurn _ _ _ _ _ = throwE "NOT IMPLEMENTED"
