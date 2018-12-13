module MakeRequests 
    ( getGame
    ) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Network.HTTP
import Network.HTTP.Headers

import BattleshipTypes
import PlayBattleships

-- If they have to start the game, call this
getGame :: GameState -> String -> ExceptT String IO String
getGame gameState url = do
    response <- liftIO $
        simpleHTTP ( (getRequest url) {rqHeaders = [Header HdrAccept "application/json+nolists"]} ) >>=
             getResponseBody
    if response == "No move available at the moment" 
    then liftIO (putStrLn "Waiting for response...") >> getGame gameState url
    else do
        let (playResult, newState) = runState (runExceptT $ playBattleships response) gameState
        case playResult of -- I don't want to write this!!!
            Left error -> throwE error
            Right (coords, result) -> case coords of
                Nothing -> return "Victory!"
                Just justCoords -> if null $ myShips newState
                    then postDefeat url response
                    else postTurn newState url response justCoords result

-- If we have to start the game, call this
postStart :: GameState -> String -> ExceptT String IO String
postStart gameState url = do
    let (playResult, newState) = runState (runExceptT $ playBattleships "") gameState
    case playResult of -- I don't want to write this again!!!
        Left error -> throwE error -- This will never happen
        Right (coord, result) -> case coord of
            Nothing -> return "Victory!" -- This will never happen
            Just justCoord -> do
                let json = "{\"coord\":{\"1\":\"" ++ head justCoord ++ 
                           "\",\"2\":\"" ++ last justCoord ++
                           "\"},\"result\":null, \"prev\":null}"
                nextTurn gameState url json justCoord result

-- Posts our turn and prints it to console
postTurn :: GameState -> String -> String -> [String] -> SquareState -> ExceptT String IO String
postTurn gameState url prev coord result = do
    let json = "{\"coord\":{\"1\":\"" ++ head coord ++ 
               "\",\"2\":\"" ++ last coord ++
               "\"},\"result\":\"" ++ show result ++
               "\",\"prev\":" ++ prev ++ "}"
    nextTurn gameState url json coord result

-- TBH I only wrote this to get rid of red squigly lines
nextTurn :: GameState -> String -> String -> [String] -> SquareState -> ExceptT String IO String
nextTurn gameState url json coord result = do
    liftIO $ postJson url json -- There's indeed duplication, but not here
    liftIO $ putStrLn $ concat coord ++ show result
    getGame gameState url

-- Call this when you lose
postDefeat :: String -> String -> ExceptT String IO String
postDefeat url prev = do
    let json = "{\"coord\":{},\"result\":\"" ++ show HIT ++ "\",\"prev\":" ++ prev ++ "}"
    liftIO $ postJson url json
    return "Defeat!"

postJson :: String -> String -> IO ()
postJson url json = do
    simpleHTTP $ postRequestWithBody url "application/json+nolists" json
    return ()



