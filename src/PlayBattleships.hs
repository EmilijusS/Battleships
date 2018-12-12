module PlayBattleships 
    ( playBattleships
    ) where 

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.List

import BattleshipTypes
import Move
import ListMoves

-- From JSON returns result and coords
playBattleships :: String -> ExceptT String (State GameState) (Maybe [String], SquareState)
playBattleships json = 
    -- let moves = getMoveList json
    -- let myMove = moves >>= move
    -- let enemyMoveResult = checkEnemyMove <$> moves -- (this might not even work)
    -- ?????
    case getMoveList json of
        Left moves -> throwE moves
        Right moves -> case move moves of
            Left myMove -> throwE myMove
            Right myMove -> do
                enemyMoveResult <- lift $ checkEnemyMove moves
                return (myMove, enemyMoveResult)

-- Provided last enemy move returns whether it was a 'hit' or 'miss'
checkEnemyMove :: [String] -> State GameState SquareState
checkEnemyMove [] = state $ \gameState -> (EMPTY, gameState)
checkEnemyMove (move:_) = state $ \gameState ->
    let ships = myShips gameState in
        case find (== move) ships of
            Nothing -> (MISS, gameState)
            Just s -> (HIT, gameState {myShips = delete s ships})

-- Checks if we lost
-- checkDefeat :: State GameState Bool
-- checkDefeat = state $ \gameState -> null $ myShips gameState

-- Provided a previous JSON, move and enemy shot result forms a new JSON to send
-- createJSON :: String -> [String] -> SquareState -> String
-- createJSON prev [] result = "{\"coord\":{},\"result\":\"" ++ show result ++ "\",\"prev\":" ++ prev ++ "}"
-- createJSON prev coord result = "{\"coord\":{\"1\":\"" ++ head coord ++ 
--                                "\",\"2\":\"" ++ tail coord ++
--                                "\"},\"result\":\"" ++ show result ++
--                                "\",\"prev\":" ++ prev ++ "}"