module PlayBattleships 
    ( playBattleships
    , checkEnemyMove
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

-- Provided enemy moves returns whether it was a 'hit' or 'miss'
checkEnemyMove :: [String] -> State GameState SquareState
checkEnemyMove []       = state $ \gameState -> (NULL, gameState)
checkEnemyMove ("":_)   = state $ \gameState -> (EMPTY, gameState)
checkEnemyMove (move:_) = state $ \gameState ->
    let ships = myShips gameState in
        case find (== move) ships of
            Nothing -> (MISS, gameState)
            Just s -> (HIT, gameState {myShips = delete s ships})
