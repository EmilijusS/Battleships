module CheckEnemyMove 
    ( checkEnemyMove
    ) where

import Control.Monad.State.Lazy
import Data.List

import BattleshipTypes

-- Provided last enemy move returns whether it was a 'hit' or 'miss'
checkEnemyMove :: String -> State GameState SquareState
checkEnemyMove move = state $ \gameState ->
    let ships = myShips gameState in
        case find (== move) ships of
            Nothing -> (Miss, gameState)
            Just s -> (Hit, gameState {myShips = delete s ships})
