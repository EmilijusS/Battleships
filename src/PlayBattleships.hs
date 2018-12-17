module PlayBattleships 
    ( playBattleships
    , checkEnemyMove
    ) where 

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.List
import System.Random

import BattleshipTypes
import ListMoves

-- From JSON returns result and coords
playBattleships :: String -> ExceptT String (State GameState) (String, SquareState)
playBattleships json = 
    -- let moves = getMoveList json
    -- let myMove = moves >>= move
    -- let enemyMoveResult = checkEnemyMove <$> moves -- (this might not even work)
    -- ?????
    case getMoveList json of
        Left moves -> throwE moves
        Right moves -> do
            enemyMoveResult <- lift $ checkEnemyMove moves
            myMove <- lift makeMove
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

makeMove :: State GameState String
makeMove = state $ \gameState -> do
    let guesses = myGuesses gameState
    let (index, newGen) = randomR (0, length guesses - 1) $ randomGen gameState
    let guess = guesses !! index
    (guess, gameState { myGuesses = delete guess guesses, randomGen = newGen })


-- makeMove :: ExceptT String (State GameState) String
-- makeMove = state $ \gameState -> 
--     if myGuesses gameState null then throwE "Already shot at all squares"
--         else return ("A1", gameState)
