import Test.HUnit
import Control.Monad.State.Lazy
import System.Random

import BattleshipTypes
import PlayBattleships

main :: IO ()
main = do
    runTestTT tests
    return ()

getState :: GameState
getState = GameState { myShips = ["A4", "B10", "C9"] 
                     , myGuesses = [x : show y | x <- ['A'..'J'], y <- [1..10]]
                     , randomGen = mkStdGen 1 } 

testCheckEnemyMoveNull = TestCase (assertEqual "When there are no moves" NULL $ evalState (checkEnemyMove []) getState)
testCheckEnemyMoveHit = TestCase (assertEqual "When myShips contains enemy move" HIT $ evalState (checkEnemyMove ["B10", "C4"]) getState)
testCheckEnemyMoveMiss = TestCase (assertEqual "When myShips doesn't contain enemy move" MISS $ evalState (checkEnemyMove ["C4", "B10"]) getState)
testCheckEnemyMoveEmpty = TestCase (assertEqual "When incoming move is empty" EMPTY $ evalState (checkEnemyMove ["","C4", "B10"]) getState)

tests = TestList [ TestLabel "checkEnemyMove Null" testCheckEnemyMoveNull
                 , TestLabel "checkEnemyMove Hit" testCheckEnemyMoveHit
                 , TestLabel "checkEnemyMove Miss" testCheckEnemyMoveMiss
                 , TestLabel "checkEnemyMove Empty" testCheckEnemyMoveEmpty
                 ]
