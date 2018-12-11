import Test.HUnit
import Control.Monad.State.Lazy

import BattleshipTypes
import PlayBattleships

main :: IO ()
main = do
    runTestTT tests
    return ()

getState :: GameState
getState = GameState ["A1", "B10", "C3"] []

testCheckEnemyMoveHit = TestCase (assertEqual "When myShips contains enemy move" HIT $ evalState (checkEnemyMove "B10") getState)
testCheckEnemyMoveMiss = TestCase (assertEqual "When myShips doesn't contain enemy move" MISS $ evalState (checkEnemyMove "C4") getState)

tests = TestList [ TestLabel "checkEnemyMove Hit" testCheckEnemyMoveHit
                 , TestLabel "checkEnemyMove Miss" testCheckEnemyMoveMiss
                 ]
