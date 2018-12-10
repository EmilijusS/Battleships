import Test.HUnit
import Control.Monad.State.Lazy

import BattleshipTypes
import CheckEnemyMove

main :: IO ()
main = do
    runTestTT tests
    return ()

getState :: GameState
getState = GameState ["A1", "B10", "C3"] []

testCheckEnemyMoveHit = TestCase (assertEqual "When myShips contains enemy move" Hit $ evalState (checkEnemyMove "B10") getState)
testCheckEnemyMoveMiss = TestCase (assertEqual "When myShips doesn't contain enemy move" Miss $ evalState (checkEnemyMove "C4") getState)

tests = TestList [ TestLabel "checkEnemyMove Hit" testCheckEnemyMoveHit
                 , TestLabel "checkEnemyMove Miss" testCheckEnemyMoveMiss
                 ]
