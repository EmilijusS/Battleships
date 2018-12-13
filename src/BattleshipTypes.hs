module BattleshipTypes 
    ( SquareState(..)
    , MyShips
    , MyGuesses
    , GameState(..)
    ) where 

import System.Random

data SquareState = HIT | MISS | EMPTY | NULL deriving (Show, Eq)  

type MyShips = [String]
type MyGuesses = [String]
data GameState = GameState { myShips :: MyShips
                           , myGuesses :: MyGuesses
                           , randomGen :: StdGen
                           } deriving (Show) 
