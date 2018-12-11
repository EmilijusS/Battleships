module BattleshipTypes 
    ( SquareState(..)
    , MyShips
    , MyGuesses
    , GameState(..)
    ) where 

data SquareState = HIT | MISS | EMPTY deriving (Show, Eq)  

type MyShips = [String]
type MyGuesses = [String]
data GameState = GameState { myShips :: MyShips
                           , myGuesses :: MyGuesses
                           } deriving (Show, Eq) 
