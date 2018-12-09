module Move 
    ( move
    ) where 

-- Returns next move from a list of both player moves
move :: [String] -> Either String (Maybe [String])
move moves = do
    end <- isGameEnd moves
    let playerMoves = splitList moves
    p1 <- getRemainingMoves $ fst playerMoves
    p2 <- getRemainingMoves $ snd playerMoves
    if end then Right Nothing else
        if null p1 && null p2 then Left "It's impossible for both players to have no moves" else
            if length p1 == length p2 then Right $ Just [[head (head p1)], tail (head p1)] else
                Right $ Just [[head (head p2)], tail (head p2)]

-- Gets a list of moves available for player at the given moment
getRemainingMoves :: [String] -> Either String [String]
getRemainingMoves [] = Right [x : show y | x <- ['A'..'J'], y <- [1..10]]
getRemainingMoves (x:t)
    | x == ""   = getRemainingMoves t
    | otherwise = getRemainingMoves t >>= removeFromList x 

-- Removes provided item from the list
removeFromList :: (Eq a) => a -> [a] -> Either String [a]
removeFromList _ [] = Left "There are two identical moves"
removeFromList y (x:t)
    | x == y    = Right t
    | otherwise = (x:) <$> removeFromList y t

-- Determines whether the game has ended (and validates empty moves, because
-- that wasn't validated in getMoveList)
isGameEnd :: [String] -> Either String Bool
isGameEnd [] = Right False
isGameEnd [x] = Right $ x == ""
isGameEnd (x:t)
    | x == ""   = Left "There exists an empty move which is not the last one!"
    | otherwise = isGameEnd t

-- Splits a list into two on every second entry
splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList (x:y:t) = (x:a, y:b)
    where (a, b) = splitList t
splitList (x:t) = (x:a, b)
    where (a, b) = splitList t

