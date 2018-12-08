module Move 
    ( move
    ) where 

move :: String -> Either String (Maybe [String])
move x = do
    moves <- getMoveList x
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

--Returns a list of moves made (and validates JSON in process)
getMoveList :: String -> Either String [String]
getMoveList x = do
    insides <- stripOuterBraces x
    stripped <- stripStart insides "\"coord\":{"
    coordString <- getUntilChar stripped '}'
    coords <- parseCoords coordString
    remaining <- getAfterChar stripped '}'
    prev <- getPrev remaining
    if prev == "null" then Right [coords] else (coords:) <$> getMoveList prev

-- Gets previous move and checks for errors
getPrev :: String -> Either String String
getPrev x = do
    resultCut <- stripStart x ",\"result\":"
    result <- getUntilChar resultCut ','
    remaining <- getAfterChar resultCut ','
    prev <- stripStart remaining "\"prev\":" 
    if result == "\"HIT\"" || result == "\"MISS\"" || (result == "null" && prev == "null")
        then Right prev else Left "Incorrect result or nulls where they shouldn't be"

-- Parses set for coordinates
parseCoords :: String -> Either String String
parseCoords ('\"':'1':'\"':':':'\"':letter:'\"':',':'\"':'2':'\"':':':'\"':digit:'\"':"")
    | elem digit ['1'..'9'] && elem letter ['A'..'J'] = Right $ letter : [digit]
    | otherwise = Left "Some digit or letter in coordinates is wrong"
parseCoords ('\"':'1':'\"':':':'\"':letter:'\"':',':'\"':'2':'\"':':':'\"':'1':'0':'\"':"")
    | letter `elem` ['A'..'J'] = Right $ letter : "10"
    | otherwise = Left "Some letter in coordinates is wrong"
parseCoords ('\"':'2':'\"':':':'\"':digit:'\"':',':'\"':'1':'\"':':':'\"':letter:'\"':"")
    | elem digit ['1'..'9'] && elem letter ['A'..'J'] = Right $ letter : [digit]
    | otherwise = Left "Some digit or letter in coordinates is wrong"
parseCoords ('\"':'2':'\"':':':'\"':'1':'0':'\"':',':'\"':'1':'\"':':':'\"':letter:'\"':"")
    | letter `elem` ['A'..'J'] = Right $ letter : "10"
    | otherwise = Left "Some letter in coordinates is wrong"
parseCoords "" = Right ""
parseCoords _ = Left "Coordinate format is wrong"

-- Gets content inside braces
stripOuterBraces :: String -> Either String String
stripOuterBraces x
    | head x == '{' && last x == '}' = Right $ init $ tail x 
    | otherwise = Left "Missing braces"

-- Checks if the beggining of the first string is the same as second string
-- and returns first string with that beggining cut off    
stripStart :: String -> String -> Either String String
stripStart x [] = Right x
stripStart [] _ = Left "Nothing left to strip"
stripStart (x1:y1) (x2:y2)
    | x1 == x2  = stripStart y1 y2 
    | otherwise = Left "Start doesn't match"

-- Cuts the beggining of the string until given character occurs    
getUntilChar :: String -> Char -> Either String String
getUntilChar [] _ = Left "Couldn't find character requested"
getUntilChar (x:t) c 
    | x == c    = Right [] 
    | otherwise = (x:) <$> getUntilChar t c

-- Cuts the ending of the string after given character occurs
getAfterChar :: String -> Char -> Either String String
getAfterChar [] _ = Left "Couldn't find character requested"
getAfterChar (x:t) c
    | x == c    = Right t
    | otherwise = getAfterChar t c
