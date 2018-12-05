module Move where

import Lib

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
getRemainingMoves (x:t) = 
    if x == ""
    then getRemainingMoves t
    else
        case getRemainingMoves t of
        Left r -> Left "There are two identical moves" 
        Right r -> removeFromList r x

-- Removes provided item from the list
removeFromList :: (Eq a) => [a] -> a -> Either String [a]
removeFromList [] _ = Left "There are two identical moves"
removeFromList (x:t) y = 
    if x == y
    then Right t
    else
        case removeFromList t y of
        Left a -> Left a 
        Right a -> Right $ x:a 

-- Determines whether the game has ended (and validates empty moves, because
-- that wasn't validated in getMoveList)
isGameEnd :: [String] -> Either String Bool
isGameEnd [] = Right False
isGameEnd [x] = Right $ x == ""
isGameEnd (x:t) = 
    if x == ""
    then Left "There exists an empty move which is not the last one!"
    else isGameEnd t

-- Splits a list into two on every second entry
splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList (x:y:t) = 
    let
        (a, b) = splitList t
    in
        (x:a, y:b)
splitList (x:t) = 
    let
        (a, b) = splitList t
    in
        (x:a, b)

--Returns a list of moves made (and validates JSON in process)
getMoveList :: String -> Either String [String]
getMoveList x =  
    let
        insides = stripOuterBraces x
        stripped = 
            case insides of
            Left r -> Left "Missing braces"
            Right r -> stripStart r "\"coord\":{"
        coordString = 
            case stripped of
            Left r -> Left "Missing ^\"coord\":{^"
            Right r -> getUntilChar r '}'
        coords = 
            case coordString of
            Left r -> Left "Missing closing brace"
            Right r -> parseCoords r
        remaining =
            case stripped of
            Left r -> Left "Missing ^\"coord\":{^" 
            Right r -> getAfterChar r '}'
        prev = 
            case remaining of
            Left r -> Left "Missing closing brace"
            Right r -> getPrev r
    in
        case prev of
        Left r -> Left r
        Right r ->
            case coords of
            Left c -> Left c
            Right c ->
                if r == "null"
                then Right [c]
                else 
                    case getMoveList r of
                    Left a -> Left a
                    Right a -> Right $ c:a 


-- Gets previous move and checks for errors
getPrev :: String -> Either String String
getPrev x = 
    let
        resultCut = stripStart x ",\"result\":"
        result = 
            case resultCut of
            Left r -> Left "Missing ^,\"result\":^"
            Right r -> getUntilChar r ','
        prev = 
            case resultCut of
            Left r -> Left "Missing ^,\"result\":^"
            Right r -> 
                case getAfterChar r ',' of
                -- I'm running out of letters :/
                Left a -> Left "Missing a comma"
                Right a -> stripStart a "\"prev\":"
    in
        case result of
        Left r -> Left r
        Right r -> 
            case prev of
            Left p -> Left "Missing ^\"prev\":^"
            Right p -> 
                if r == "\"HIT\"" || r == "\"MISS\"" || (r == "null" && p == "null")
                then Right p
                else Left "Incorrect result or nulls where they shouldn't be"

-- Parses set for coordinates
parseCoords :: String -> Either String String
parseCoords ('\"':'1':'\"':':':'\"':letter:'\"':',':'\"':'2':'\"':':':'\"':digit:'\"':"") =
    if elem digit ['1'..'9'] && elem letter ['A'..'J'] 
    then Right $ letter : [digit] 
    else Left "Some digit or letter in coordinates is wrong"
parseCoords ('\"':'1':'\"':':':'\"':letter:'\"':',':'\"':'2':'\"':':':'\"':'1':'0':'\"':"") =
    if letter `elem` ['A'..'J'] 
    then Right $ letter : "10"
    else Left "Some letter in coordinates is wrong"
parseCoords ('\"':'2':'\"':':':'\"':digit:'\"':',':'\"':'1':'\"':':':'\"':letter:'\"':"") =
    if elem digit ['1'..'9'] && elem letter ['A'..'J'] 
    then Right $ letter : [digit] 
    else Left "Some digit or letter in coordinates is wrong"
parseCoords ('\"':'2':'\"':':':'\"':'1':'0':'\"':',':'\"':'1':'\"':':':'\"':letter:'\"':"") =
    if letter `elem` ['A'..'J'] 
    then Right $ letter : "10"
    else Left "Some letter in coordinates is wrong"
parseCoords "" = Right ""
parseCoords _ = Left "Coordinate format is wrong"

-- Gets content inside braces
stripOuterBraces :: String -> Either String String
stripOuterBraces x = 
    if head x == '{' && last x == '}' 
    then Right $ init $ tail x 
    else Left "Missing braces"

-- Checks if the beggining of the first string is the same as second string
-- and returns first string with that beggining cut off    
stripStart :: String -> String -> Either String String
stripStart x [] = Right x
stripStart [] _ = Left "Nothing left to strip"
stripStart (x1:y1) (x2:y2) = 
    if x1 == x2 
    then stripStart y1 y2 
    else Left "Start doesn't match"

-- Cuts the beggining of the string until given character occurs    
getUntilChar :: String -> Char -> Either String String
getUntilChar [] _ = Left "Couldn't find character requested"
getUntilChar (x:t) c = 
    if x == c 
    then Right [] 
    else 
        case getUntilChar t c of
        Left result -> Left result
        Right result -> Right $ x:result

-- Cuts the ending of the string after given character occurs
getAfterChar :: String -> Char -> Either String String
getAfterChar [] _ = Left "Couldn't find character requested"
getAfterChar (x:t) c =
    if x == c
    then Right t
    else getAfterChar t c
