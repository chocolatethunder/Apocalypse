isSet :: Eq a => [a] -> Bool
isSet [] = True
isSet (a:as) = if ((length as) == 0) then True else if (a == (head as)) then False else isSet as



toSet :: Eq a => [a] -> [a]
let y = []
toSet (a:as) = if (a /= (head as)) then (if (length as == 1) then y ++ a else toSet (tail as)) else toSet as

storage b a = 

-- elem' :: (Eq a) => a -> [a] -> Bool  
-- elem' a [] = False  
-- elem' a (x:xs)  
--    | a == x    = True  
--    | otherwise = a `elem'` xs

--elementOf :: (Eq a, Show a) => a -> [a] -> Either String Bool
--elementOf a [] = Right False
--elementOf a (x:xs)  
--    | a == x    = Right True  
--    | otherwise = a `elementOf` xs

--intersection :: Eq a => [a] -> [a] -> Maybe [a]

---------------**********************game_Assignment_CPSC449--------------------

-- Initial board stat:
-- START
let top =   " _ _ _ _ _ "
		
let field = "|_|_|_|_|_|"

getPiece x y = (gameboard !! x) !! y

let c = [top] ++ [ field | x <- [1..5]]
-- END

-- Moving piece:
-- START
play :: (a) -> String -> [c] -> [d]
play a b c = [((take ((fst a * 2)-1)  ( head (take 1 ( drop ( snd a ) c) ) ) ) ++ b ++ reverse(take (11-( fst a * 2)) (reverse ( head (take 1 ( drop ( snd a ) c) ) ) ) ))]

update :: (a) -> [b] -> [c] -> [d]
update a b c = reverse( drop ((length c) - (snd a)) (reverse c)) ++ b ++ (drop ((snd a)+1) c)
-- END

-- Turn-taking:
-- START

-- END

-- Clashes:
-- START

clash_check :: (a, a1) -> [Char] -> (b, b1) -> [Char] -> Bool
clash_check a a_c b b_c = if (a == b) then chess_check a_c b_c else "Continue the move"

chess_check :: [Char] -> [Char] -> maybe [Char]
chess_check a b = if ((a == "X" && b == "#") || (a == "/" && b == "+")) then remove_chess else decide_keep a b

remove_chess a b = if ()

decide_keep ::[Char] -> [Char] -> [Char]
decide_keep a b = if (a == "X") then a else b

-- END

-- Pawn missed capture:
-- START

-- END

-- Penalty:
-- START

-- END

-- Pawn promotion:
-- START

-- END

emp = "_"
wall = "|"
spc = " "
black_pwn = "/"
white_pwn = "+"
black_hor = "X"
white_hor = "#"

-- status of the game
let black_penalty = 0
let white_penalty = 0
let black_win = false
let white_win = false

-- the invalid move is made, the player will receive one penalty point

--game ends when one of the following condition is occur:
--if (null a && null b) then "Count which side has the most pawn(s)" else "continue the game"
--if (black_penalty >= 2 || white_penalty >= 2) then 
--

--merge (a:as) = if ((length as) > 0) then a ++ merge (as) else a

let top =   " _ _ _ _ _ "
board = "|X|/|/|/|X|"
		"|/|_|_|_|/|"
		"|_|_|_|_|_|"
		"|+|_|_|_|+|"
		"|#|+|+|+|#|"

let top =   " _ _ _ _ _ "
		
let field = "| | | | |"

let c = [top] ++ [ field | x <- [1..5]]

play :: (a) -> String -> [c] -> [d]
play a b c = [((take ((fst a * 2)-1)  ( head (take 1 ( drop ( snd a ) c) ) ) ) ++ b ++ reverse(take (11-( fst a * 2)) (reverse ( head (take 1 ( drop ( snd a ) c) ) ) ) ))]

update :: (a) -> [b] -> [c] -> [d]
update a b c = reverse( drop ((length c) - (snd a)) (reverse c)) ++ b ++ (drop ((snd a)+1) c)

init_chess a b c = update a b c

-- a (type: tuple) is the tuple that represent the coordinate of the chess
-- b (type: String) is type of chess (Horse or Pawn)
-- c (type: list of Strings) is the board or map
