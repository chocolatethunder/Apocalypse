-- module AI.Offensive (...) where

-- Offensive AI code

-- The offensive AI attacks the enemy on their territory and takes the kill. 
-- The Offensive AI will scan the board from enemy to friendly territory. The friendly unit 
-- that is the furthest into the enemy territory will scan for victims.

-- The pawn will check 2 positions for victims.
--  For White units (moving top to bottom):
--    (currX-1,currY+1), 
--    (currX+1,currY+1),
--  For Black units (moving bottom to top):
--    (currX-1,currY-1), 
--    (currX+1,currY-1),

-- The knight will check 8 positions 
--    (currX-2,currY-1), (currX-2,currY+1), 
--    (currX+2,currY-1), (currX+2,currY+1), 
--    (currX-1, currY-2), (currX+1,currY-2),
--    (currX+2,currY-1), (currX+2,currY+1)

-- If the move is valid and there is a target then the AI will take that move
