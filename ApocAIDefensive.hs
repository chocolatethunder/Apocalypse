-- Defensive AI code

-- The defensive AI plays the game to protect home territory from invading enemy units.
-- The Defensive AI will scan the board from friendly to enemy territory. The friendly unit 
-- that first encounters an enemy unit in it's strike position will move in for the kill.

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
