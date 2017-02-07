-- takes a GameState and a desired piece (eg BP, BK, WP, WK) and returns the count of that piece
-- for exampe getPieceCount curr_gamestate BP will return the number of black pawns                                                                                    
getPieceCount :: GameState -> Cell -> Int
getPieceCount curr_gamestate piece = do
                                let strBoard = board2Str (theBoard curr_gamestate)
                                length ( filter ( == (cell2Char piece)) strBoard)
                            
-- Human player prompt messages for the player move. Wording of prompt depends on PlayType
playerPromptMessage :: PlayType -> Player -> String
playerPromptMessage Normal Black = "Enter the move coordinates for player Black in the form 'srcX srcY destX destY'\n[0 >= n >= 4, or just enter return for a 'pass'] B2:"
playerPromptMessage PawnPlacement Black = "Enter the coordinates to place the pawn for player Black in the form 'destX destY':\n[0 >= n >= 4] B1:"
playerPromptMessage Normal White = "Enter the move coordinates for player White in the form 'srcX srcY destX destY'\n[0 >= n >= 4, or just enter return for a 'pass'] W2:"
playerPromptMessage PawnPlacement White = "Enter the coordinates to place the pawn for player White in the form 'destX destY':\n[0 >= n >= 4] W1:"
