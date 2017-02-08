module AI.Offensive where 

import ApocTools
import Lib.Language
import Lib.Functions

aiOffensive :: Chooser
aiOffensive currBoard Normal playerType = return (Just [(2,0),(2,1)])

--Check playerType
  --if BLACK
    --if (BK == 0)
      --if possiblePawnCapture
        --select random possiblePawnCapture
        --RETURN
      --else no possiblePawnCapture
        --select random WP movement
        --RETURN
    --else (BK != 0)
      --if possibleKnightCapture
        --select random possibleKnightCapture
        --RETURN
      --else no possibleKnightCapture
        --if possiblePawnCapture
          --select random possiblePawnCapture
          --RETURN
        --else no possiblePawnCapture
          --select random WP movement
          --RETURN
          
  --else WHITE
