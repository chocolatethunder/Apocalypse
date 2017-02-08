module AI.Offensive where 

import ApocTools
import Lib.Language
import Lib.Functions

aiOffensive :: Chooser
aiOffensive currBoard Normal playerType = return (Just [(2,0),(2,1)])