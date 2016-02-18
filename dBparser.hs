-- Aranas, Michael June C.
-- De Bruijn Indices Parser
-- CSc 153

import Parsing
import Data.List

data Exp = Ind Int
	 | Apply Exp Exp
	 | Abs Exp
	   deriving Show

expdB :: Parser Exp
expdB = abst +++ apply +++ ind

ind :: Parser Exp
ind = (do t <- integer;  return (Ind t))

abst :: Parser Exp
abst = (do symbol "(\\."; f <- expdB; symbol ")"; return ((Abs f))) 

apply :: Parser Exp
apply = (do symbol "("; t <- expdB; e <- expdB; symbol ")"; return (Apply t e))

parsedB :: String -> Exp
parsedB str = case parse expdB str of
                           [(ast,[])] -> ast
                           [(_,out)]  -> error ("unused input " ++ out)
                           []         -> error "invalid input"
