module Regex where {- Maycon Amaro, 2021 -}


{- Regular Expression Definition -}

data Regex = Empty | Lambda | Const Char | Concat Regex Regex | Sum Regex Regex | Kleene Regex
    deriving (Eq, Show)
  

{- Dealing with problematic regexes (Medeiros et al, 2012) -}

empty :: Regex -> Bool 
empty Empty = False -- (Delfino, 2019)
empty Lambda = True
empty (Const _) = False
empty (Kleene e) = empty e
empty (Concat e1 e2) = empty e1 && empty e2
empty (Sum e1 e2) = empty e1 && empty e2

null' :: Regex -> Bool
null' Empty = False -- (Delfino, 2019)
null' Lambda = True
null' (Const _) = False
null' (Kleene _) = True
null' (Concat e1 e2) = null' e1 && null' e2
null' (Sum e1 e2) = null' e1 || null' e2

fout :: Regex -> Regex
fout (Concat e1 e2) = Concat (fout e1) (fout e2)
fout (Sum e1 e2) = Sum (fout e1) (fout e2)
fout (Kleene e)
    | (not . null') e = Kleene (fout e)
    | empty e = Lambda
    | otherwise = Kleene (fin e)
fout e = e -- empty case only in (Delfino, 2019)

fin :: Regex -> Regex
fin (Concat e1 e2) = fin (Sum e1 e2)
fin (Sum e1 e2)
    | empty e1 && null' e2 = fin e2
    | empty e1 && (not . null') e2 = fout e2
    | null' e1 && empty e2 = fin e1
    | (not . null') e1 && empty e2 = fout e1
    | (not . null') e1 && (not . empty) e2 = Sum (fout e1) (fin e2)
    | (not . empty) e1 && (not . null') e2 = Sum (fin e1) (fout e2)
    | otherwise = Sum (fin e1) (fin e2)
fin (Kleene e)
    | null' e = fin e
    | otherwise = fout e
fin e = e -- I'm putting it so the patterns are exaustive

-- decide if regex is problematic, (Maycon Amaro)
problematic :: Regex -> Bool
problematic (Concat e1 e2) = problematic e1 || problematic e2
problematic (Sum e1 e2) = problematic e1 || problematic e2
problematic (Kleene e)
    | (not . null') e = problematic e
    | empty e = False 
    | otherwise = True
problematic _ = False