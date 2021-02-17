module VirtualMachine where {- Maycon Amaro, 2021 -}

import Regex ( Regex(..), fout )

{- VM Definition (Russ Cox, 2009) -}

data Instr
    = IChar Char
    | IMatch
    | IJump Int
    | ISplit Int Int
    deriving (Eq, Show)

type Program = [Instr]

data VMState = VMState {
      pc :: Int
    , input :: String
    , prog :: Program
    } deriving (Eq, Show)

{- Compile Regex into VM (Maycon Amaro) 
   This was the first successful attempt, I'll look if I can simplify/optimize later -}

increaseAddress :: Int -> Instr -> Instr
increaseAddress n (IJump x)    = IJump (x + n)
increaseAddress n (ISplit x y) = ISplit (x + n) (y + n)
increaseAddress _ i            = i

concatCodes :: Program -> Program -> Program
concatCodes p1 p2 = p1 ++ p2'
    where
        p2' = map (increaseAddress (length  p1)) p2

compiledCode :: Regex -> Program
compiledCode = prog . compile'

compile :: Regex -> VMState
compile e = virtualMachine { prog = prog virtualMachine ++ [IMatch] } 
    where
        virtualMachine = (compile' . fout) e
    
compile' :: Regex -> VMState
compile' Empty          = VMState (-1) "" [] -- always fail
compile' Lambda         = VMState 0 "" [] -- does nothing
compile' (Const a)      = VMState 0 "" [IChar a]
compile' (Concat e1 e2) = VMState p "" (compiledCode e1 `concatCodes` compiledCode e2)
    where
        p = (pc . compile') e1 `min` (pc . compile') e2
compile' (Sum e1 e2)    = VMState p "" splitCode
    where
        xCode = map (increaseAddress 1) (compiledCode e1)
        yCode = compiledCode e2
        finalYJump = length xCode + length yCode + 2
        splitCode = ((ISplit 1 (length xCode + 2) : xCode) ++ [IJump finalYJump]) `concatCodes` yCode  
        p = (pc . compile') e1 `max` (pc . compile') e2
compile' (Kleene e)     = VMState 0 "" kleeneCode
    where
        eCode = map (increaseAddress 1) (compiledCode e)
        kleeneCode = (ISplit 1 (length eCode + 2) : eCode) ++ [IJump 0]


{- Backtracking implementation for VM. Adapted by Maycon Amaro from (Russ Cox, 2009) C/C++ code -}

data VMStepOutput 
    = Continue VMState 
    | Thread (VMState, VMState) 
    | HaltSuccess 
    | HaltFail
    deriving (Eq, Show)

data VMOutput 
    = Success 
    | Fail
    deriving (Eq, Show)

step :: VMState -> Instr -> VMStepOutput
step (VMState pc' xs prog') ins = case ins of
    IChar c      -> if (not . null) xs && head xs == c
                    then Continue (VMState (pc' + 1) (tail xs) prog') 
                    else HaltFail
    IMatch       -> if null xs then HaltSuccess else HaltFail
    IJump p      -> Continue (VMState p xs prog')
    ISplit p1 p2 -> Thread (VMState p1 xs prog', VMState p2 xs prog')

run :: VMState -> VMOutput
run vm = if counter >= instrSize || counter < 0
            then Fail 
            else case step vm ins of
                    Continue vm'      -> run vm'
                    Thread (vm1, vm2) -> case run vm1 of
                                            Success -> Success
                                            Fail    -> run vm2 
                    HaltFail          -> Fail
                    HaltSuccess       -> Success
    where
        ins = prog vm !! pc vm
        counter = pc vm
        instrSize = (length . prog) vm

match :: String -> Regex -> Bool
match s e = run vm == Success 
    where 
        vm = (compile e) {input = s}