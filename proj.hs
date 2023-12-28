-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

import Data.List
import Stack

-- Do not modify our definition of Inst and Code
data Inst =
    Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
    Branch Code Code | Loop Code Code
    deriving Show
type Code = [Inst]

tt :: Integer
tt = 1

ff :: Integer
ff = 0

le :: Integer -> Integer -> Integer
le x y = if x <= y then tt else ff


fetch :: String -> State -> Stack Integer-> Stack Integer
fetch x state stack =
    case lookup x state of
        Just value -> (value : stack)
        Nothing -> error $ "Variable " ++ x ++ " not found"

store :: String -> State -> Stack Integer-> State
store x state [] = error "Stack is empty"
store x state (n:stack) = ((x, n) : state)
{-
branch :: Code -> Code -> State -> Stack -> (State, Stack, Code)
branch c1 c2 state [] = error "Stack is empty"
branch c1 c2 state (n:stack)
    | n == tt = (state, stack, Just c1)
    | n == ff = (state, stack, Just c2)
    | otherwise = error "Top of stack is not a boolean value"
-}

loop :: (Code, Code) -> Code
loop (c1, c2) = [c1 : Branch [c2, Loop (c1, c2)] : Noop]

createEmptyStack :: Stack Integer
createEmptyStack = []

stack2Str :: Stack Integer -> String
stack2Str stack = concatMap show (reverse stack)

type State = [(String, Integer)]

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," $ map (\(var, val) -> var ++ "=" ++ show val) (sort state)
    where sort = sortBy (compare `on` fst)

run :: (Code, Stack Integer, State) -> (Code, Stack Integer, State)
run (instructions, stack, [states]) = runAux (instructions, stack, [states])
runAux :: (Code, Stack Integer, State) -> (Code, Stack Integer, State)
runAux ([], stack, state) = ([], stack, state)
runAux ( Push x :instructions, stack, state) = runAux (instructions, push x stack, state)
runAux ( Tru :instructions, stack, state) = runAux (instructions, push tt stack, state)
runAux ( Fals :instructions, stack, state) = runAux (instructions, push ff stack, state)
runAux ( Add :instructions, stack, state) = runAux (instructions, push (top stack + top (pop stack)) (pop (pop stack)), state)
runAux ( Mult :instructions, stack, state) = runAux (instructions, push (top stack * top (pop stack)) (pop (pop stack)), state)
runAux ( Sub :instructions, stack, state) = runAux (instructions, push (top stack - top (pop stack)) (pop (pop stack)), state)
runAux ( Equ :instructions, stack, state) = runAux (instructions, push (if top stack == top (pop stack) then tt else ff) (pop (pop stack)), state)
runAux ( Le :instructions, stack, state) = runAux (instructions, push (le top stack top (pop stack)) (pop (pop stack)), state)
runAux ( Fetch x :instructions, stack, state) = runAux (instructions, fetch x state stack, state)
runAux ( Store x :instructions, stack, state) = runAux (instructions, pop stack, store x state stack)
runAux ( Branch c1 c2 :instructions, stack, state) = runAux(if top stack == tt then runAux(c1,stack,state) else runAux(c2,stack,state))
runAux ( Loop c1 c2 :instructions, stack, state) = runAux(c1,stack,state)
runAux ( Noop :instructions, stack, state) = runAux (instructions, stack, state)

{- guardar


runAux ([instruction:instructions, stack, state)
  | instruction == Push x = runAux (instructions, push x stack, state)
  | instruction == Tru = runAux (instructions, push tt stack, state)
  | instruction == Fals = runAux (instructions, push ff stack, state)
  | instruction == Add = runAux (instructions, push (top stack + top (pop stack)) (pop (pop stack)), state)
  | instruction == Mult = runAux (instructions, push (top stack * top (pop stack)) (pop (pop stack)), state)
  | instruction == Sub = runAux (instructions, push (top stack - top (pop stack)) (pop (pop stack)), state)
  | instruction == Equ = runAux (instructions, push (if top stack == top (pop stack) then tt else ff) (pop (pop stack)), state)
  | instruction == Le = runAux (instructions, push (le top stack top (pop stack)) (pop (pop stack)), state)
  | instruction == Fetch x = runAux (instructions, fetch x state stack, state)
  | instruction == Store x = runAux (instructions, pop stack, store x state stack)
  | instruction == Branch (c1,c2) = runAux(if top stack == tt then runAux(c1,stack,state) else runAux(c2,stack,state))
  | instruction == Loop (c1,c2) = runAux(c1,stack,state)
  | instruction == Noop = runAux (instructions, stack, state)

-}


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

main :: IO ()
main = do
  print (testAssembler [Push 1,Push 2,Add,Store "x"] == ("x","3"))
    