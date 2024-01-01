-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.List
import Stack
import Data.Foldable
import Data.Function
import Data.Text
import Parser

-- Do not modify our definition of Inst and Code
data Inst =
    Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
    Branch Code Code | Loop Code Code
type Code = [Inst]
tt :: Bool
tt = True

ff :: Bool
ff = False

topInt :: Stack -> Integer
topInt stack = case top stack of
    IntElem value -> value
    _ -> error "Run-time error"

topBool :: Stack -> Bool
topBool stack = case top stack of
    BoolElem value -> value
    _ -> error "Run-time error"

compStackElem :: Stack -> Bool
compStackElem stack = case (top stack, top (pop stack)) of
    (IntElem x, IntElem y) -> if x == y then tt else ff
    (BoolElem x, BoolElem y) -> if x == y then tt else ff
    _ -> error "Run-time error"


le :: Integer -> Integer -> Bool
le x y = if x <= y then tt else ff

fetch :: String -> State -> Stack-> Stack
fetch x state stack =
    case lookup x state of
        Just value -> push value stack
        Nothing -> error "Run-time error"

store :: String -> State -> Stack -> State
store x state stack = (x, top stack) : deleteBy (\(k1, _) (k2, _) -> k1 == k2) (x, undefined) state


loop :: (Code, Code) -> Code
loop (c1, c2) = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]


createEmptyStack :: Stack
createEmptyStack = Stack.empty

stack2Str :: Stack -> String
stack2Str (Stk xs) = Data.List.intercalate "," (Data.List.map showStackElem xs)

showStackElem :: StackElem -> String
showStackElem (IntElem i) = show i
showStackElem (BoolElem b) = show b

type State = [(String, StackElem)]

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = Data.List.intercalate "," $ Data.List.map (\(var, val) -> var ++ "=" ++ showStackElem val) (sort state)
    where sort = sortBy (compare `on` fst)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, states) = ([], stack, states)
run ( Push x :instructions, stack, state) = run (instructions, push (IntElem x) stack, state)
run ( Tru :instructions, stack, state) = run (instructions, push (BoolElem tt) stack, state)
run ( Fals :instructions, stack, state) = run (instructions, push (BoolElem ff) stack, state)
run ( Add :instructions, stack, state) = run (instructions, push (IntElem (topInt stack + topInt (pop stack))) (pop (pop stack)), state)
run ( Mult :instructions, stack, state) = run (instructions, push (IntElem (topInt stack * topInt (pop stack))) (pop (pop stack)), state)
run ( Sub :instructions, stack, state) = run (instructions, push (IntElem (topInt stack - topInt (pop stack))) (pop (pop stack)), state)
run ( Equ :instructions, stack, state) = run (instructions, push (BoolElem (compStackElem stack)) (pop (pop stack)), state)
run ( Le :instructions, stack, state) = run (instructions, push (BoolElem (le (topInt stack) (topInt (pop stack)))) (pop (pop stack)), state)
run ( Fetch x :instructions, stack, state) = run (instructions, fetch x state stack, state)
run ( Store x :instructions, stack, state) = run (instructions, pop stack, store x state stack)
run ( Branch c1 c2 :instructions, stack, state) = if topBool stack == tt then run (c1 ++ instructions, pop stack,state) else run (c2 ++ instructions, pop stack,state)
run ( Loop c1 c2 :instructions, stack, state) = run (loop (c1,c2) ++ instructions,stack,state)
run ( Noop :instructions, stack, state) = run (instructions, stack, state)
run ( Neg :instructions, stack, state) = run (instructions, push (BoolElem (not (topBool stack))) (pop stack), state)
run _ = error "Run-time error"




-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

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

{-parseCode :: String -> Code
parseCode str = case words str of
    [var, ":=", val] -> [Push (read val), Store var]
    [val1, "+", val] -> [Push (read val1), Push (read val), Add]
    [val1, "+", val] -> [Push (read val1), Fetch val, Add]
    [val1, "-", val] -> [Push (read val), Push (read val1), Sub]
    [val1, "*", val] -> [Push (read val1), Push (read val), Mult]
    _ -> error "Invalid input"-}

compA :: Aexp -> Code
compA (VarExp x) = [Fetch x]
compA (I n) = [Push n]
compA (AddExp aexp1 aexp2) = (compA aexp1) ++ (compA aexp2) ++ [Add]
compA (SubExp aexp1 aexp2) = (compA aexp2) ++ (compA aexp1) ++ [Sub]
compA (MultExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Mult]

compB :: Bexp -> Code
compB (BTrue) = [Tru]
compB (BFalse) = [Fals]
compB (IEqExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Equ]
compB (EqExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [Equ]
compB (LeExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Le]
compB (NotExp bexp) = compB bexp ++ [Neg]
compB (AndExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]

compile :: Program -> Code
compile [] = []
compile (Assign var aexp : rest) = (compA aexp) ++ [Store var] ++ compile rest
compile (If bexp p1 p2 : rest) = (compB bexp) ++ [Branch (compile p2) (compile p1)] ++ compile rest
compile (While bexp p : rest) = (compB bexp) ++ [Loop (compile p) (compB bexp)] ++ compile rest

parse :: String -> Program
parse text = extractStm (parseStms (lexer text))

extractStm :: Maybe ([Stm], [Token]) -> Program
extractStm (Just (want, dontWant)) = want

--parseIf :: [String] -> Code -> (String, Code)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

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
    print( testParser "x := 0 - 2;" == ("","x=-2"))
    print( testParser "x := 5; x := x - 1;" == ("","x=4"))
    --print( testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
    print( testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
    print( testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
    print( testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
    print( testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
    print( testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
    print( testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
    print( testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
    print( testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
    print( testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))