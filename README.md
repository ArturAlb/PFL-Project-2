# Project 2
>       Group: T11_G07

>   (50%) up202006479 - Ruben Silverio Fernandes Esteves

>   (50%) up202108663 - Artur Jose Albuquerque Oliveira

## Execution

The code can be executed ran under GHCi version 9.0
or later and by using the following commands:

```bash
#compile
ghc --make main.hs

#run
./main
```

## Project summary

This project is able to take a program in the form of a string, for example:
```
"i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
```
And run it, returning the result:

```
("","fact=3628800,i=1")
```

## Project description

The program starts by using the fucntion lexer to turn the input string into a list of tokens:

```haskell
lexer :: String -> [Token]
```

```haskell
data Token = PlusTok
    | MinusTok
    | TimesTok
    | OpenTok
    | CloseTok
    | IntTok Integer
    | EqTok
    | LeTok
    | VarTok String
    | IfTok
    | ThenTok
    | ElseTok
    | WhileTok
    | AndTok
    | NotTok
    | AssignTok
    | TrueTok
    | FalseTok
    | DEqTok
    | SemiColonTok
    | DoTok
    deriving (Show)
```
Tokens are used to better identify the components of the string and are then used by our parse functions to be turned into Aexp (Arithmetic expressions), Bexp (Boolean expressions) or Stm (Statements).

Inside our Aexp we put everything that is related to arithmetic from integers to variables and of course every operation available.

We followed the same idea for the Bexp separating the False and True for easier use when compiling and restricting the boolean operators to be associated with the values they needed, for example, Eq which is the "=" is restricted to only have Bexp as parameters since its job was to only compare booleans. We applied that same logic to every other operator.

Finally, for the Stm data we added the if and while which required both Bexp and a list of statements and the Assign that represents the assignement of Aexp, Integers or arithmetic operations, to a variable represented by the string.

```haskell
-- Arithmetic expressions
data Aexp =
  I Integer           -- constant
  | VarExp String        -- variables
  | AddExp Aexp Aexp     -- addition
  | MultExp Aexp Aexp    -- multiplication
  | SubExp Aexp Aexp    -- subtraction
  deriving Show

-- Boolean expressions
data Bexp =
     BTrue              -- true constant
    | BFalse           -- false constant
    | IEqExp Aexp Aexp    -- integer equality test
    | EqExp Bexp Bexp     -- equality test
    | LeExp Aexp Aexp     -- less than or equal to
    | NotExp Bexp         -- logical negation
    | AndExp Bexp Bexp    -- logical and
    deriving Show

-- Program as a list of statements
type Program = [Stm]

-- Statements
data Stm
  = Assign String Aexp    -- Assignment
  | If Bexp Program Program   -- If-then-else statement
  | While Bexp Program      -- While loop
  deriving Show
```

Our compiler functions (compile, compA and compB) are then responsible for turning those statements and expressions into Code:

```haskell
{- Functions to compile the program ------------------------------------------------}
compile :: Program -> Code
compile [] = []
compile (Assign var aexp : rest) = compA aexp ++ [Store var] ++ compile rest
compile (If bexp p1 p2 : rest) = compB bexp ++ [Branch (compile p1) (compile p2)] ++ compile rest
compile (While bexp p : rest) = Loop (compB bexp) (compile p) : compile rest

-- Compile arithmetic expressions
compA :: Aexp -> Code
compA (VarExp x) = [Fetch x]
compA (I n) = [Push n]
compA (AddExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Add]
compA (SubExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (MultExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Mult]

-- Compile boolean expressions
compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (IEqExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Equ]
compB (EqExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [Equ]
compB (LeExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (NotExp bexp) = compB bexp ++ [Neg]
compB (AndExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]
```