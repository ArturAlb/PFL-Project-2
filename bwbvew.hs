data Aexp
  = Var String            -- Variable
  | Num Integer           -- Integer literal
  | AAdd Aexp Aexp        -- Addition
  | ASub Aexp Aexp        -- Subtraction
  | AMul Aexp Aexp        -- Multiplication
  deriving Show

data Bexp
  = BTrue                 -- True constant
  | BFalse                -- False constant
  | Eq Aexp Aexp          -- Equality
  | BLe Aexp Aexp          -- Less than or equal to
  | Not Bexp              -- Logical negation
  deriving Show

data Stm
  = Assign String Aexp    -- Assignment
  | Seq [Stm]             -- Sequence of statements
  | If Bexp [Stm] [Stm]   -- If-then-else statement
  | While Bexp [Stm]      -- While loop
  deriving Show

type Program = [Stm]

-- Compiler functions
compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (AAdd a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (ASub a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (AMul a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (BLe a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Not b) = compB b ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq s1 -> compile s1 ++ compile rest
  If cond thenStm elseStm -> compB cond ++ [Branch (compile thenStm) (compile elseStm)] ++ compile rest
  While cond body -> [Loop (compB cond) (compile body)] ++ compile rest


lexer :: String -> [String]
lexer "" = []
lexer ('w':'h':'i':'l':'e':rest) = "while" : lexer rest
lexer ('i':'f':rest) = "if" : lexer rest
lexer ('t':'h':'e':'n':rest) = "then" : lexer rest
lexer ('e':'l':'s':'e':rest) = "else" : lexer rest
lexer ('*':rest) = "*" : lexer rest
lexer ('+':rest) = "+" : lexer rest
lexer ('/':rest) = "/" : lexer rest
lexer ('-':rest) = "-" : lexer rest
lexer (';':rest) = ";" : lexer rest
lexer ('(':rest) = "(" : lexer rest
lexer (')':rest) = ")" : lexer rest
lexer ('<':'=':rest) = "<=" : lexer rest
lexer ('=':'=':rest) = "==" : lexer rest
lexer ('n':'o':'t':rest) = "not" : lexer rest
lexer ('=':rest) = "=" : lexer rest
lexer ('a':'n':'d':rest) = "and" : lexer rest
lexer (':':'=':rest) = ":=" : lexer rest
lexer ('d':'o':rest) = "do" : lexer rest
lexer (' ':rest) = lexer rest
lexer (a:rest) = lexeracc (a:rest) []

lexeracc :: String -> String -> [String]
lexeracc "" stracc | stracc == "" = []
                   | otherwise = [stracc]
lexeracc (' ':rest) stracc | stracc == "" = lexer rest
                          | otherwise = stracc : lexer rest
lexeracc (a:rest) stracc = lexeracc rest (stracc ++ [a])


-- Parser for integers
parseInt :: String -> (Integer, String)
parseInt s = (read numStr, rest)
  where
    (numStr, rest) = span isDigit s

-- Parser for variable names
parseVar :: [String] -> (String, [String])
parseVar (c:cs) | all isAlpha c = (c, cs)
                | otherwise = error "Invalid variable name"
parseVar [] = error "Unexpected end of input"

-- Parser for arithmetic expressions
parseAexp :: [String] -> (Aexp, [String])
parseAexp ("(":rest) = trace ("Parsing in parentheses: " ++ show rest) (parseAexpInParens rest)
parseAexp (c:rest)
  | all isDigit c = trace ("Parsed Num: " ++ show (read c :: Integer)) (Num (read c), rest)
  | all isAlpha c = trace ("Parsed Var: " ++ c) (Var c, rest)
  | c == "-" = case parseAexp rest of
                 (a1, rest') -> trace ("Parsed Unary Minus: " ++ show (ASub (Num 0) a1)) (ASub (Num 0) a1, rest')
  | otherwise = error "Invalid arithmetic expression"
parseAexp _ = error "Invalid arithmetic expression"

parseAexpInParens :: [String] -> (Aexp, [String])
parseAexpInParens s = case parseAexp s of
  (aexp, rest) -> trace ("Parsed expression in parentheses: " ++ show aexp) (aexp, rest)
  _ -> error "Mismatched parentheses in arithmetic expression"

-- Parser for boolean expressions
parseBexp :: [String] -> (Bexp, [String])
parseBexp ("True":rest) = (BTrue, rest)
parseBexp ("False":rest) = (BFalse, rest)
parseBexp ("not":rest) = case parseBexp rest of
  (bexp, after) -> (Not bexp, after)
parseBexp ("(":rest) = parseBexpInParens rest
parseBexp _ = error "Invalid boolean expression"

parseBexpInParens :: [String] -> (Bexp, [String])
parseBexpInParens s = case parseBexp s of
  (bexp, ")":rest) -> (bexp, rest)
  _ -> error "Mismatched parentheses in boolean expression"

-- Parser for statements
parseStm :: [String] -> (Stm, [String])
parseStm ("if":rest) = parseIf rest
parseStm ("while":rest) = parseWhile rest
parseStm (var:":=":rest) = case parseAexp rest of
  (expr, after) -> (Assign var expr, after)
parseStm _ = error "Invalid statement"

-- Parser for assignment statements
parseAssignment :: [String] -> (Stm, [String])
parseAssignment s = case parseVar s of
  (var, ":=":rest) ->
    case parseAexp rest of
      (expr, after) -> (Assign var expr, after)
  _ -> error "Invalid assignment statement"

parseSequence :: [String] -> ([Stm], [String])
parseSequence s = case parseStm s of
  (stmt, rest) -> case rest of
    (";":rest') -> case parseSequence rest' of
      (stmts, rest'') -> (stmt : stmts, rest'')
    _ -> ([stmt], rest)

parseIf :: [String] -> (Stm, [String])
parseIf s =
  case parseBexp s of
    (cond, rest) ->
      case parseStm rest of
        (thenBranch, rest') ->
          case rest' of
            ("else":rest'') ->
              case parseStm rest'' of
                (elseBranch, rest''') -> (If cond [thenBranch] [elseBranch], rest''')
            _ -> error "Invalid if-then-else statement"
        -- _ -> error "Invalid if-then-else statement"

parseWhile :: [String] -> (Stm, [String])
parseWhile s =
  case parseBexp s of
    (cond, rest) ->
      case parseStm rest of
        (body, rest') -> (While cond [body], rest')

-- Parser for the entire program
-- parseProgram :: [String] -> (Program, [String])
-- parseProgram s = parseSequence s

parse :: String -> Program
parse input =
  case parseSequence (lexer input) of
    (program, remaining) ->
      if null remaining
        then program
        else error $ "Parsing error. Remaining input: " ++ unwords remaining