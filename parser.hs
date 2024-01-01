module Parser (Token(..),Aexp(..),Bexp(..),
lexer,parseIntOrParenthesis,parseAddOrProdOrInt,parseProdOrInt,parseSubOrAddOrProdOrInt,parseAexp) where
import Data.Char
import Data.Type.Bool (If)


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
    | WhileTok
    | AndTok
    | NotTok
    | AssignTok
    | TrueTok
    | FalseTok
    | DEqTok
    deriving (Show)

data Aexp =
  I Integer           -- constant
  | VarExp String        -- variables
  | AddExp Aexp Aexp     -- addition
  | MultExp Aexp Aexp    -- multiplication
  | SubExp Aexp Aexp    -- subtraction
  deriving Show

data Bexp =
    BTrue              -- true constant
    | BFalse           -- false constant
    | EqExp Aexp Aexp     -- equality test
    | LeExp Aexp Aexp     -- less than or equal to
    | NotExp Bexp         -- logical negation
    | AndExp Bexp Bexp    -- logical and
    deriving Show

data Stm
  = Assign String Aexp    -- Assignment
  | Seq [Stm]             -- Sequence of statements
  | If Bexp [Stm] [Stm]   -- If-then-else statement
  | While Bexp [Stm]      -- While loop
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('=' : '=' : restStr) = DEqTok : lexer restStr
lexer ('=' : restStr) = EqTok : lexer restStr
lexer (':' : '=' : restStr) = AssignTok : lexer restStr
lexer ('<' : '=' : restStr) = LeTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('i' : 'f' : restStr) = IfTok : lexer restStr
lexer ('w' : 'h': 'i' : 'l' : 'e' : restStr) = WhileTok : lexer restStr
lexer ('a' : 'n' : 'd' : restStr) = AndTok : lexer restStr
lexer ('n' : 'o' : 't' : restStr) = NotTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr) = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr
lexer (chr : restStr)
    | isSpace chr = lexer restStr
    | isAlpha chr = VarTok (takeWhile isAlpha (chr : restStr)) : lexer (dropWhile isAlpha restStr)
    | isDigit chr = let (number, rest) = span isDigit (chr : restStr) in IntTok (read number) : lexer rest
lexer (unexpectedChar : _) = error ("unexpected character: " ++ show unexpectedChar)

parseIntOrParenthesis :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenthesis (IntTok n : restTokens) = Just (I n, restTokens)
parseIntOrParenthesis (VarTok var : restTokens) = Just (VarExp var, restTokens)
parseIntOrParenthesis (OpenTok : tokens) =
    case parseAexp tokens of
        Just (aexp, CloseTok : restTokens) -> Just (aexp, restTokens)
        _ -> Nothing
parseIntOrParenthesis _ = Nothing

parseAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseAddOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, PlusTok : restTokens) ->
            case parseAddOrProdOrInt restTokens of
                Just (aexp2, restTokens') -> Just (AddExp aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
    case parseIntOrParenthesis tokens of
        Just (aexp1, TimesTok : restTokens) ->
            case parseProdOrInt restTokens of
                Just (aexp2, restTokens') -> Just (MultExp aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseSubOrAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSubOrAddOrProdOrInt tokens =
    case parseAddOrProdOrInt tokens of
        Just (aexp1, MinusTok : restTokens) ->
            case parseSubOrAddOrProdOrInt restTokens of
                Just (aexp2, restTokens') -> Just (SubExp aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp = parseSubOrAddOrProdOrInt


