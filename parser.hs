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
  | Var String        -- variables
  | Add Aexp Aexp     -- addition
  | Mult Aexp Aexp    -- multiplication
  | Sub Aexp Aexp    -- subtraction
  deriving Show

data Bexp =
     BTrue              -- true constant
    | BFalse           -- false constant
    | IEq Aexp Aexp     -- integer equality
    | Eq Bexp Bexp     -- equality test
    | Le Aexp Aexp     -- less than or equal to
    | Not Bexp         -- logical negation
    | And Bexp Bexp    -- logical and
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
lexer ('w' : 'h' : 'i' : 'l' : 'e' : restStr) = WhileTok : lexer restStr
lexer ('a' : 'n' : 'd' : restStr) = AndTok : lexer restStr
lexer ('n' : 'o' : 't' : restStr) = NotTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr) = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr
lexer (chr : restStr)
    | isSpace chr = lexer restStr
    | isAlpha chr = VarTok (takeWhile isAlpha (chr : restStr)) : lexer (dropWhile isAlpha restStr)
    | isDigit chr = let (number, rest) = span isDigit (chr : restStr) in IntTok (read number) : lexer rest
lexer (unexpectedChar : _) = error ("unexpected character: " ++ show unexpectedChar)

parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok n : restTokens) = Just (I n, restTokens)
parseInt (VarTok var : restTokens) = Just (Var var, restTokens)

parseIntOrParenthesis :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenthesis (IntTok n : restTokens) = Just (I n, restTokens)
parseIntOrParenthesis (VarTok var : restTokens) = Just (Var var, restTokens)
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
                Just (aexp2, restTokens') -> Just (Add aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
    case parseIntOrParenthesis tokens of
        Just (aexp1, TimesTok : restTokens) ->
            case parseProdOrInt restTokens of
                Just (aexp2, restTokens') -> Just (Mult aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseSubOrAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSubOrAddOrProdOrInt tokens =
    case parseAddOrProdOrInt tokens of
        Just (aexp1, MinusTok : restTokens) ->
            case parseSubOrAddOrProdOrInt restTokens of
                Just (aexp2, restTokens') -> Just (Sub aexp1 aexp2, restTokens')
                _ -> Nothing
        result -> result

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp = parseSubOrAddOrProdOrInt

parseTrueOrFalseOrIntOrParenthesis :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseTrueOrFalseOrIntOrParenthesis (TrueTok : restTokens) = Just (Right BTrue, restTokens)
parseTrueOrFalseOrIntOrParenthesis (FalseTok : restTokens) = Just (Right BFalse, restTokens)
parseTrueOrFalseOrIntOrParenthesis (IntTok n : restTokens) =
    case parseAexp (IntTok n : restTokens) of
        Just (aexp, restTokens') -> Just (Left aexp, restTokens')
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis (OpenTok : tokens) =
    case parseBexp tokens of
        Just (bexp, CloseTok : restTokens) -> Just (Right bexp, restTokens)
        _ -> Nothing

parseLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseLeOrValue tokens =
    case parseTrueOrFalseOrIntOrParenthesis tokens of
        Just (Left aexp1, LeTok : restTokens) ->
            case parseLeOrValue restTokens of
                Just (Left aexp2, restTokens') -> Just (Right (Le aexp1 aexp2), restTokens')
                _ -> Nothing
        result -> result

parseIeqOrLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseIeqOrLeOrValue tokens =
    case parseLeOrValue tokens of
        Just (Left aexp1, DEqTok : restTokens) ->
            case parseIeqOrLeOrValue restTokens of
                Just (Left aexp2, restTokens') -> Just (Right (IEq aexp1 aexp2), restTokens')
                _ -> Nothing
        result -> result

parseNotOrIeqOrLeOrValue :: [Token] -> Maybe (Bexp, [Token])
parseNotOrIeqOrLeOrValue (NotTok : restTokens) =
    case parseIeqOrLeOrValue restTokens of
        Just (Right bexp, restTokens') -> Just (Not bexp, restTokens')
        _ -> Nothing
parseNotOrIeqOrLeOrValue tokens = 
    case parseIeqOrLeOrValue tokens of
        Just (Right bexp, restTokens) -> Just (bexp, restTokens)
        _ -> Nothing

parseEqOrNotOrIeqOrLeOrValue :: [Token] -> Maybe (Bexp, [Token])
parseEqOrNotOrIeqOrLeOrValue tokens =
    case parseNotOrIeqOrLeOrValue tokens of
        Just (bexp1, EqTok : restTokens) ->
            case parseEqOrNotOrIeqOrLeOrValue restTokens of
                Just (bexp2, restTokens') -> Just (Eq bexp1 bexp2, restTokens')
                _ -> Nothing
        result -> result

parseAndOrEqOrNotOrLeOrIeqOrValue :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqOrNotOrLeOrIeqOrValue tokens =
    case parseEqOrNotOrIeqOrLeOrValue tokens of
        Just (bexp1, AndTok : restTokens) ->
            case parseAndOrEqOrNotOrLeOrIeqOrValue restTokens of
                Just (bexp2, restTokens') -> Just (And bexp1 bexp2, restTokens')
                _ -> Nothing
        result -> result

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp = parseAndOrEqOrNotOrLeOrIeqOrValue

main :: IO ()
main = do
    let tokens = "not True and 2 <= 5 = 3+1 == 4"
    print $ parseBexp (lexer tokens)
