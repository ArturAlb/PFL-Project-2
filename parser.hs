{-
module Parser (Token(..),Aexp(..),Bexp(..),
lexer,parseIntOrParenthesis,parseAddOrProdOrInt,parseProdOrInt,parseSubOrAddOrProdOrInt,parseAexp) where
-}
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
    | IEqExp Aexp Aexp    -- integer equality test
    | EqExp Bexp Bexp     -- equality test
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
lexer ('w' : 'h' : 'i' : 'l' : 'e' : restStr) = WhileTok : lexer restStr
lexer ('d' : 'o' : restStr) = DoTok : lexer restStr
lexer ('i' : 'f' : restStr) = IfTok : lexer restStr
lexer ('t' : 'h' : 'e' : 'n' : restStr) = ThenTok : lexer restStr
lexer ('e' : 'l' : 's' : 'e' : restStr) = ElseTok : lexer restStr
lexer ('a' : 'n' : 'd' : restStr) = AndTok : lexer restStr
lexer ('n' : 'o' : 't' : restStr) = NotTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr) = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr
lexer (';' : restStr) = SemiColonTok : lexer restStr
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
                Just (aexp2, restTokens1) -> Just (AddExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
    case parseIntOrParenthesis tokens of
        Just (aexp1, TimesTok : restTokens) ->
            case parseProdOrInt restTokens of
                Just (aexp2, restTokens1) -> Just (MultExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseSubOrAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSubOrAddOrProdOrInt tokens =
    case parseAddOrProdOrInt tokens of
        Just (aexp1, MinusTok : restTokens) ->
            case parseSubOrAddOrProdOrInt restTokens of
                Just (aexp2, restTokens1) -> Just (SubExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp = parseSubOrAddOrProdOrInt

parseTrueOrFalseOrIntOrParenthesis :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseTrueOrFalseOrIntOrParenthesis (TrueTok : restTokens) = Just (Right BTrue, restTokens)
parseTrueOrFalseOrIntOrParenthesis (FalseTok : restTokens) = Just (Right BFalse, restTokens)
parseTrueOrFalseOrIntOrParenthesis (IntTok n : restTokens) =
    case parseAexp (IntTok n : restTokens) of
        Just (aexp, restTokens1) -> Just (Left aexp, restTokens1)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis (VarTok var : restTokens) =
    case parseAexp (VarTok var : restTokens) of
        Just (aexp, restTokens1) -> Just (Left aexp, restTokens1)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis (OpenTok : tokens) =
    case parseBexp tokens of
        Just (bexp, CloseTok : restTokens) -> Just (Right bexp, restTokens)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis _ = Nothing

parseLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseLeOrValue tokens =
    case parseTrueOrFalseOrIntOrParenthesis tokens of
        Just (Left aexp1, LeTok : restTokens) ->
            case parseLeOrValue restTokens of
                Just (Left aexp2, restTokens1) -> Just (Right (LeExp aexp1 aexp2), restTokens1)
                _ -> Nothing
        result -> result

parseIeqOrLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseIeqOrLeOrValue tokens =
    case parseLeOrValue tokens of
        Just (Left aexp1, DEqTok : restTokens) ->
            case parseIeqOrLeOrValue restTokens of
                Just (Left aexp2, restTokens1) -> Just (Right (IEqExp aexp1 aexp2), restTokens1)
                _ -> Nothing
        result -> result

parseNotOrIeqOrLeOrValue :: [Token] -> Maybe (Bexp, [Token])
parseNotOrIeqOrLeOrValue (NotTok : restTokens) =
    case parseIeqOrLeOrValue restTokens of
        Just (Right bexp, restTokens1) -> Just (NotExp bexp, restTokens1)
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
                Just (bexp2, restTokens1) -> Just (EqExp bexp1 bexp2, restTokens1)
                _ -> Nothing
        result -> result

parseAndOrEqOrNotOrLeOrIeqOrValue :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqOrNotOrLeOrIeqOrValue tokens =
    case parseEqOrNotOrIeqOrLeOrValue tokens of
        Just (bexp1, AndTok : restTokens) ->
            case parseAndOrEqOrNotOrLeOrIeqOrValue restTokens of
                Just (bexp2, restTokens1) -> Just (AndExp bexp1 bexp2, restTokens1)
                _ -> Nothing
        result -> result

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp = parseAndOrEqOrNotOrLeOrIeqOrValue

parseStms :: [Token] -> Maybe ([Stm], [Token])
parseStms tokens =
    case parseStm tokens of
        Just (stms1, restTokens) ->
            case restTokens of
                (SemiColonTok : restTokens1) ->
                    case parseStms restTokens1 of
                        Just (stms2, restTokens2) -> Just (stms1 ++ stms2, restTokens2)
                        _ -> Just (stms1, restTokens1)
                _ -> Just (stms1, restTokens)
        _ -> Nothing

parseNestedStms :: [Token] -> Maybe ([Stm], [Token])
parseNestedStms (OpenTok : restTokens) =
    case parseStms restTokens of
        Just (stms, CloseTok : restTokens1) -> Just (stms, restTokens1)
        _ -> Nothing
parseNestedStms tokens =
    case parseStm tokens of
        Just (stm, SemiColonTok : restTokens) -> Just (stm, restTokens)
        _ -> parseStms tokens

parseStm :: [Token] -> Maybe ([Stm], [Token])
parseStm (IfTok : restTokens) =
    case parseBexp restTokens of
        Just (bexp, restTokens1) ->
            case restTokens1 of
                (ThenTok : restTokens2) ->
                    case parseNestedStms restTokens2 of
                        Just (stms1, ElseTok : restTokens3) ->
                            case parseNestedStms restTokens3 of
                                Just (stms2, restTokens4) ->
                                    case parseStms restTokens4 of
                                        Just (stms, restTokens5) -> Just (If bexp stms1 stms2 : stms, restTokens5)
                                        _ -> Just ([If bexp stms1 stms2], restTokens4)
                                _ -> Nothing
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
parseStm (WhileTok : restTokens) =
    case parseBexp restTokens of
        Just (bexp, DoTok : restTokens1) ->
            case parseNestedStms restTokens1 of
                Just (stms, restTokens2) -> Just ([While bexp stms], restTokens2)
                _ -> Nothing
        _ -> Nothing
parseStm (VarTok var : AssignTok : restTokens) =
    case parseAexp restTokens of
        Just (aexp, restTokens1) -> Just ([Assign var aexp], restTokens1)
        _ -> Nothing
parseStm tokens = Nothing

main :: IO ()
main = do
    --let tokens = "x := 5; x := x - 1;"
    -- let tokens = "x := 0 - 2;"
    let tokens = "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2; x := (1+1)+4;"
    -- let tokens = "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
    -- let tokens = "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
    -- let tokens = "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
    -- let tokens = "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
    -- let tokens = "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"
    -- let tokens = "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
    -- let tokens = "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
    -- let tokens = "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
    -- let tokens = "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;); x := 1"
    print $ parseStms (lexer tokens)
