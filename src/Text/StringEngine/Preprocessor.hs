----------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Text.StringEngine.Preprocessor(preprocessor) where

import Data.List(foldl', intercalate)

import Text.StringEngine.ToString

----------------------------------------------------------------------------------------------------

pattern DelBeginChar = '<'
pattern DelEndChar = '>'
pattern EscChar = '\\'


data Token
   = TokenStrLiteral String
   | TokenExprs String
   deriving(Show, Eq)


data LexerState
   = InString String
   | Escape String
   | InStatements String


instance ToString Token where
   toString (TokenExprs str) = str
   toString (TokenStrLiteral str) = "\"" ++ (intercalate "\\n" $ lines str) ++ "\""


preprocessor :: String -> String
preprocessor = concatMap toString . lexer


lexer :: String -> [Token]
lexer str = case endState of
   InString [] -> filteredTokens
   InString s -> filteredTokens ++ [TokenStrLiteral s]
   _ -> error "Unexpected end"

   where
      (_tokens, endState) = foldl' step ([], InString "") str
      filteredTokens = filter notEmpty _tokens

      step (prefixTokens, currentState) _char = case (currentState, _char) of
         (InString prefixStr, DelBeginChar) -> (prefixTokens ++ [TokenStrLiteral prefixStr], InStatements "")
         (InString prefixStr, EscChar) -> (prefixTokens, Escape prefixStr)
         (InString prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [_char]))
         (Escape prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [_char]))
         (InStatements prefixStr, DelEndChar) -> (prefixTokens ++ [TokenExprs prefixStr], InString "")
         (InStatements prefixStr, _) -> (prefixTokens, InStatements (prefixStr ++ [_char]))

      notEmpty (TokenStrLiteral []) = False
      notEmpty _ = True

----------------------------------------------------------------------------------------------------
