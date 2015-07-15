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
   = StrLiteral String
   | Expressions String
   deriving(Show, Eq)


data LexerState
   = InString String
   | Escape String
   | InStatements String


instance ToString Token where
   toString (Expressions str) = str
   toString (StrLiteral str) = "\"" ++ (intercalate "\\n" $ lines str) ++ "\""


preprocessor :: String -> String
preprocessor = concat . map toString . lexer


lexer :: String -> [Token]
lexer str = case endState of
   InString [] -> filteredTokens
   InString s -> filteredTokens ++ [StrLiteral s]
   _ -> error "Unexpected end"

   where
      (_tokens, endState) = foldl' step ([], InString "") str
      filteredTokens = filter notEmpty _tokens

      step (prefixTokens, currentState) _char = case (currentState, _char) of
         (InString prefixStr, DelBeginChar) -> (prefixTokens ++ [StrLiteral prefixStr], InStatements "")
         (InString prefixStr, EscChar) -> (prefixTokens, Escape prefixStr)
         (InString prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [_char]))
         (Escape prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [_char]))
         (InStatements prefixStr, DelEndChar) -> (prefixTokens ++ [Expressions prefixStr], InString "")
         (InStatements prefixStr, _) -> (prefixTokens, InStatements (prefixStr ++ [_char]))

      notEmpty (StrLiteral []) = False
      notEmpty _ = True

----------------------------------------------------------------------------------------------------
