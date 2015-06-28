----------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Text.StringEngine.Lexer
(
   Token(..),
   lexer
)
where


import Data.List(foldl')

----------------------------------------------------------------------------------------------------


pattern DelBeginChar = '<'
pattern DelEndChar = '>'
pattern EscChar = '\\'


data Token
   = SimpleStr String
   | Statements String
   deriving(Show, Eq)


data LexerState
   = InString String
   | Escape String
   | InStatements String


lexer :: String -> [Token]
lexer str =
   let
      (tokens, endState) = foldl' step ([], InString "") str
      step (prefixTokens, currentState) char = case (currentState, char) of
         (InString prefixStr, DelBeginChar) -> (prefixTokens ++ [SimpleStr prefixStr], InStatements "")
         (InString prefixStr, EscChar) -> (prefixTokens, Escape prefixStr)
         (InString prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [char]))
         (Escape prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [char]))
         (InStatements prefixStr, DelEndChar) -> (prefixTokens ++ [Statements prefixStr], InString "")
         (InStatements prefixStr, _) -> (prefixTokens, InStatements (prefixStr ++ [char]))

   in
      tokens

----------------------------------------------------------------------------------------------------
