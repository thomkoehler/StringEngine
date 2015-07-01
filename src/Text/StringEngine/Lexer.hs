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
   | Var String
   | ForEach
   deriving(Show, Eq)


data LexerState
   = InString String
   | Escape String
   | InStatements String


lexer :: String -> [Token]
lexer str = case endState of
   InString [] -> filteredTokens
   InString str -> filteredTokens ++ [SimpleStr str]
   _ -> error "Unexpected end"

   where
      (tokens, endState) = foldl' step ([], InString "") str
      filteredTokens = filter notEmpty tokens

      step (prefixTokens, currentState) char = case (currentState, char) of
         (InString prefixStr, DelBeginChar) -> (prefixTokens ++ [SimpleStr prefixStr], InStatements "")
         (InString prefixStr, EscChar) -> (prefixTokens, Escape prefixStr)
         (InString prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [char]))
         (Escape prefixStr, _) -> (prefixTokens, InString (prefixStr ++ [char]))
         (InStatements prefixStr, DelEndChar) -> (prefixTokens ++ statementLexer prefixStr, InString "")
         (InStatements prefixStr, _) -> (prefixTokens, InStatements (prefixStr ++ [char]))

      notEmpty (SimpleStr []) = False
      notEmpty _ = True


statementLexer :: String -> [Token]
statementLexer str = [Var str]

----------------------------------------------------------------------------------------------------
