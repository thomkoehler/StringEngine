----------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Text.StringEngine.Parser
(
   Token(..),
   lexer
)
where

import Data.List(foldl')

import Text.Parsec hiding(State, parse)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Control.Monad.Identity

----------------------------------------------------------------------------------------------------


pattern DelBeginChar = '<'
pattern DelEndChar = '>'
pattern EscChar = '\\'


data Token
   = StrLiteral String
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
         (InStatements prefixStr, DelEndChar) -> (prefixTokens ++ statementLexer prefixStr, InString "")
         (InStatements prefixStr, _) -> (prefixTokens, InStatements (prefixStr ++ [_char]))

      notEmpty (StrLiteral []) = False
      notEmpty _ = True


languageDef :: GenLanguageDef String st Identity
languageDef = P.LanguageDef
   {
      P.commentStart = "/*",
      P.commentEnd = "*/",
      P.commentLine  = "//",
      P.nestedComments = True,
      P.identStart  = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.reservedNames =
         [
            "foreach"
         ],
      P.opStart = P.opLetter languageDef,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["++"],
      P.caseSensitive  = True
   }


parsecLexer = P.makeTokenParser languageDef

identifier  = P.identifier parsecLexer

statementLexer :: String -> [Token]
statementLexer str = [Var str]


----------------------------------------------------------------------------------------------------
