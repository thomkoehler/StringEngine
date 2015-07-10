
---------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Text.StringEngine(stringEngine, str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.StringEngine.Parser(stringEngine)

import Text.StringEngine.DynAny --TODO

---------------------------------------------------------------------------------------------------

str :: QuasiQuoter
str = QuasiQuoter
   {
      quoteExp = dataToExpQ (const Nothing),
      quotePat = error "Cannot use se as a pattern",
      quoteType = error "Cannot use se as a type",
      quoteDec = error "Cannot use se as a dec"
   }

---------------------------------------------------------------------------------------------------
