
---------------------------------------------------------------------------------------------------

module Text.StringEngine(str) where

import Language.Haskell.TH.Quote

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
