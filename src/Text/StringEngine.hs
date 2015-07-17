
---------------------------------------------------------------------------------------------------

module Text.StringEngine(str, strEngine, Var(..)) where


import Language.Haskell.TH.Quote

import Text.StringEngine.Engine(strEngine, Var(..))

---------------------------------------------------------------------------------------------------

str :: QuasiQuoter
str = QuasiQuoter
   {
      quoteExp = dataToExpQ (const Nothing),
      quotePat = error "Cannot use str as a pattern",
      quoteType = error "Cannot use str as a type",
      quoteDec = error "Cannot use str as a dec"
   }

---------------------------------------------------------------------------------------------------
