
---------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Text.StringEngine(stringEngine, se) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Text.StringEngine.Parser(stringEngine)

---------------------------------------------------------------------------------------------------

se :: QuasiQuoter
se = QuasiQuoter
   {
      quoteExp = quoteExpSe,
      quotePat = error "Cannot use se as a pattern",
      quoteType = error "Cannot use se as a type",
      quoteDec = error "Cannot use se as a dec"
   }


quoteExpSe :: String -> TH.Q TH.Exp
quoteExpSe str = do
   let out = stringEngine (\_ -> Nothing) str
   dataToExpQ (const Nothing) out

---------------------------------------------------------------------------------------------------
