----------------------------------------------------------------------------------------------------

module Text.StringEngine.Functions
(
   lookupFunction1
)
where

import Prelude hiding(lookup)
import Data.Map hiding(null)
import Data.Maybe

import Text.StringEngine.DynAny

----------------------------------------------------------------------------------------------------

lookupFunction1 :: String -> (DynAny -> DynAny)
lookupFunction1 name =
   fromMaybe (error ("Function " ++ name ++ " is unknown.")) (lookup name functions1)


functions1 :: Map String (DynAny -> DynAny)
functions1 = fromList
   [
      ("isNull", isNull),
      ("isNotNull", isNotNull),
      ("isEmpty", isEmpty),
      ("isNotEmpty", isNotEmpty)
   ]


isNull :: DynAny -> DynAny
isNull DynNothing = dynTrue
isNull _ = dynFalse


isNotNull :: DynAny -> DynAny
isNotNull DynNothing = dynFalse
isNotNull _ = dynTrue


isEmpty :: DynAny -> DynAny
isEmpty (DynString str) = toDynAny $ null str
isEmpty (DynList ds) = toDynAny $ null ds
isEmpty _ = error "isEmpty: String or List expected."


isNotEmpty :: DynAny -> DynAny
isNotEmpty (DynString str) = toDynAny $ not $ null str
isNotEmpty (DynList ds) = toDynAny $ not $ null ds
isNotEmpty _ = error "isNotEmpty: String or List expected."

----------------------------------------------------------------------------------------------------

