----------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Text.StringEngine.DynAny where


import Text.StringEngine.ToString

----------------------------------------------------------------------------------------------------

data DynAny
   = DynString String
   | DynList [DynAny]
   | DynNothing


class ToDynAny a where
   toDynAny :: a -> DynAny


instance ToDynAny DynAny where
   toDynAny = id


instance ToDynAny String where
   toDynAny = DynString


instance (ToDynAny d) => ToDynAny [d] where
   toDynAny = DynList . map toDynAny


instance (ToDynAny d) => ToDynAny (Maybe d) where
   toDynAny (Just d) = toDynAny d
   toDynAny Nothing = DynNothing


instance ToString DynAny where
   toString (DynString str) = str
   toString _ = error "String expected."


toList :: DynAny -> [DynAny]
toList da = case da of
   DynList ds -> ds
   _ -> error "List expected."


isNull :: DynAny -> Bool
isNull (DynList []) = True
isNull DynNothing = True
isNull _ = False


----------------------------------------------------------------------------------------------------
