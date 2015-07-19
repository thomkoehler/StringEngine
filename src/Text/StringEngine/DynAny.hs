----------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Text.StringEngine.DynAny where

----------------------------------------------------------------------------------------------------

data DynAny
   = DynString String
   | DynBool Bool
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


asBool :: DynAny -> Bool
asBool (DynBool b) = b
asBool _ = error "Bool expected."


asString :: DynAny -> String
asString (DynString str) = str
asString _ = error "String expected."


asList :: DynAny -> [DynAny]
asList da = case da of
   DynList ds -> ds
   _ -> error "List expected."

----------------------------------------------------------------------------------------------------
