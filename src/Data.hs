{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Data (
  ID,
  Object(..),
  Item,
  World(..),
  setAtt,
  setAtt',
  removeAtt,
  getAtt,
  getAtt',
  hasAtt
) where

import Data.Aeson hiding (Object)
import qualified Data.Aeson.Key as Key
import Data.Aeson.TH
import Data.Aeson.Types hiding (Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap
import Data.Kind
import Data.Map.Lazy (Map)
import Data.String
import Data.Type.Equality
import GHC.TypeLits

type ID = Int

data Object = Commodity String Double | Item ID deriving Show

$(deriveJSON defaultOptions ''Object)

type Item = [AnAtt]

data World = World {
  itemStore :: Map ID Item,
  nextID :: ID,
  playerSouls :: Map String ID
}

class KnownSymbol s => Attribute (s::Symbol) where
  type AData s :: Type
  type instance AData s = ()
  defaultData :: AData s
  default defaultData :: (Monoid (AData s)) => AData s
  defaultData = mempty

newtype SSymbol (s :: Symbol) = SSymbol String --This isn't public in this GHC version, but I need something like it and it doesn't actually matter if it's the same as the library's version.
data AnAtt where
  AnAtt :: forall (s::Symbol). (KnownSymbol s, ToJSON (AData s)) => SSymbol s -> AData s -> AnAtt

instance Attribute "container" where type AData "container" = [Object] --The contents of a container are not attached and can trivially be removed.
instance Attribute "components" where type AData "components" = [Object] --The components of an item may be hard to remove and may affect its function.
instance Attribute "name" where type AData "name" = String
instance Attribute "desc" where type AData "desc" = String
instance Attribute "text" where type AData "text" = String
instance Attribute "recipe" --TODO: Add some representation
instance Attribute "location"
instance Attribute ".." where
  type AData ".." = ID
  defaultData = error "There is no default location."

setAtt :: forall s. (Attribute s, ToJSON (AData s)) => AData s -> Item -> Item
setAtt att (AnAtt s' att':item') = case sameSymbol @s undefined s' of
  Just Refl -> AnAtt s' att : item'
  Nothing -> AnAtt s' att' : setAtt @s att item'
setAtt att [] = [AnAtt (SSymbol @s (symbolVal @s undefined)) att]

getAtt' :: forall s. Attribute s => Item -> Maybe (AData s)
getAtt' (AnAtt s' att:item') = case sameSymbol @s undefined s' of
  Just Refl -> Just att
  Nothing -> getAtt' @s item'
getAtt' [] = Nothing

removeAtt :: forall s. Attribute s => Item -> Item
removeAtt (AnAtt s' att':item') = case sameSymbol @s undefined s' of
  Just _ -> item'
  Nothing -> AnAtt s' att' : removeAtt @s item'
removeAtt [] = []

setAtt' :: forall s. (Attribute s, ToJSON (AData s)) => Item -> Item
setAtt' = setAtt @s (defaultData @s)

hasAtt :: forall s. Attribute s => Item -> Bool
hasAtt = maybe False (const True) . getAtt' @s

getAtt :: forall s. Attribute s => Item -> AData s
getAtt = maybe (defaultData @s) id . getAtt' @s

instance ToJSON AnAtt where
  toJSON = error "Serialising individual attributes is not defined."
  toJSONList = Aeson.Object . Data.Aeson.KeyMap.fromList . map (\(AnAtt (SSymbol s) x) -> (fromString s, toJSON x))
  toEncodingList = toEncoding . toJSONList --The toEncoding used here is from the ToJSON Value instance rather than this one.

data AttType where
  AttType :: forall s. (KnownSymbol s, FromJSON (AData s), ToJSON (AData s)) => SSymbol s -> AttType

instance FromJSON AnAtt where
  parseJSON = error "Deserialising individual attributes is not defined."
  parseJSONList = withObject "Item" (mapM parseAtt . Data.Aeson.KeyMap.toList)
    where parseAtt :: (Key,Value) -> Parser AnAtt
          parseAtt (k,v) = modifyFailure (const $ "Unknown attribute: "++show k) $ case someSymbolVal (Key.toString k) of
            SomeSymbol p -> mconcat $ map (\(AttType p') -> case sameSymbol p p' of
                Just Refl -> AnAtt p' <$> parseJSON v
                Nothing -> mempty
              ) attTypes
          attTypes = [attType @"container", attType @"components", attType @"name", attType @"desc", attType @"text", attType @"recipe", attType @"location", attType @".."]
          attType :: forall s. (KnownSymbol s, FromJSON (AData s), ToJSON (AData s)) => AttType
          attType = AttType $ SSymbol @s (symbolVal @s undefined)
