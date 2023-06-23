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
  emptyWorld,
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
import Data.List (mapAccumL)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.String
import Data.Type.Equality
import GHC.TypeLits

type ID = Int

data Object = Commodity String Double | ItemRef ID deriving Show
data ObjectInline = CommodityI String Double | ItemI Item

type Item = [AnAtt]

data World = World {
  itemStore :: Map ID Item,
  nextID :: ID,
  playerSouls :: Map String ID
}

emptyWorld :: World
emptyWorld = World M.empty 0 M.empty

class KnownSymbol s => Attribute (s::Symbol) where
  type AData s :: Type
  type instance AData s = ()
  defaultData :: AData s
  default defaultData :: (Monoid (AData s)) => AData s
  defaultData = mempty

newtype SSymbol (s :: Symbol) = SSymbol String --This isn't public in this GHC version, but I need something like it and it doesn't actually matter if it's the same as the library's version.
data AnAtt where
  AnAtt :: forall (s::Symbol). (KnownSymbol s, ToJSON (AData s)) => SSymbol s -> AData s -> AnAtt
$(deriveJSON defaultOptions ''Object)
$(deriveJSON defaultOptions ''ObjectInline)

instance Attribute "container" where type AData "container" = [Object] --The contents of a container are not attached and can trivially be removed.
instance Attribute "components" where type AData "components" = [Object] --The components of an item may be hard to remove and may affect its function.
instance Attribute "containerInline" where type AData "containerInline" = [ObjectInline]
instance Attribute "componentsInline" where type AData "componentsInline" = [ObjectInline]
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

$(deriveJSON defaultOptions ''World)

-- Given an item all of whose contents and components are inline, add it to the world. Optionally give the item being added some location.
deinline :: Maybe ID -> Item -> World -> (World,ID)
deinline up it w = (w3{itemStore = M.insert n it3 (itemStore w3)}, n)
  where n = nextID w
        it1 = case up of
          Nothing -> it
          Just up' -> setAtt @".." up' it
        w1 = w{nextID = n+1}
        (it2,w2) = case getAtt' @"containerInline" it1 of
          Nothing -> (it1,w1)
          Just c -> let (w',c') = insertContents w1 c in (setAtt @"container" c' $ removeAtt @"containerInline" it1, w')
        (it3,w3) = case getAtt' @"componentsInline" it2 of
          Nothing -> (it2,w2)
          Just c -> let (w',c') = insertContents w2 c in (setAtt @"components" c' $ removeAtt @"componentsInline" it2, w')
        insertContents = mapAccumL (\w' o -> case o of
            ItemI i -> ItemRef <$> deinline (Just n) i w'
            CommodityI t q -> (w',Commodity t q)
          )
