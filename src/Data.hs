module Data (
) where

import Data.Map.Lazy

type ID = Int

data Object = Commodity String Double | Item ID

type Item = Map String Attribute

data Attribute =
  AContainer [Object] --The contents of a container are not attached and can trivially be removed.
  | AComponents [Object] --The components of an item may be hard to remove and may affect its function.
  | AName String
  | ADesc String
  | ARecipe --Some representation of recipes
  | ALocation
  | AText String

attType :: Attribute -> String
attType AContainer _ = "container"
attType AComponents _ = "components"
attType AName _ = "name"
attType ADesc _ = "desc"
attType ARecipe = "recipe"
attType ALocation = "location"
attType AText _ = "text"

setAtt :: Attribute -> Item -> Item
setAtt att item = insert (attType att) att item

removeAtt :: String -> Item -> Item
removeAtt = delete

getAtt :: String -> Item -> Maybe Attribute
getAtt = lookup -- Maybe overkill, but I might want to change the representation later and this seems like the sort of thing that should be abstract.

hasAtt :: String -> Item -> Boolean
hasAtt n i = isJust $ getAtt n i

getContents :: Item -> [Object]
getContents = maybe [] (\AContainer c -> c) . getAtt "container"

getComponents :: Item -> [Object]
getComponents = maybe [] (\AComponents c -> c) . getAtt "components"
