module ExNNImpossible exposing (..)

import List


-- AKA "head"
first : List a -> Maybe a
first list =
  case list of
    [] ->
      Nothing
    (x :: _) ->
      Just x




-- so every time...

type Item = Item { id : String, label : String } -- etc.

{-
getSelection : Maybe Item -> List Item -> Maybe Item
getSelection selected items =
  case selected of
    Just selectedItem ->
      findItemByItem 

-}









-- Helpers
findItemById : Item -> List Item -> Maybe Item
findItemById (Item { id }) items =
  items
    |> List.filter
        (\(Item { id: filterId } -> filterId == id)
    |> first

