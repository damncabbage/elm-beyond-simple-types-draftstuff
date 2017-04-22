-- Imp OSS sable
module Ex05 exposing (..)

import List




-- It's about states.

oneOrTheOther : (Maybe String, Maybe Int)
oneOrTheOther =
  (Just "Hi", Just 2)
  --(Just "Hi", Nothing)
  --(Nothing, Just 2)
  --(Nothing, Nothing)  -- 😫







type These a b =
    This a
  | That b
  | These a b






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




-- may bee pagh ram it eye sate shone

type OrgId = OrgId String
type Organisation = Organisation {
    id : OrgId,
    name : String
  }

type UserId = UserId String
type User = User {
    id : UserId,
    username : String,
    organisation : Maybe Organisation
  }


coWorkers : User -> List User -> List User
coWorkers (User user) allUsers =
  case user.organisation of
    Just (Organisation org) ->
      findByOrgId org.id allUsers
    Nothing ->
      []

findByOrgId : OrgId -> List User -> List User
findByOrgId id users =
  List.filter
    (\(User u) ->
      case u.organisation of
        Just (Organisation o) ->
          id == o.id
        Nothing ->
          False
    )
    users



-- Helpers

someUser : User Organisation
someUser =
  User
    { id = UserId "2"
    , username = "Sam"
    , organisation =
        Organisation
          { id = OrgId "1"
          , name = "Elm Illuminati"
          }
    }

someUsers : List (User OrgId)
someUsers =
  [ User { id = UserId "1", username = "Kate"
         , organisation = OrgId "1" }
  , User { id = UserId "2", username = "Sam"
         , organisation = OrgId "1" }
  , User { id = UserId "3", username = "Sally"
         , organisation = OrgId "2" }
  , User { id = UserId "4", username = "Harish"
         , organisation = OrgId "1" }
  ]



-- Helpers

findItemById : Item -> List Item -> Maybe Item
findItemById (Item { id }) items =
  items
    |> List.filter
        (\(Item i) -> i.id == id)
    |> first

