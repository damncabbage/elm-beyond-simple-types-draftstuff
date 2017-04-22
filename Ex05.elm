-- Making Invalid Things Unrepresentable (https://vimeo.com/14313378)
-- Making Sure the Impossible is Impossible (TODO: Feldman talk link.)
module Ex05 exposing (..)

import List




---- It's about valid states. ----

oneOrTheOther : (Maybe String, Maybe Int)
oneOrTheOther =
  (Just "Hi", Just 2)
  -- Or...
  --   (Just "Hi", Nothing)
  --   (Nothing, Just 2)
  --   (Nothing, Nothing)  -- 😫

-- Let's try:
type These a b =
    This a
  | That b
  | These a b

oneOrTheOtherV2 : These String Int
oneOrTheOtherV2 =
  This "Hi"
  -- Or...
  --   That 2
  --   These "Hi" 2
  -- No Nothing; always returns _something_.
  -- Enforced by compiler.



---- Lists that you want to make sure always have at least one item ----

-- firstItem is also known as "List.head"
firstItem : List a -> Maybe a
firstItem list =
  case list of
    [] ->
      Nothing
    (x :: _) ->
      Just x


-- With some items:
type Item = Item { id : String, label : String } -- etc.

someItems : List Item
someItems =
  [ Item { id = "a1", label = "Apple" }
  , Item { id = "b2", label = "Banana" }
  , Item { id = "c3", label = "Rock" }
  ]

containsOrDefault : Item -> List Item -> Maybe Item
containsOrDefault (Item { id }) items =
  items
    |> List.filter (\(Item i) -> i.id == id)
    |> firstItem

-- Is a -> Maybe Item, so every time we use this we need
-- to account for this being empty, even if we know we've
-- already got an item in the list.


-- What about this type instead?
type NonEmptyList a =
  NonEmptyList a (List a)  -- A thing, and then the rest.

someItemsV2 : NonEmptyList Item
someItemsV2 =
  NonEmptyList
    (Item { id = "a1", label = "Apple" }) -- <= First item...
    [ Item { id = "b2", label = "Banana" } -- <= ... And the rest.
    , Item { id = "c3", label = "Rock" }
    ]

containsOrDefaultV2 : Item -> NonEmptyList Item -> Item -- Not a Maybe!
containsOrDefaultV2 (Item { id }) (NonEmptyList first rest) =
  (first :: rest)
    |> List.filter (\(Item i) -> i.id == id)
    |> \items -> Maybe.withDefault first (firstItem items)
  --   ^== We can actually provided a default now; the first item in the non-empty list.
  --       (FYI, Maybe.withDefault : a -> Maybe a -> a)





---- Maybe Parameterisation ----

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

-- Clumsy; we need to handle Maybe everywhere,
-- and have to have Users with all the Organisation data.
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


-- Let's do this instead:
{-
-- Parameterised User data; now sub in what we want...
type User org = User {
    id : UserId,
    username : String,
    organisation : org -- <= ... here.
  }

coWorkers :
  User Organisation -> -- With a User that has Org info
  List (User OrgId) -> -- And Users with only OrgIDs
  List (User OrgId)    -- Select those relevant users.
coWorkers (User user) allUsers =
  case user.organisation of
    Organisation org ->
      findByOrgId org.id allUsers

findByOrgId :
  OrgId ->
  List (User OrgId) ->
  List (User OrgId)
findByOrgId id users =
  List.filter
    (\(User u) -> u.organisation == id)
    users

-- ^=== Look Ma, no Maybe


-- Demo Helpers
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
-}




---- Textbox behind a checkbox. ----

type alias SomeUI = {
    hasTitle : Bool,      -- Checkbox, "has title?"
    title : Maybe String  -- Just (textbox value), or Nothing if no title textbox.
  }

type alias SomeUIv2 = {
    title: Maybe String  -- Just (textbox value, eg. "", "foo") if checked, or Nothing if not.
  }



---- Loading some items from a server ----
-- http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html

type Status =
    Loading
  | Failed
  | Succeeded

type alias ItemsFromServer error item =
  { status: Status
  , items: List item
  , error: Maybe error
  }

-- Allows nonsensical things like...
--   { status = Loading, item: ["123"], error: Just "It exploded!" }
--   { status = Failed, item: ["123"], error: Nothing }
--   { status = Succeeded, item: ["123"], error: Just "It exploded!" }

-- Try instead:
type RemotelyLoaded error item =
    IsLoading
  | HasFailed error
  | HasSucceeded (List item)

-- eg.
--   IsLoading                   : RemotelyLoaded e x
--   HasFailed "Mandatory error" : RemotelyLoaded String x
--   HasSucceeded [1,2,3]        : RemotelyLoaded x Int

-- (PS: Please pardon the pointless constructor renames; was
-- to avoid colliding with the first example.)
