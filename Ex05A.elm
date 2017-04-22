module Ex05A exposing (..)



type OrgId = OrgId String
type Organisation = Organisation {
    id : OrgId,
    name : String
  }

type UserId = UserId String
type User org = User {
    id : UserId,
    username : String,
    organisation : org
  }


coWorkers :
  User Organisation ->
  List (User OrgId) ->
  List (User OrgId)
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
