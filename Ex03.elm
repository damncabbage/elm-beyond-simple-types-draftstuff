-- Gate Key Purse
module Ex03 exposing (..)

-- Defining an external module with some familiar types and functions:
import Email
import Email exposing (Email(..))

import Task
import Task exposing (Task)
import Helpers -- Talk shortcuts




type alias Message = {
    from: Email,
    to: Email,
    subject: String,
    body: String
  }

send : Message -> Task () ()
send msg =
  -- Whatever. This is a stand-in for doing something.
  Task.succeed ()



dummyMessage : Email -> Message
dummyMessage to = {
    from = Helpers.fromOk (Email.create "foo@bar.com"),
    to = to,
    subject = "Hello!",
    body = "... World."
  }

