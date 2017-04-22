-- Gatekeepers / Private Constructors
module Ex03 exposing (..)

-- We've written a module, Email, which exposes
-- some familiar types and functions.
import Email
import Email exposing (Email)

import Task
import Task exposing (Task)
import Helpers -- Talk shortcuts


-- How do we create an Email? We need to use
-- Email.create; there's no other way to
-- create a value of the Email type.
-- (Let's open that file now.)


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






-- Demo helpers

dummyMessage : Email -> Message
dummyMessage to = {
    from = Helpers.fromOk (Email.create "foo@bar.com"),
    to = to,
    subject = "Hello!",
    body = "... World."
  }

