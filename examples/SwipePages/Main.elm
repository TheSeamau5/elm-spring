import SwipePages.SwipePages as SwipePages
import SwipePages.CenteredText as Text
import SwipePages.Helper exposing (..)

import Html exposing (Html)
import Html.Attributes

import Signal exposing (Signal, Address)
import Task exposing (Task)
import Window

import AnimationFrame

type alias State = Maybe
  { state : SwipePages.State Text.State
  , context : SwipePages.Context
  }

initial = Nothing

initialState : Vector -> State
initialState size = Just
  { state =
      SwipePages.init
        [ Text.init "Page 0"
        , Text.init "Page 1"
        , Text.init "Page 2"
        , Text.init "Page 3"
        , Text.init "Page 4"
        ]
  , context = { size = size }
  }


type Action a
  = Resize Vector
  | ChildAction (SwipePages.Action a)
  | NoOp


noChange _ _ state = state

update : Action a -> State -> State
update appAction appState =
  case appAction of
    Resize size ->
      case appState of
        Nothing ->
          initialState size

        Just state -> Just
          { state | context = { size = size }}

    ChildAction action ->
      case appState of
        Nothing ->
           Nothing

        Just state -> Just
          { state | state = SwipePages.update noChange state.context action state.state }

    NoOp -> appState

textView context _ state =
  Text.view context state

view : Address (Action a) -> State -> Html
view address appState =
  case appState of
    Nothing ->
      Html.text "Loading..."

    Just state ->
      SwipePages.view textView state.context (Signal.forwardTo address ChildAction) state.state



-----------------

frames =
  Signal.map (ChildAction << SwipePages.NextFrame) AnimationFrame.frame

windowDimensions =
  Signal.merge
    (Signal.sampleOn startAppMailbox.signal Window.dimensions)
    Window.dimensions


resizes =
  Signal.map (\(x,y) -> Resize { x = toFloat x , y = toFloat y }) windowDimensions

startAppMailbox =
  Signal.mailbox ()

port startApp : Signal (Task error ())
port startApp =
  Signal.constant (Signal.send startAppMailbox.address ())


actionMailbox =
  Signal.mailbox NoOp

address =
  actionMailbox.address

actions =
  Signal.mergeMany
    [ actionMailbox.signal
    , resizes
    , frames
    ]

main =
  Signal.map (view address)
    (Signal.foldp update initial actions)
