import DragIcons.Helper exposing (..)
import DragIcons.Icon as Icon
import DragIcons.Grid as Grid

import Signal exposing (Address)
import List

import Html exposing (Html)
import Window
import Task exposing (Task)

import AnimationFrame

import Color

initialState context =
  Grid.init context
    [ Icon.init "Icon0"
    , Icon.init "Icon1"
    , Icon.init "Icon2"
    , Icon.init "Icon3"
    , Icon.init "Icon4"
    , Icon.init "Icon5"
    , Icon.init "Icon6"
    , Icon.init "Icon7"
    , Icon.init "Icon8"
    , Icon.init "Icon9"
    , Icon.init "Icon10"
    , Icon.init "Icon11"
    ]

makeContext : Vector -> Grid.Context
makeContext viewport =
  let
      numCols = 4

      padding =
        5

      cellHeight =
        6 * (viewport.x / toFloat numCols) / 5
  in
      { size        = viewport
      , cellHeight  = cellHeight
      , numCols     = numCols
      , padding     = padding
      }

gridToIconContext : Grid.ChildContext -> Icon.Context
gridToIconContext context =
  if context.isSelected
  then
    { color = Color.green
    , size  = context.size
    }
  else
    { color = Color.blue
    , size  = context.size
    }

type alias ApplicationState = Maybe
  { context : Grid.Context
  , state   : Grid.State Icon.State
  }


type Action appAction
  = Resize Vector
  | ApplicationAction appAction
  | NoOp




update : Action (Grid.Action) -> ApplicationState -> ApplicationState
update action maybeState =
  case action of
    Resize viewport ->
      case maybeState of
        Nothing ->
          let
              context = makeContext viewport
          in
              Just
                { context = context
                , state   = initialState context
                }

        Just appState ->
          let
              context = appState.context

              context' =
                { context | size <- viewport }
          in
              Just { appState | context <- context' }

    ApplicationAction action ->
      case maybeState of
        Nothing -> Nothing

        Just appState -> Just
          { appState | state <- Grid.update appState.context action appState.state }

    NoOp -> maybeState

view : Address (Action Grid.Action) -> ApplicationState -> Html
view address maybeState =
  case maybeState of
    Nothing ->
      Html.text "Loading..."

    Just appState ->
      let
          appAddress =
            Signal.forwardTo address ApplicationAction
      in
          Grid.view (gridToIconContext >> Icon.view) appAddress appState.context appState.state


frames =
  Signal.map (Grid.NextFrame >> ApplicationAction) AnimationFrame.frame


startAppMailbox =
  Signal.mailbox ()

startAppTaskMailbox =
  Signal.mailbox (Signal.send startAppMailbox.address ())

port startApp : Signal (Task error ())
port startApp =
  startAppTaskMailbox.signal

firstDimensions =
  Signal.sampleOn startAppMailbox.signal Window.dimensions

windowDimensions =
  Signal.merge firstDimensions Window.dimensions

resizes =
  Signal.map (\(x,y) -> Resize { x = toFloat x , y = toFloat y }) windowDimensions

appMailbox =
  Signal.mailbox NoOp

address =
  appMailbox.address

actions =
  Signal.mergeMany
    [ resizes
    , frames
    , appMailbox.signal
    ]


main =
  Signal.map (view address)
    (Signal.foldp update Nothing actions)
