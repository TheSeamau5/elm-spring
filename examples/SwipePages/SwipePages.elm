module SwipePages.SwipePages where

import SwipePages.Helper exposing (..)

import Html exposing (Html)
import Html.Attributes

import Signal exposing (Address)
import List

import Spring exposing (Spring)

type alias State a =
  { offset    : Spring Float
  , children  : List a
  , lastPress : Maybe Float
  }

init : List a -> State a
init children =
  { offset    = Spring.create 170 10
  , children  = children
  , lastPress = Nothing
  }

type alias Context =
  { size : Vector }


type Action a
  = Press Float
  | Move Float
  | Release Float
  | NextFrame Float
  | ChildAction Int a


------------
-- UPDATE --
------------

update : (Context -> action -> state -> state) -> Context -> Action action -> State state -> State state
update updateChild context action state =
  case action of
    Press x ->
      { state | lastPress <- Just x }

    Move x ->
      case state.lastPress of
        Nothing ->
          state

        Just x0 ->
          let
              destination =
                (x0 - x) / context.size.x
                |> clamp 0 (toFloat (List.length state.children - 1))
          in
              { state | offset <- Spring.setDestination destination state.offset }


    Release x ->
      case state.lastPress of
        Nothing ->
          state
        Just x0 ->
          let
              destination =
                round ((x0 - x) / context.size.x)
                |> clamp 0 (List.length state.children - 1)
                |> toFloat
          in
              { state | offset    <- Spring.setDestination destination state.offset
                      , lastPress <- Nothing
              }

    NextFrame frame ->
      { state | offset <- Spring.animate frame state.offset }

    ChildAction index childAction ->
      { state | children <- updateNth index (updateChild context childAction) state.children }





----------
-- VIEW --
----------

view : (Context -> Address action -> state -> Html) -> Context -> Address (Action action) -> State state -> Html
view viewChild context address state =
  let
      widthStr  = toString context.size.x ++ "px"
      heightStr = toString context.size.y ++ "px"

      containerStyle =
          [ "position"  => "absolute"
          , "width"     => widthStr
          , "height"    => heightStr
          , "overflow"  => "hidden"
          ]

      offset =
        Spring.current state.offset

      viewN index child =
        let
            left =
              (toFloat index - offset) * context.size.x

            childContainerStyle =
                [ "position"  => "absolute"
                , "width"     => widthStr
                , "height"    => heightStr
                , "transform" => "translate3d(" ++ toString left ++ "px, 0px, 0px)"
                ]

            childAddress =
              Signal.forwardTo address (ChildAction index)
        in
            Html.div
                [ Html.Attributes.style childContainerStyle ]
                [ viewChild context childAddress child ]
  in
      Html.div
          [ Html.Attributes.style containerStyle
          , onMouseDown address Press
          , onMouseMove address Move
          , onMouseUp address Release
          ]
          ( List.indexedMap viewN state.children )
