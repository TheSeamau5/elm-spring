module SwipePages.CenteredText where

import SwipePages.Helper exposing (..)
import SwipePages.SwipePages exposing (Context)

import Html exposing (Html)
import Html.Attributes


type alias State =
  { label : String }

init : String -> State
init label =
  { label = label }

view : Context -> State -> Html
view context state =
  let
      fontSize =
        context.size.x / 10

      containerStyle =
          [ "position"  => "absolute"
          , "width"     => toString context.size.x ++ "px"
          , "height"    => toString context.size.y ++ "px"
          , "display"   => "flex"
          , "align-items"     => "center"
          , "justify-content" => "center"
          , "text-align"      => "center"
          , "-webkit-user-select" => "none"
          , "font-size" => toString fontSize ++ "px"
          ]
  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          [ Html.text state.label ]
