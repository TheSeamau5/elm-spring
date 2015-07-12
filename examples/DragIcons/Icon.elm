module DragIcons.Icon where

import DragIcons.Helper exposing (..)

import Html exposing (Html)
import Html.Attributes

import Color exposing (Color)

type alias State =
  { label : String }

init : String -> State
init label =
  { label = label }

type alias Context =
  { size  : Vector
  , color  : Color
  }


view : Context -> State -> Html
view context state =
  let
      ratio = 5 / 6

      topHeight = ratio * context.size.y

      bottomHeight = 1 - topHeight

      iconStyle =
          [ "position"  => "absolute"
          , "width"     => toString context.size.x ++ "px"
          , "height"    => toString topHeight ++ "px"
          , "border-radius" => "4px"
          , "background-color" => toRgbaString context.color
          ]

      fontSize = 10
        --min context.size.x context.size.y / 5

      textStyle =
          [ "position" => "absolute"
          , "top" => toString topHeight ++ "px"
          , "width" => toString context.size.x ++ "px"
          , "height" => toString bottomHeight ++ "px"
          , "font-size" => toString fontSize ++ "px"
          , "overflow" => "hidden"
          , "-webkit-user-select" => "none"
          ]
  in
      Html.div
          [ ]
          [ Html.div
                [ Html.Attributes.style iconStyle ]
                []
          , Html.div
                [ Html.Attributes.style textStyle ]
                [ Html.text state.label ]
          ]

{-}
main =
  view
    { size = { x = 64 , y = (6 * 64) / 5 }, color = Color.blue }
    { label = "Hello world" }
-}
