module ChatHeads.ChatHead where

import Html exposing (Html)
import Html.Attributes

------------
infixl 2 =>
(=>) = (,)
------------


type alias State =
  { radius    : Float
  , source    : String
  , position  : { x : Float , y : Float }
  }

init : String -> State
init source =
  { radius = 40
  , source = source
  , position = { x = 0 , y = 0 }
  }

view : State -> Html
view state =
  let
      containerStyle =
        [ "position"  => "absolute"
        --, "top"       => toString (state.position.y - state.radius) ++ "px"
        --, "left"      => toString (state.position.x - state.radius) ++ "px"
        , "transform" => "translate3d(" ++ toString (state.position.x - state.radius) ++ "px, " ++ toString (state.position.y - state.radius) ++ "px, 0px)"
        , "width"     => toString (state.radius * 2) ++ "px"
        , "height"    => toString (state.radius * 2) ++ "px"
        , "border-radius"       => "50%"
        , "background-image"    => "url(" ++ state.source ++ ")"
        , "background-repeat"   => "no-repeat"
        , "background-size"     => "cover"
        , "background-position" => "center"
        , "border" => "4px solid red"
        ]
  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          []
