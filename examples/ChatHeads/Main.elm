import Spring exposing (Spring)

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events

import Signal exposing (Signal, Address)
import AnimationFrame
import List

import Focus exposing (Focus)

import Mouse

import ChatHeads.ChatHead as ChatHead


type alias State =
  { heads       : List (Spring ChatHead.State)
  , destination : Vector
  }


x : Focus Vector Float
x =
  Focus.create .x (\update v -> { v | x = update v.x })

y : Focus Vector Float
y =
  Focus.create .y (\update v -> { v | y = update v.y })

position : Focus ChatHead.State Vector
position =
   Focus.create .position (\update v -> { v | position = update v.position })


init : List String -> State
init sources =
  let
      spring =
        Spring.create 170 20

      vector =
        Spring.map2 Vector spring spring

      chatHeads =
        List.map ChatHead.init sources

      setChatHead head =
        Spring.map (\v -> Focus.set position v head) vector

      heads =
        List.map setChatHead chatHeads
  in
      { heads       = heads
      , destination = Vector 0 0
      }

initial : State
initial =
  init
    [ "nedstark.jpg"
    , "catelynstark.png"
    , "robbstark.jpg"
    , "jonsnow.jpg"
    , "sansastark.jpg"
    , "aryastark.png"
    , "branstark.jpeg"
    , "rickonstark.jpg"
    ]

type alias Vector =
  { x : Float , y : Float }

type Action
  = MoveTo Vector
  | NextFrame Float


andMap =
  List.map2 (<|)

update : Action -> State -> State
update action state =
  case action of
    MoveTo destination ->
      { state | destination = destination }

    NextFrame frame ->
      let
          positions =
            List.map (Spring.map .position) state.heads

          updatePosition position =
            position
            |> Spring.animateNested x frame
            |> Spring.animateNested y frame

          newPositions =
            Spring.connectMany state.destination (List.map updatePosition positions)

          newHeads =
            List.map2 (Spring.map2 (Focus.set position)) newPositions state.heads

      in
          { state | heads = newHeads }



view : State -> Html
view state =
  Html.div
      [ ]
      ( List.map (Spring.current >> ChatHead.view) (List.reverse state.heads))


nextFrame : Signal Action
nextFrame =
  Signal.map NextFrame AnimationFrame.frame

moveTo : Signal Action
moveTo =
  Signal.map (\(x,y) -> MoveTo { x = toFloat x, y = toFloat y }) Mouse.position

actions : Signal Action
actions =
  Signal.merge moveTo nextFrame

main : Signal Html
main =
  Signal.map view
    (Signal.foldp update initial actions)
