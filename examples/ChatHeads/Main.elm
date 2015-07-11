import Spring exposing (Spring)

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events

import Signal exposing (Signal, Address)
import AnimationFrame
import List

import Mouse

import ChatHeads.ChatHead as ChatHead

type alias State =
  { heads : List (Spring ChatHead.State)
  , destination : Vector
  }

(=>) = (,)

init : List String -> State
init sources =
  let
      spring =
        Spring.create 170 30

      position =
        Spring.map2 Vector spring spring

      chatHeads =
        List.map ChatHead.init sources

      setPosition : Vector -> ChatHead.State -> ChatHead.State
      setPosition position head =
        { head | position <- position }

      setChatHead head =
        Spring.map (flip setPosition head) position

      heads =
        List.map setChatHead chatHeads
  in
      { heads = heads
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


update : Action -> State -> State
update action state =
  case action of
    MoveTo destination ->
      { state | destination <- destination }

    NextFrame frame ->
      let
          positions : List (Spring Vector)
          positions =
            List.map (Spring.map .position) state.heads

          setPosition : Vector -> ChatHead.State -> ChatHead.State
          setPosition position head =
            { head | position <- position }

          stepPosition : Spring Vector -> Spring Vector
          stepPosition position =
            let
                x =
                  Spring.map .x position

                y =
                  Spring.map .y position

                newX =
                  Spring.animate frame x

                newY =
                  Spring.animate frame y

            in
                Spring.map2 Vector newX newY

          newPositions : List (Spring Vector)
          newPositions =
            Spring.connectMany state.destination (List.map stepPosition positions)

          newHeads : List (Spring ChatHead.State)
          newHeads =
            List.map2 (Spring.map2 setPosition) newPositions state.heads

      in
          { state | heads <- newHeads }


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
