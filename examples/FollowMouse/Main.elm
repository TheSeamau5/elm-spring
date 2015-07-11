import Spring exposing (Spring)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, move, filled, circle)
import Color
import Signal exposing (Signal)
import Mouse
import AnimationFrame

type alias Vector =
  { x : Float
  , y : Float
  }

type alias State =
  { position : Vector }

initial : Spring State
initial =
  let
      spring = Spring.create 170 10

      position = Spring.map2 Vector spring spring
  in
      Spring.map State position

type Action
  = MoveTo Vector
  | NextFrame Float

update : Action -> Spring State -> Spring State
update action spring =
  case action of
    MoveTo destination ->
      let
          position = Spring.map .position spring

          newPosition = Spring.setDestination destination position
      in
          Spring.map State newPosition


    NextFrame frameRate ->
      let
          x = Spring.map (.position >> .x) spring
          y = Spring.map (.position >> .y) spring

          newX = Spring.animate frameRate x
          newY = Spring.animate frameRate y

          position = Spring.map2 Vector newX newY

      in
          Spring.map State position

view : State -> Element
view state =
  circle 20
  |> filled Color.red
  |> move (state.position.x, state.position.y)
  |> (\x -> [x])
  |> collage 400 400


nextFrame : Signal Action
nextFrame =
  Signal.map NextFrame AnimationFrame.frame

moveTo : Signal Action
moveTo =
  Signal.map (\(x,y) -> MoveTo { x = toFloat x - 200, y = 200 - toFloat y }) Mouse.position

actions : Signal Action
actions =
  Signal.merge moveTo nextFrame

main : Signal Element
main =
  Signal.map (Spring.current >> view)
    (Signal.foldp update initial actions)
