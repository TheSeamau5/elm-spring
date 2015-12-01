import Spring exposing (Spring)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, move, filled, circle)
import Color
import Signal exposing (Signal)
import Mouse
import AnimationFrame
import Focus exposing (Focus)

type alias Vector =
  { x : Float
  , y : Float
  }

type alias State = Vector

initial : Spring State
initial =
  let
      spring = Spring.create 170 10
  in
      Spring.map2 Vector spring spring


x : Focus Vector Float
x =
  Focus.create .x (\update v -> { v | x = update v.x })

y : Focus Vector Float
y =
  Focus.create .y (\update v -> { v | y = update v.y })


type Action
  = MoveTo Vector
  | NextFrame Float


update : Action -> Spring State -> Spring State
update action spring =
  case action of
    MoveTo destination ->
      Spring.setDestination destination spring

    NextFrame frame ->
      spring
      |> Spring.animateNested x frame
      |> Spring.animateNested y frame



view : State -> Element
view state =
  circle 20
  |> filled Color.red
  |> move (state.x, state.y)
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
