module DragIcons.Helper where

import Color exposing (Color)
import List
import Json.Decode exposing (Decoder, (:=))
import Html.Events
import Signal

type alias Vector =
  { x : Float , y : Float }

infixl 2 =>
(=>) = (,)

--decoder : Decoder Vector
decoder =
  Json.Decode.object2 Vector
    ("pageX" := Json.Decode.float)
    ("pageY" := Json.Decode.float)


--event : String -> Address a -> (Vector -> a) -> Attribute
event name address constructor =
  Html.Events.on name decoder (constructor >> Signal.message address)


onMouseDown = event "mousedown"
onMouseUp = event "mouseup"
onMouseMove = event "mousemove"

toRgbaString : Color -> String
toRgbaString color =
  let {red, green, blue, alpha} = Color.toRgb color
  in
      "rgba(" ++ toString red ++ ", " ++ toString green ++ ", " ++ toString blue ++ ", " ++ toString alpha ++ ")"


nth : Int -> List a -> Maybe a
nth n list =
  case List.filter (\(x, _) -> x == True)
    (List.indexedMap (\m value -> if n == m then (True, value) else (False, value)) list)
  of
    [] -> Nothing
    (_, x) :: _ -> Just x


updateNth : Int -> (a -> a) -> List a -> List a
updateNth n update list =
  List.indexedMap (\index value -> if index == n then update value else value) list



swap : Int -> Int -> List a -> List a
swap n m list =
  let
      nthElement = nth n list
      mthElement = nth m list
  in
      case (nthElement, mthElement) of
        (Just nx, Just mx) ->
          List.indexedMap (\index value ->
            if index == n then
              mx
            else if index == m then
              nx
            else
              value
          ) list

        _ -> list
