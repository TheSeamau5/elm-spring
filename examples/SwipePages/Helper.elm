module SwipePages.Helper where

import Color exposing (Color)
import List
import Json.Decode exposing (Decoder, (:=))
import Html.Events
import Signal

type alias Vector =
  { x : Float , y : Float }

infixl 2 =>
(=>) = (,)


--decoder : Decoder Float
decoder =
  "pageX" := Json.Decode.float


--event : String -> Address a -> (Float -> a) -> Attribute
event name address constructor =
  Html.Events.on name decoder (constructor >> Signal.message address)


onMouseDown = event "mousedown"
onMouseUp = event "mouseup"
onMouseMove = event "mousemove"

updateNth : Int -> (a -> a) -> List a -> List a
updateNth n update list =
  List.indexedMap (\index value -> if index == n then update value else value) list
