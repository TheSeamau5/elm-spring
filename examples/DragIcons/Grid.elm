module DragIcons.Grid where

import DragIcons.Helper exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events

import Spring exposing (Spring)

import Focus exposing (Focus)

import Signal exposing (Address)


-----------------
-- HELPER FOCI --
-----------------

--x : Focus Vector Float
x =
  Focus.create .x (\update v -> { v | x = update v.x })

--y : Focus Vector Float
y =
  Focus.create .y (\update v -> { v | y = update v.y })

-----------
-- STATE --
-----------
-- Position is a fraction of the context size
type alias Animated a =
  { value : a
  , position : Spring Vector
  }


type alias State childState =
  { children : List (Animated childState)
  , selected : Maybe Int
  }

makeSpring : Context -> Vector -> Spring Vector
makeSpring context position =
  let
      stiffness = 170

      damping   = 10

      x =
        Spring.createAt position.x stiffness damping

      y =
        Spring.createAt position.y stiffness damping

      vector =
        Spring.map2 Vector x y
  in
      vector


positionFromIndex : Context -> Int -> Vector
positionFromIndex context index =
  let
      row =
        index // context.numCols

      column =
        index % context.numCols

      cellDims =
        cellSize context

  in
      { x = toFloat column * cellDims.x
      , y = toFloat row * cellDims.y
      }

centerPosition : Context -> Vector -> Vector
centerPosition context position =
  let
      cellDims =
        cellSize context
  in
      { x = position.x - (cellDims.x / 2)
      , y = position.y - (cellDims.y / 2)
      }

fractionalPositionFromIndex : Context -> Int -> Vector
fractionalPositionFromIndex context index =
  fractionalPositionFromPosition context (positionFromIndex context index)

fractionalPositionFromPosition : Context -> Vector -> Vector
fractionalPositionFromPosition context position =
  { x = position.x / context.size.x
  , y = position.y / context.size.y
  }

init : Context -> List childState -> State childState
init context list =
  let
      initChild index child =
        { value = child
        , position =
            makeSpring context (fractionalPositionFromIndex context index)
        }
  in
      { children = List.indexedMap initChild list
      , selected = Nothing
      }


type alias Context =
  { size        : Vector
  , cellHeight  : Float
  , numCols     : Int
  , padding     : Float
  }

cellSize : Context -> Vector
cellSize context =
  { x = context.size.x / (toFloat context.numCols)
  , y = context.cellHeight
  }

findClosestIndex : Context -> Vector -> Int
findClosestIndex context position =
  let
      cellDims =
        cellSize context

      column =
        max 0 (round (position.x / cellDims.x))

      row =
        max 0 (round (position.y / cellDims.y))

      index =
        (row * context.numCols) + column
  in
      index

type alias ChildContext =
  { isSelected : Bool
  , size : Vector
  }


------------
-- UPDATE --
------------

type Action
  = Press Vector
  | MoveTo Vector
  | ReleaseAt Vector
  | NextFrame Float



swapCells : Context -> Int -> Int -> List (Animated childState) -> List (Animated childState)
swapCells context selectedIndex selecteeIndex list =
  if selectedIndex == selecteeIndex then list
  else
    let
        nthElement = nth selectedIndex list
        mthElement = nth selecteeIndex list
    in
        case (nthElement, mthElement) of
          (Just selected, Just selectee) ->
            let
                updateN index value =
                  if index == selectedIndex then
                    gotoCellAtIndex context selectedIndex selectee
                  else if index == selecteeIndex then
                    gotoCellAtIndex context selecteeIndex selected
                  else
                    value

            in
                List.indexedMap updateN list

          _ -> list


swapCellsWhileSelected : Context -> Int -> Int -> List (Animated childState) -> List (Animated childState)
swapCellsWhileSelected context selectedIndex selecteeIndex list =
  if selectedIndex == selecteeIndex then list
  else
    let
        nthElement = nth selectedIndex list
        mthElement = nth selecteeIndex list
    in
        case (nthElement, mthElement) of
          (Just selected, Just selectee) ->
            let
                updateN index value =
                  if index == selectedIndex then
                    gotoCellAtIndex context selectedIndex selectee
                  else if index == selecteeIndex then
                    selected
                  else
                    value

            in
                List.indexedMap updateN list

          _ -> list




gotoCellAtIndex : Context -> Int -> Animated childState -> Animated childState
gotoCellAtIndex context index state =
  let
      destination =
        index
        |> positionFromIndex context
        |> fractionalPositionFromPosition context

  in
      { state | position = Spring.setDestination destination state.position }


update : Context -> Action -> State childState -> State childState
update context action state =
  case action of
    Press position ->
      case state.selected of
        Nothing ->
          let
              adjustedPosition =
                centerPosition context position

              fractionalPosition =
                fractionalPositionFromPosition context adjustedPosition

              index =
                adjustedPosition
                |> findClosestIndex context

              updatePosition child =
                { child | position = Spring.setDestination fractionalPosition child.position }

          in
              { state | selected = Just index
                      , children = updateNth index updatePosition state.children
              }

        Just _ ->
          state


    MoveTo position ->
      case state.selected of
        Nothing ->
          state

        Just index ->
          let
              adjustedPosition =
                centerPosition context position

              fractionalPosition =
                fractionalPositionFromPosition context adjustedPosition

              updatePosition child =
                { child | position = Spring.setDestination fractionalPosition child.position }

              pointTowardsPosition children =
                updateNth index updatePosition children

              closestIndex =
                findClosestIndex context adjustedPosition

              swapLocal children =
                swapCellsWhileSelected context index closestIndex children


              updateChildren =
                pointTowardsPosition >> swapLocal

          in
              { state | children = updateChildren state.children
                      , selected = Just closestIndex
              }

    ReleaseAt position ->
      case state.selected of
        Nothing ->
          state

        Just index ->
          let
              adjustedPosition =
                centerPosition context position

              closestIndex =
                findClosestIndex context adjustedPosition

              anchorSelected n child =
                if n == index
                then
                  gotoCellAtIndex context n child
                else
                  child

              swapLocal children =
                swapCells context index closestIndex children

              updateChildren =
                swapLocal >> List.indexedMap anchorSelected
          in
              { state | children = updateChildren state.children
                      , selected = Nothing
              }

    NextFrame frame ->
      let
          animatePosition pos =
            pos
            |> Spring.animateNested x frame
            |> Spring.animateNested y frame

          updateChild child =
            { child | position = animatePosition child.position }

      in
          { state | children = List.map updateChild state.children }


----------
-- VIEW --
----------

view : (ChildContext -> childState -> Html) -> Address Action -> Context -> State childState -> Html
view viewChild address context grid =
  let
      cellDims =
        cellSize context

      containerStyle =
          [ "position"  => "absolute"
          , "width"     => toString context.size.x ++ "px"
          , "height"    => toString context.size.y ++ "px"
          ]


      childSize =
        { x = cellDims.x - context.padding
        , y = cellDims.y - context.padding
        }

      childContext =
        { isSelected  = False
        , size        = childSize
        }

      selectedChildContext =
        { childContext | isSelected = True }



      viewSelected index child =
        let
            curRatio =
              Spring.current child.position

            curPos =
              { x = (curRatio.x * context.size.x) + context.padding / 2
              , y = (curRatio.y * context.size.y) + context.padding / 2
              }

            childContainerStyle =
                [ "position"  => "absolute"
                , "transform" => "translate3d(" ++ toString curPos.x ++ "px, " ++ toString curPos.y ++ "px, 0px)"
                , "width"     => toString childSize.x ++ "px"
                , "height"    => toString childSize.y ++ "px"
                , "z-index"   => "1"
                ]
        in
            Html.div
                [ Html.Attributes.style childContainerStyle ]
                [ viewChild selectedChildContext child.value ]

      viewCell index child =
        let
            curRatio =
              Spring.current child.position

            curPos =
              { x = (curRatio.x * context.size.x) + context.padding / 2
              , y = (curRatio.y * context.size.y) + context.padding / 2
              }


            childContainerStyle =
                [ "position"  => "absolute"
                , "transform" => "translate3d(" ++ toString curPos.x ++ "px, " ++ toString curPos.y ++ "px, 0px)"
                , "width"     => toString childSize.x ++ "px"
                , "height"    => toString childSize.y ++ "px"
                , "z-index"   => "0"
                ]
        in
            Html.div
                [ Html.Attributes.style childContainerStyle ]
                [ viewChild childContext child.value ]


      viewN index child =
        case grid.selected of
          Nothing ->
            viewCell index child

          Just n ->
            if index == n
            then
              viewSelected index child
            else
              viewCell index child

  in
      Html.div
          [ Html.Attributes.style containerStyle
          , onMouseMove address MoveTo
          , onMouseDown address Press
          , onMouseUp address ReleaseAt
          ]
          ( List.indexedMap viewN grid.children )
