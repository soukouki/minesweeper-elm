module Main exposing (..)

import String exposing (fromInt)
import Maybe exposing (withDefault)
import Random
import Browser
import Html exposing (Html, div, p, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)


-- MAIN

main =
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Cell = 
  { isOpen: Bool
  , isBomb: Bool
  , surroundBombsCount: Int
  }

type alias Table a = List (List a)
type alias CellTable = Table Cell

atTable : (Table a) -> (Int, Int) -> Maybe a
atTable table (x, y) =
  Maybe.withDefault Nothing
    <| Maybe.map (\line -> 
      List.head <| List.drop x line)
    <| List.head
    <| List.drop y table
indexedMapTable : ((Int, Int) -> a -> b) -> (Table a) -> Table b
indexedMapTable proc table =
  List.indexedMap (\y line ->
    List.indexedMap (\x item ->
      proc (x, y) item) 
      line) 
    table

type alias Model = 
  { mode: Mode
  , table: CellTable
  }

type Mode = Playing | GameOver

init : () -> (Model, Cmd Msg)
init () =
  ( Model GameOver [[]] 
  , tableGenerater (10, 10)
  )


-- UPDATE

type Msg
  = Generate (Int, Int)
  | NewTable CellTable
  | Open (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Generate tuple ->
      ( model, tableGenerater tuple )
    NewTable table ->
      ( Model Playing table, Cmd.none )
    Open (x, y) ->
      let
        cell = atTable model.table (x, y)
        newCell = openCell model.table (x, y)
      in
      if withDefault False <| Maybe.map (\c -> c.isBomb) <| cell then
        ( { model | table = newCell, mode = GameOver }, Cmd.none)
      else
        ( { model | table = newCell }, Cmd.none )

openCell : CellTable -> (Int, Int) -> CellTable
openCell table (x, y) =
  List.indexedMap 
    (\ly line -> List.indexedMap (\lx cell -> 
      if lx==x && ly==y then
        { cell | isOpen = True }
      else
        cell
    ) line)
    table

type alias CellTableParent = Table Bool
tableGenerater : (Int, Int) -> Cmd Msg
tableGenerater tuple =
  case tuple of
    (x, y) ->
      let
        tableParent = 
          Random.list y <| Random.list x cellParentGenerator
      in
        Random.generate NewTable 
        <| Random.map (\table -> 
          indexedMapTable (countCellSurroundBombs table) table)
        <| tableParent

cellParentGenerator : Random.Generator Bool
cellParentGenerator =
  Random.map (\n -> if n==0 then True else False)
    <| Random.int 0 3

countCellSurroundBombs : CellTableParent -> (Int, Int) -> Bool -> Cell
countCellSurroundBombs table (x, y) isBomb =
  let
    at = (\(ix, iy) -> withDefault False <| atTable table (x+ix, y+iy))
    surround = 
      [ at (-1, -1), at(0, -1), at(1, -1)
      , at (-1, 0),             at(1, 0)
      , at (-1, 1),  at(0, 1),  at(1, 1)
      ]
  in
    { isOpen = False
    , isBomb = isBomb
    , surroundBombsCount = List.length <| List.filter (\a -> a) surround }
  


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div 
    [ classList [("content", True), ("gameover", model.mode == GameOver)]
    ]
    [ viewTable model.table
    , div [ class "button-reset-area" ]
      [ div 
        [ onClick (Generate (10, 10)), class "button-reset" ] 
        [ text <| if model.mode == GameOver then "Retry!" else "Reset"  ]
      ]
    ]

viewTable : CellTable -> Html Msg
viewTable table =
  div [class "cells-table"]
    <| List.indexedMap viewTableLine
    <| table

viewTableLine : Int -> List Cell -> Html Msg
viewTableLine y line =
  div [class "cells-line"]
    <| List.indexedMap 
      (\x cell -> viewCell (x, y) cell)
    <| line

viewCell : (Int, Int) -> Cell -> Html Msg
viewCell tuple cell =
  div 
    [ classList 
      [ ("cell-button", True)
      , ("cell-sealed", not cell.isOpen)
      , ("cell-empty", cell.isOpen && not cell.isBomb)
      , ("cell-bomb", cell.isOpen && cell.isBomb)]
    , onClick (Open tuple)]
    [ p [] [ text <| fromInt cell.surroundBombsCount ] ]

