module Main exposing (..)

import Browser
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import Random
import String exposing (fromInt)


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
        ( { model | table = openCell model.table (x, y) }, Cmd.none )

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
          Random.list y <| Random.list x cellGenerator
      in
        Random.generate NewTable (countSurroundBombs tableParent)

cellGenerator : Random.Generator Bool
cellGenerator =
  Random.map (\n -> if n==0 then True else False)
    <| Random.int 0 3

countSurroundBombs : Random.Generator CellTableParent -> Random.Generator CellTable
countSurroundBombs gen =
  Random.map (\table -> 
    List.indexedMap (\y line ->
      List.indexedMap (\x cell -> countCellSurroundBombs table (x, y) cell) line) table) gen

countCellSurroundBombs : CellTableParent -> (Int, Int) -> Bool -> Cell
countCellSurroundBombs table (x, y) isBomb =
  let
    at = (\(ix, iy) -> Maybe.withDefault False <| atTable table (x+ix, y+iy))
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
  div [class "content"]
    [ viewTable model.table ]

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

