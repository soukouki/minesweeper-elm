module Main exposing (..)

import String exposing (fromInt)
import Maybe exposing (withDefault)
import Random
import Browser
import Html exposing (Html, div, p, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)

import Table exposing (Table, TablePos)

-- MAIN

main =
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type CellMode = Opened | Marked | Closed

type alias Cell = 
  { mode: CellMode
  , isBomb: Bool
  , surroundBombsCount: Int
  }
type alias CellTable = Table Cell

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
  | Open TablePos

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Generate tuple ->
      ( model, tableGenerater tuple )
    NewTable table ->
      ( Model Playing table, Cmd.none )
    Open (x, y) ->
      let
        cell = Table.at model.table (x, y)
        newTable = openCell model.table (x, y)
        isBomb = withDefault False <| Maybe.map .isBomb <| cell
        newModel = 
          if isBomb then
            { model | table = newTable, mode = GameOver }
          else
            { model | table = newTable }
      in
        ( newModel, Cmd.none )

openCell : CellTable -> TablePos -> CellTable
openCell table pos =
  if withDefault True <| Maybe.map ((\m -> m==Opened) << .mode) <| Table.at table pos then
    table
  else
    let
      opnedTable = openOneCell table pos
      surroundBombsCount = Maybe.map .surroundBombsCount <| Table.at table pos
    in
      if surroundBombsCount == Just 0 then
        openSurroundCell opnedTable pos
      else
        opnedTable

openSurroundCell : CellTable -> TablePos -> CellTable
openSurroundCell table (x, y) =
  Table.indexedFoldl -- この部分なんか手続きっぽいので、ifをfillterみたいなのを使って処理させたい
    (\cell itable (ix, iy) ->
      let
        isSurround = abs (ix-x) <= 1 && abs (iy-y) <= 1 && not (x==ix && y==iy)
      in
        if isSurround && cell.mode /= Opened then
          openCell itable (ix, iy)
        else
          itable
      ) table table

openOneCell : CellTable -> TablePos -> CellTable
openOneCell table (x, y) =
  List.indexedMap 
    (\ly line -> List.indexedMap (\lx cell -> 
      if lx==x && ly==y then
        { cell | mode = Opened }
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
          Table.indexedMap (countCellSurroundBombs table) table)
        <| tableParent

cellParentGenerator : Random.Generator Bool
cellParentGenerator =
  Random.map (\n -> if n==0 then True else False)
    <| Random.int 0 10--3

countCellSurroundBombs : CellTableParent -> TablePos -> Bool -> Cell
countCellSurroundBombs table pos isBomb =
  { mode = Closed
  , isBomb = isBomb
  , surroundBombsCount = 
    List.length 
      <| List.filter (\a -> a)
      <| List.map (\c -> withDefault False c)
      <| Table.getSurroundItem table pos }
  


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

-- indexedMapTableを使いたいが、divの生成をする関係から使えないので、直接やっている
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

viewCell : TablePos -> Cell -> Html Msg
viewCell tuple cell =
  div 
    [ classList 
      [ ("cell-button", True)
      , ("cell-sealed", cell.mode == Closed)
      , ("cell-empty", cell.mode == Opened && not cell.isBomb)
      , ("cell-bomb", cell.mode == Opened && cell.isBomb)]
    , onClick (Open tuple)]
    [ p [] [ text <| fromInt cell.surroundBombsCount ] ]

