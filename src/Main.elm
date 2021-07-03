module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, input, p, text)
import Html.Attributes exposing (class, classList, type_, value)
import Html.Events exposing (onInput)
import Html.Events.Extra.Mouse as Mouse
import Maybe exposing (withDefault)
import Random
import String exposing (fromInt)
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


type CellMode
    = Opened
    | Marked
    | Closed


type alias Cell =
    { mode : CellMode
    , isBomb : Bool
    , surroundBombsCount : Int
    }


type alias CellTable =
    Table Cell


type alias TableSize =
    ( Int, Int )


type alias Setting =
    { size : TableSize
    , bombRate : Int
    }


type alias Model =
    { mode : Mode
    , table : CellTable
    , isSettingVisible : Bool
    , setting : Setting
    }


type Mode
    = Playing
    | GameOver
    | GameClear


init : () -> ( Model, Cmd Msg )
init () =
    ( Model Playing [ [] ] False { size = ( 10, 10 ), bombRate = 6 }
    , tableGenerater <| Setting ( 10, 10 ) 6
    )



-- UPDATE


type Msg
    = Generate
    | OpenCloseSetting Bool
    | ChangeSetting ChangeSettingKind
    | NewTable CellTable
    | Open TablePos
    | Mark TablePos


type ChangeSettingKind
    = ChangeX Int
    | ChangeY Int
    | ChangeBombRate Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model, tableGenerater model.setting )

        NewTable table ->
            ( { model | mode = Playing, table = table }, Cmd.none )

        OpenCloseSetting isSettingVisible ->
            ( { model | isSettingVisible = isSettingVisible }, Cmd.none )

        ChangeSetting kind ->
            let
                newSetting =
                    changeSetting model.setting kind
            in
            if Table.all (\c -> c.mode == Closed) model.table then
                ( { model | setting = newSetting }, tableGenerater model.setting )

            else
                ( { model | setting = newSetting }, Cmd.none )

        Open pos ->
            let
                cell =
                    Table.at model.table pos

                isBomb =
                    withDefault False <| Maybe.map .isBomb <| cell

                opneedTable =
                    openCell model.table pos

                newModel =
                    if withDefault False <| Maybe.map ((\m -> m == Marked) << .mode) <| cell then
                        model

                    else if isBomb then
                        { model | table = openOneCell model.table pos, mode = GameOver }

                    else if model.mode == Playing && Table.all (\c -> c.isBomb || c.mode == Opened) opneedTable then
                        { model | table = opneedTable, mode = GameClear }

                    else
                        { model | table = opneedTable }
            in
            ( newModel, Cmd.none )

        Mark pos ->
            ( { model | table = markCell model.table pos }, Cmd.none )


openCell : CellTable -> TablePos -> CellTable
openCell table pos =
    if withDefault True <| Maybe.map ((\m -> m == Opened) << .mode) <| Table.at table pos then
        table

    else
        let
            opnedTable =
                openOneCell table pos

            surroundBombsCount =
                Maybe.map .surroundBombsCount <| Table.at table pos
        in
        if surroundBombsCount == Just 0 then
            openSurroundCell opnedTable pos

        else
            opnedTable


openSurroundCell : CellTable -> TablePos -> CellTable
openSurroundCell table ( x, y ) =
    Table.indexedFoldl
        -- この部分なんか手続きっぽいので、ifをfillterみたいなのを使って処理させたい
        (\cell itable ( ix, iy ) ->
            let
                isSurround =
                    abs (ix - x) <= 1 && abs (iy - y) <= 1 && not (x == ix && y == iy)
            in
            if isSurround && cell.mode /= Opened then
                openCell itable ( ix, iy )

            else
                itable
        )
        table
        table


openOneCell : CellTable -> TablePos -> CellTable
openOneCell table ( x, y ) =
    -- TODO: ifの処理を書かないようにうまくやる
    Table.indexedMap
        (\( ix, iy ) cell ->
            if ix == x && iy == y then
                { cell | mode = Opened }

            else
                cell
        )
        table


markCell : CellTable -> TablePos -> CellTable
markCell table ( x, y ) =
    Table.indexedMap
        (\( ix, iy ) cell ->
            if ix == x && iy == y then
                case cell.mode of
                    Closed ->
                        { cell | mode = Marked }

                    Marked ->
                        { cell | mode = Closed }

                    Opened ->
                        cell

            else
                cell
        )
        table


type alias CellTableParent =
    Table Bool


tableGenerater : Setting -> Cmd Msg
tableGenerater { size, bombRate } =
    let
        ( x, y ) =
            size

        tableParent =
            Random.list y <| Random.list x <| cellParentGenerator bombRate
    in
    Random.generate NewTable <|
        Random.map
            (\table ->
                Table.indexedMap (countCellSurroundBombs table) table
            )
        <|
            tableParent


cellParentGenerator : Int -> Random.Generator Bool
cellParentGenerator bombRate =
    Random.map
        (\n ->
            if n == 0 then
                True

            else
                False
        )
    <|
        Random.int 0 bombRate


countCellSurroundBombs : CellTableParent -> TablePos -> Bool -> Cell
countCellSurroundBombs table pos isBomb =
    { mode = Closed
    , isBomb = isBomb
    , surroundBombsCount =
        List.length <|
            List.filter (\a -> a) <|
                List.map (\c -> withDefault False c) <|
                    Table.getSurroundItem table pos
    }


changeSetting : Setting -> ChangeSettingKind -> Setting
changeSetting setting kind =
    let
        ( ox, oy ) =
            setting.size
    in
    case kind of
        ChangeX x ->
            { setting | size = ( x, oy ) }

        ChangeY y ->
            { setting | size = ( ox, y ) }

        ChangeBombRate rate ->
            { setting | bombRate = rate }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "content", True )
            , ( "gameover", model.mode == GameOver )
            , ( "gameclear", model.mode == GameClear )
            ]
        ]
        [ div [ class "header" ]
            [ h1
                [ classList
                    [ ( "gameover", model.mode == GameOver )
                    , ( "gameclear", model.mode == GameClear )
                    ]
                ]
                [ text <|
                    case model.mode of
                        Playing ->
                            "マインスイーパー！"

                        GameOver ->
                            "ゲームオーバー！"

                        GameClear ->
                            "ゲームクリア！"
                ]
            ]
        , viewTable model.table
        , div [ class "button-area" ]
            [ div
                [ Mouse.onClick (\_ -> Generate)
                , classList [ ( "button", True ), ( "button-reset", True ) ]
                ]
                [ text <|
                    if model.mode == GameOver then
                        "Retry!"

                    else
                        "Reset"
                ]
            , div
                [ Mouse.onClick (\_ -> OpenCloseSetting <| not model.isSettingVisible)
                , classList [ ( "button", True ), ( "button-setting", True ) ]
                ]
                [ text "Setting" ]
            ]
        , viewSetting model
        ]



-- indexedMapTableを使いたいが、divの生成をする関係から使えないので、直接やっている


viewTable : CellTable -> Html Msg
viewTable table =
    div [ class "cells-table" ] <|
        List.indexedMap viewTableLine <|
            table


viewTableLine : Int -> List Cell -> Html Msg
viewTableLine y line =
    div [ class "cells-line" ] <|
        List.indexedMap
            (\x cell -> viewCell ( x, y ) cell)
        <|
            line


viewCell : TablePos -> Cell -> Html Msg
viewCell pos cell =
    div
        [ class "cell-parent"
        , Mouse.onClick (\_ -> Open pos)
        , Mouse.onContextMenu (\_ -> Mark pos)
        ]
        [ div
            [ classList
                [ ( "cell-button", True )
                , ( "cell-closed", cell.mode == Closed )
                , ( "cell-marked", cell.mode == Marked )
                , ( "cell-empty", cell.mode == Opened && not cell.isBomb )
                , ( "cell-bomb", cell.mode == Opened && cell.isBomb )
                ]
            ]
            [ p []
                [ text <|
                    if cell.isBomb || cell.surroundBombsCount == 0 then
                        ""

                    else
                        String.fromInt cell.surroundBombsCount
                ]
            ]
        ]


viewSetting : Model -> Html Msg
viewSetting model =
    let
        setting =
            model.setting

        ( x, y ) =
            setting.size

        inputNum =
            \label kind num ->
                div
                    [ class "setting-small-area" ]
                    [ p [] [ text label ]
                    , input
                        [ type_ "number"
                        , onInput
                            (\text ->
                                ChangeSetting <| kind <| withDefault num <| String.toInt text
                            )
                        , value <| String.fromInt <| num
                        , class "setting-input-number"
                        ]
                        []
                    ]
    in
    div
        [ classList [ ( "setting-area", True ), ( "setting-unvisible", not model.isSettingVisible ) ] ]
        [ inputNum "width" ChangeX x
        , inputNum "height" ChangeY y
        , inputNum "bomb-rate" ChangeBombRate setting.bombRate
        ]
