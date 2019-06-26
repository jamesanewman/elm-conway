module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Svg
import Svg.Attributes

import Time exposing (..)
import Task exposing (..)
import Json.Decode as Decode -- exposing (..)

import List.Extra
import Dict
import DictGrid


main =
    Browser.element
        { 
            init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
        }

type alias Model = 
    {
        grid: DictGrid.DictGrid,
        tick: Int,
        width: Int,
        height: Int
    }

init: () -> (Model, Cmd Msg)
init flags =
    let
        n = 40
        size = n * n
        seed = 1000
        startingGrid = DictGrid.buildGrid seed size
        model = Debug.log "Start Model"
            { 
                grid = startingGrid,
                tick = 1000,
                width = 400,
                height = 400
            }
    in
        ( 
            model , 
            Cmd.none
        )

type Msg
    = Iterate
    | UpdateGrid Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    -- case Debug.log "update" msg of
    case msg of
        Iterate ->
            (
                { model | grid = (DictGrid.iterate model.grid) }
                , Cmd.none
            )
        UpdateGrid time ->
            (
                { model | grid = (DictGrid.iterate model.grid) }
                , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tick of 
        0 ->
            Sub.none
        _ ->
            Time.every (toFloat model.tick) UpdateGrid
        
view : Model -> Html Msg
view model =
    div [] 
        [
            displayGrid model
        ]

displayGrid: Model -> Html Msg
displayGrid model =
    let
        states = Dict.values model.grid
        size = List.length states
        rowSize = round (sqrt (toFloat size))
        cellWidth = model.width // rowSize
        -- dummy = Debug.log "DGSize " (size, cellWidth)
        rows = List.Extra.groupsOf rowSize states
        drawRow = displayRow cellWidth
        gridSVG = List.map drawRow (List.indexedMap Tuple.pair rows)
                    |> List.concat 
    in

        Svg.svg
            [ Svg.Attributes.width (String.fromInt model.width)
            , Svg.Attributes.height (String.fromInt model.height)
            ]
            gridSVG

displayRow: Int -> (Int, DictGrid.States)  -> List (Html Msg)
displayRow size (rowNumber, states) =
    let
        columns = List.indexedMap Tuple.pair states
        showState = displayCell size rowNumber
    in
        List.map showState columns

displayCell: Int -> Int -> (Int, DictGrid.CellState) -> Html Msg
displayCell size rowNumber (columnNumber, state) =
    let
        startx = String.fromInt (columnNumber * size)
        starty = String.fromInt (rowNumber * size)
        cellSize = String.fromInt size
        color = if state == DictGrid.Alive then "blue" else "green"
        --dummy = Debug.log "cell >> " (columnNumber, rowNumber, state)
        --dummy2 = Debug.log "  >> " (cellSize)
    in
        Svg.rect
            [ Svg.Attributes.x startx
            , Svg.Attributes.y starty
            , Svg.Attributes.width cellSize
            , Svg.Attributes.height cellSize
            , Svg.Attributes.rx "5"
            , Svg.Attributes.ry "5"
            , Svg.Attributes.fill color
            ]
            []    
    