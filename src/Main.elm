
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)

import Svg -- exposing (..)
import Svg.Attributes -- exposing (..)

import Time exposing (..)
import Task exposing (..)


import Grid

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
        iteration: Int,
        width: Int,
        height: Int,
        rows: Int,
        cols: Int,
        grid: Grid.States,
        autoIterate: Int,
        paused: Bool
    }

init : () -> (Model, Cmd Msg)
init flags =
    let
        r = 10
        c = 10
        iterationPause = 1000 -- 0 = none
        startSeed = 2000
        startIteration = 4
        startGrid = Grid.createGrid startSeed r c startIteration
    in 
        (
            Model startIteration 400 400 r c startGrid iterationPause False
            , Cmd.none 
        )

type Msg
    = Start
    | Stop
    | Pause
    | Iterate
    | UpdateGrid Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            (model, Cmd.none)

        Stop ->
            (model, Cmd.none)

        Pause ->
            ( {model | paused = not model.paused}, Cmd.none)

        Iterate ->
            (
                { 
                    model 
                        | grid = (Grid.iterateGrid model.rows model.cols model.grid)
                        , iteration = (model.iteration + 1) 
                },
                Cmd.none
            )

        UpdateGrid time ->
            (
                { 
                    model 
                        | grid = (Grid.iterateGrid model.rows model.cols model.grid)
                        , iteration = (model.iteration + 1) 
                },
                Cmd.none
            )        
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.autoIterate of 
        0 ->
            Sub.none
        _ ->
            case model.paused of
                True ->
                    Sub.none 
                _ -> Time.every (toFloat model.autoIterate) UpdateGrid

view : Model -> Html Msg
view model =
    div []
        [ 

            h1 
                [] 
                [ 
                    Html.text (String.fromInt model.iteration) 
                ]
            , drawGrid model model.grid
            , button [ onClick Iterate ] [ Html.text "Iterate" ]
            , button [ onClick Pause ] [ if model.paused then Html.text "Restart Iterating" else Html.text "Pause Iterating" ]
        ]  

drawGrid model grid =   
    let 
        posGrid = Grid.statesToPosStates model.rows grid
        cellWidth = model.width // model.cols
        cellHeight = model.height // model.rows
    in
        Svg.svg
            [ Svg.Attributes.width (String.fromInt model.width)
            , Svg.Attributes.height (String.fromInt model.height)
            ]
            (List.map (drawCell cellWidth cellHeight) posGrid)

drawCell: Int -> Int -> Grid.PosState -> Html Msg 
drawCell w h ((x, y), state) = 
    let
        startx = String.fromInt (x * w)
        starty = String.fromInt (y * h)
        color = case state of 
            Grid.Alive ->
                "red"
            Grid.Dead ->
                "none"
    in
    
        Svg.rect
                    [ Svg.Attributes.x startx
                    , Svg.Attributes.y starty
                    , Svg.Attributes.width (String.fromInt w)
                    , Svg.Attributes.height (String.fromInt h)
                    , Svg.Attributes.rx "5"
                    , Svg.Attributes.ry "5"
                    , Svg.Attributes.fill color
                    ]
                    []
