
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Svg
import Svg.Attributes

import Time exposing (..)
import Task exposing (..)
import Json.Decode as Decode -- exposing (..)

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
        startSeed: Int,
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
            Model startSeed startIteration 400 400 r c startGrid iterationPause False
            , Cmd.none 
        )



type Msg
    = Start
    | Stop
    | Pause
    | Iterate
    | UpdateGrid Time.Posix
    | UpdateIterationPause String

updateMsg: Model -> a -> Model
updateMsg model msg =
    let
        x = Debug.log ">> " msg
    in
        model
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            (
                {
                    startSeed = model.startSeed,
                    iteration = 1,
                    width = model.width,
                    height = model.height,
                    rows = model.rows,
                    cols = model.rows,
                    grid = Grid.createGrid model.startSeed model.rows model.cols 1,
                    autoIterate = model.autoIterate,
                    paused = model.paused
                }    
            , Cmd.none)

        Stop ->
            (model, Cmd.none)

        Pause ->
            ( {model | paused = not model.paused}, Cmd.none)

        UpdateIterationPause x ->
            
            (
                updateMsg model x,
                Cmd.none
            )
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

        -- UpdateIterationPause interval ->
        --     (
        --         {model | autoIterate = interval}
        --         , Cmd.none
        --     )

           
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
            headerPanel
            , div
                [

                ]
                [
                    div 
                        [
                            style "outline" "1px solid black"
                        ]
                        [
                            drawGrid model model.grid
                        ]

                    , div 
                        [

                        ]
                        [
                            displayModel model
                        ]
                ]
            , controlPanel model
        ]  

onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Decode.map tagger targetValue)


displayModel: Model -> Html Msg 
displayModel model =
    div 
        [
            style "float" "right"
        ]
        [
            p [] [ Html.text ("Iteration: " ++ (String.fromInt model.iteration)) ]
            , p 
                []
                [
                    text "Iteration pause: ",
                    input 
                        [
                            attribute "type" "number"
                            ,attribute "value" (String.fromInt model.autoIterate)
                            , attribute "step" "1000"
                            , onInput UpdateIterationPause  -- on "blur" onBlurWithTargetValue
                                
                        ]
                        []
                ]
        ]



headerPanel: Html Msg 
headerPanel = 
    h1 
        [] 
        [ 
            Html.text "Game of life" 
        ]

controlPanel: Model -> Html Msg
controlPanel model = 
    div
        []
        [
            button [ onClick Start ] [ Html.text "Start" ]
            , button [ onClick Iterate ] [ Html.text "Iterate" ]
            , button [ onClick Pause ] [ if model.paused then Html.text "Restart Iterating" else Html.text "Pause Iterating" ]
        ]
drawGrid: Model -> Grid.States -> Html Msg
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
