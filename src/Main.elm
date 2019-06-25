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

import Grid

main =
    Browser.element
        { 
            init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
        }

-- 
type alias HistoryItem = 
    {
        seed: Int,
        iteration: Int,
        rows: Int,
        cols: Int,
        grid: Grid.States
    }

addToHistory: Model -> Model
addToHistory model =
    let
        msg = Debug.log "Current Restore point " model.currentRestorePoint
        item = HistoryItem model.startSeed model.iteration model.rows model.cols model.grid
        history = item :: model.history
    in
        { model | history = history}

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
        paused: Bool,
        history: List HistoryItem,
        currentRestorePoint: (Int, Int)
    }

init : () -> (Model, Cmd Msg)
init flags =
    let
        r = 50
        c = 50
        iterationPause = 1000 -- 0 = none
        startSeed = 9900
        startIteration = 4
        startGrid = Grid.createGrid startSeed r c startIteration
        newModel = Model startSeed startIteration 400 400 r c startGrid iterationPause False [] (startSeed, startIteration)
    in 
        (
            addToHistory newModel
            , Cmd.none 
        )



type Msg
    = Start
    | Stop
    | Save
    | Pause
    | SaveRestorePoint String
    | Restore
    | Iterate
    | UpdateGrid Time.Posix
    | UpdateIterationPause String
    | UpdateSeed String

updateMsg: Model -> a -> Model
updateMsg model msg =
    let
        x = Debug.log ">> " msg
    in
        model
    
stringToInt: String -> Int
stringToInt str = 
    let
        asInt = String.toInt str
    in
        case asInt of 
            Just i ->
                i
            
            _ ->
                0
createRestorePoint: Int -> Int -> String 
createRestorePoint startSeed iteration =
    ((String.fromInt startSeed) ++ ("_" ++ (String.fromInt iteration)))

isTargetRestorePoint: Int -> Int -> HistoryItem -> Bool
isTargetRestorePoint seed iteration history =
    if seed == history.seed && iteration == history.iteration then
        True
    else 
        False

addRestorePoint: Model -> String -> Model 
addRestorePoint model historyId =
    let
        info = String.split "_" historyId
        
    in
        case info of 
            [seed, iteration] ->
                { model | currentRestorePoint = ((Maybe.withDefault 0 (String.toInt seed)), (Maybe.withDefault 0 (String.toInt iteration)))}

            _ -> 
                model

updateModelWithHistory: Model -> HistoryItem -> Model 
updateModelWithHistory model history =
    let
        updatedModel =
            { 
                model
                | startSeed = history.seed
                , iteration = history.iteration
                , grid = history.grid
            }
        
    in
        updatedModel

restoreModel: Model -> Model 
restoreModel model =
    let
        (seed, iteration) = model.currentRestorePoint
        matches = isTargetRestorePoint seed iteration
        match = List.filter matches model.history
        
    in
        case match of 
            [history] ->
                updateModelWithHistory model history 

            _ ->
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
                    paused = model.paused,
                    history = [],
                    currentRestorePoint = (model.startSeed, model.iteration)
                }    
            , Cmd.none)

        Stop ->
            (model, Cmd.none)

        Save ->
            (
                addToHistory model,
                Cmd.none
            )
        Pause ->
            ( {model | paused = not model.paused}, Cmd.none)

        Restore ->
            (
                restoreModel model ,
                Cmd.none
            )
        SaveRestorePoint historyId ->
            (
                (addRestorePoint model historyId),
                Cmd.none
            )

        UpdateSeed seedString ->
            (
                { model | startSeed = (stringToInt seedString)}
                , Cmd.none
            )

        UpdateIterationPause pauseString ->
            
            (
                { model | autoIterate = (stringToInt pauseString)},
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
            h2 [] [ Html.text "History"],
            select 
                [
                    onInput SaveRestorePoint
                ]
                (
                    List.map (
                        \h -> 
                            let
                              id = createRestorePoint h.seed h.iteration  
                            in                           
                                option 
                                    [ 
                                        value id
                                    ] 
                                    [
                                        Html.text id
                                    ]
                    ) 
                    model.history
                )
            , button [ onClick Restore ] [ Html.text "Rerun history" ]
            , hr [] []
            , p [] [ Html.text ("Iteration: " ++ (String.fromInt model.iteration)) ]
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
            , p 
                []
                [
                    text "Start Seed: ",
                    input 
                        [
                            attribute "type" "number"
                            ,attribute "value" (String.fromInt model.startSeed)
                            , attribute "step" "1000"
                            , onInput UpdateSeed  -- on "blur" onBlurWithTargetValue
                                
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
           , button [ onClick Save ] [ Html.text "Save" ]
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
