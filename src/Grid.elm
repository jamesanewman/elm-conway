module Grid exposing (..)

import Random

gridRows = 4
gridCols = 4

type CellState = 
    Alive
    | Dead 

-- Position will hold x,y position
type alias Position = (Int, Int)

type alias Cell = 
    {
        pos: Position,
        state: CellState
    }

type alias Model = 
    {
        rows: Int,
        cols: Int,
        grids: List Cell
    }

model = 
    {
        rows = gridRows,
        cols = gridCols,
        grid = (createGrid gridRows gridCols)
    } 

convertIndexToPosition: Int -> Int -> Int -> Position
convertIndexToPosition rows cols index =
    let
        col = modBy rows index
        row = (//) index rows 
    in
        (col, row)

createSeeds: Int -> (Int, Random.Seed ) -> List Int -> List Int
createSeeds size seed seeds =
    let 
        -- Extract the seed
        newSeed = generateStep (Tuple.second seed)
        newValue = Tuple.first seed
    in
        case size of
            0 -> 
                seeds
            _ -> 
                createSeeds (size - 1) newSeed (List.append seeds [newValue])

-- createSeededRecord: List Position -> List Cell
createSeededRecord positions =
    let 
        bob = positions
    in
        List.map (\pos -> { pos = pos }) positions

convertIndexedState: Int -> Int -> (Int , CellState) -> Cell
convertIndexedState rows cols (idx, state) =
    { pos= convertIndexToPosition rows cols idx, state = state}

createGrid: Int -> Int -> List Cell
createGrid rows cols =
    let
        size = (rows * cols) - 1
        -- create a 0 or 1 state
        seeds = createSeeds size (generateStep initialSeed) []
        -- List of dead or alives
        states = List.map convertToCellState seeds
        -- Turn into tuples (idx, state)
        indexedStates = List.indexedMap Tuple.pair states

        indexConvertor = convertIndexedState rows cols
    in
        List.map indexConvertor indexedStates

convertToCellState: Int -> CellState
convertToCellState intValue =
    case intValue of

        1 -> Alive

        _ -> Dead 

initialSeed: Random.Seed
initialSeed = Random.initialSeed 1000

generateStep: Random.Seed -> (Int, Random.Seed)
generateStep seed = 
    Random.step (Random.int 0 1) seed
