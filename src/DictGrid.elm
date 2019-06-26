module DictGrid exposing (..)

import Random
import Dict
{--
A version using a dict as the grid.

Dictionary vs list grid (3x3)
name    runs / second       % change    goodness of fit
List:   1,619               -           98.45%
Dict:   46,833              +2791.56%   99.66%

5x5 List = 820 Dict = 14,043 +1611.98%
10x10 List = 259 Grid = 2,959 +1042.19%






--}

type CellState = Alive | Dead 
type alias States = List CellState
type alias Pos = (Int, Int)
type alias PosState = (Pos, CellState)
type alias PosStates = List PosState
type alias DictGrid = Dict.Dict (Int, Int) CellState
{--
Convert an array index to x,y position
--}
idxToPos: Int -> Int -> Pos
idxToPos rows idx = ( (idx // rows), (modBy rows idx) )

{--
    Convert (array_index, state) to ((x,y), state)
--}
idxToPosState: Int -> (Int, CellState) -> PosState
idxToPosState rows (idx, state) = (idxToPos rows idx, state)


statesToPosStates: Int -> States -> PosStates
statesToPosStates rows states =
    let
        indexedStates = List.indexedMap Tuple.pair states
        convertToPosState = idxToPosState rows
    in
        List.map convertToPosState indexedStates

{--
Take a random int (0,1) and return a state instead
--}
convertToCellState: Int -> CellState
convertToCellState intValue =
    case intValue of

        1 -> Alive

        _ -> Dead 

{-- A intial seed to use to generate the new ones from --}
initialSeed: Int -> Random.Seed
initialSeed startValue = Random.initialSeed startValue

{-- Generate new seed from a previous seed --}
generateStep: Random.Seed -> (Int, Random.Seed)
generateStep seed = 
    Random.step (Random.int 0 1) seed

createSeeds: Int -> (Int, Random.Seed) -> States
createSeeds size (val, seed) =
    let 
        -- Extract the seed
        newSeed = generateStep seed
    in
        case size of
            0 -> 
                []
            _ -> 
                (convertToCellState val) :: createSeeds (size - 1) newSeed

{-- Assuming we are a square! --}
buildGrid: Int -> Int -> DictGrid
buildGrid seed size =
    let
        convertToPos = idxToPos (round (sqrt (toFloat (size) )))
        positions = List.map convertToPos (List.range 0 (size - 1))
        startSeed = generateStep (initialSeed seed)
    in
        createSeeds size startSeed
        |> List.map2 Tuple.pair positions 
        |> Dict.fromList

{--
Use a Pos to create a simple list of the neighbour positions
--}
buildNeighbourList: Pos -> List Pos 
buildNeighbourList (x, y) =
    [
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
    ]

getState: DictGrid -> Pos -> CellState
getState grid pos = 
    let
        state = Dict.get pos grid
    in
        case state of
            Just s ->
                s
            _ ->
                -- Shouldn't happen
                Dead
    
getStates: DictGrid -> List Pos -> States  
getStates grid positions =
    let
        findState = getState grid
    in
        List.map findState positions

updateState: Int -> Int -> Maybe CellState -> Maybe CellState
updateState alive dead state =
    case state of 
        Just Alive ->
            if alive < 2 then
                Just Dead
            else if (alive == 2) || (alive == 3) then
                Just Alive
            else
                Just Dead

        Just Dead ->
            if alive == 3 then
                Just Alive
            else 
                Just Dead

        _ -> 
            Just Dead


-- Find out neighbours and update cells state
iterateCell: Pos -> DictGrid -> DictGrid
iterateCell pos grid =
    let 
        neighbours = buildNeighbourList pos
        states = getStates grid neighbours
        aliveNeighbours = List.length (List.filter ((==) Alive) states)
        deadNeighbours = (List.length states) - (aliveNeighbours)
        updateCell = updateState aliveNeighbours deadNeighbours
 
    in
        Dict.update pos updateCell grid

-- Go through each grid position and update
iterate: DictGrid -> DictGrid
iterate grid =
    let
        positions = Dict.keys grid
        --  updateCell = iterateCell grid
    in
        -- Go through each cell and update
        List.foldl iterateCell grid positions
    