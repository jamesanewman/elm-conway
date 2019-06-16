module Grid exposing (..)

import Random

gridRows = 4
gridCols = 4

type CellState = Alive | Dead 
type alias States = List CellState
type alias Pos = (Int, Int)
type alias PosState = (Pos, CellState)
type alias PosStates = List PosState

idxToPos: Int -> Int -> Pos
idxToPos rows idx =
    (
        (idx // rows),
        (modBy rows idx)
    )

convertToPos = idxToPos gridRows

idxStateToPosState: (Int, CellState) -> PosState
idxStateToPosState (idx, state) =
    (convertToPos idx, state)

statesToPosStates: States -> PosStates
statesToPosStates states =
    let
        indexedStates = List.indexedMap Tuple.pair states
    in
        List.map idxStateToPosState indexedStates
    
hasPos: List Pos -> PosState -> Bool
hasPos positions pos =
    let
        checkPos = Tuple.first pos
    in
        List.member checkPos positions

validPos: Int -> Int -> Pos -> Bool
validPos rows cols (x, y) =
    if 
        (x < 0) || (y < 0) 
        || ( x >= cols ) || (y >= rows)
    then
        False
    else
        True


extractPositions: List Pos -> PosStates -> PosStates
extractPositions positions posstates =
    let
        isAtPosition = hasPos positions
    in
        List.filter isAtPosition posstates

convertToStates: PosStates -> States
convertToStates postates =
    List.map Tuple.second postates



isState: CellState -> PosState -> Bool
isState state postate =
    state == (Tuple.second postate)
    
isAlive = isState Alive 
isDead = isState Dead

filterStates: CellState -> PosStates -> PosStates
filterStates state states =
    let
        isTargetState = isState state
    in
        List.filter isTargetState states

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

updateState: PosState -> Int -> Int -> PosState
updateState (pos, state) alive dead =
    if alive > dead then 
        (pos, Alive)
    else 
        (pos, Dead)     

iterate: PosStates -> PosState -> PosState
iterate states cell =
    let
        neighbourList = buildNeighbourList (Tuple.first cell)
        neighbours = extractPositions neighbourList states
        aliveNeighbours = List.filter isAlive neighbours
        deadNeighbours = List.filter isDead neighbours
    in
        updateState 
            cell 
            (List.length aliveNeighbours) 
            (List.length deadNeighbours)

iterateGrid: States -> States
iterateGrid states = 
    let 
        pstates = statesToPosStates states
        updateCell = iterate pstates
    in
        List.map updateCell pstates 
        |> List.map (Tuple.second) 


createGrid: Int -> Int -> States
createGrid rows cols =
    let
        size = (rows * cols) - 1
        -- create a 0 or 1 state
        seeds = createSeeds size (generateStep initialSeed) []
    in
        -- List of dead or alives
        List.map convertToCellState seeds

convertToCellState: Int -> CellState
convertToCellState intValue =
    case intValue of

        1 -> Alive

        _ -> Dead 


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


initialSeed: Random.Seed
initialSeed = Random.initialSeed 1000

generateStep: Random.Seed -> (Int, Random.Seed)
generateStep seed = 
    Random.step (Random.int 0 1) seed
