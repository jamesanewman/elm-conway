module Grid exposing (..)

import Random

{--
Conway game of life.  Something simple to look at elm/FP techniques.
Just playing around to find the best ways of doing some things.
--}
gridRows = 4
gridCols = 4

type CellState = Alive | Dead 
type alias States = List CellState
type alias Pos = (Int, Int)
type alias PosState = (Pos, CellState)
type alias PosStates = List PosState

{--
Convert an array index to x,y position
--}
idxToPos: Int -> Int -> Pos
idxToPos rows idx =
    (
        (idx // rows),
        (modBy rows idx)
    )

{-- 
Paritally applied version of idxToPos,
Probably need to do this via the model eventually
--}
convertToPos = idxToPos gridRows

{--
    Convert (array_index, state) to ((x,y), state)
--}
idxStateToPosState: (Int, CellState) -> PosState
idxStateToPosState (idx, state) =
    (convertToPos idx, state)


statesToPosStates: States -> PosStates
statesToPosStates states =
    let
        indexedStates = List.indexedMap Tuple.pair states
    in
        List.map idxStateToPosState indexedStates
    
{--
    Extract PosStates that are in a list of Pos's.
    Pull items from the grid
--}
hasPos: List Pos -> PosState -> Bool
hasPos positions pos =
    let
        checkPos = Tuple.first pos
    in
        List.member checkPos positions
{--
    Simple check that the position is on the grid 
    - within x/y coordinates
--}
validPos: Int -> Int -> Pos -> Bool
validPos rows cols (x, y) =
    if 
        (x < 0) || (y < 0) 
        || ( x >= cols ) || (y >= rows)
    then
        False
    else
        True

{--
    Partially apply the grid rows, cols values.
--}
positionInsideGrid = validPos gridRows gridCols

{--
    Given a list of posstates extract just the ones in
    a list of positions.
--}
extractPositions: List Pos -> PosStates -> PosStates
extractPositions positions posstates =
    let
        isAtPosition = hasPos positions
    in
        List.filter isAtPosition posstates

{--
    Change PosStates to States
--}
convertToStates: PosStates -> States
convertToStates postates =
    List.map Tuple.second postates


{--
Generic is state Alive or Dead check
--}
isState: CellState -> PosState -> Bool
isState state postate =
    state == (Tuple.second postate)

{-- 
Specialised version of isState 
--}
isAlive = isState Alive 
{--
Specialised version of the isState check
--}
isDead = isState Dead

{--
Extract only specific state tuples from a list of PosStates
--}
filterStates: CellState -> PosStates -> PosStates
filterStates state states =
    let
        isTargetState = isState state
    in
        List.filter isTargetState states

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
{--
Implement the rules, change a PosState State based on
the surrounding neighbours.
--}
updateState: PosState -> Int -> Int -> PosState
updateState (pos, state) alive dead =
    let 
        newState = case state of 
            Alive ->
                if alive < 2 then
                    Dead
                else if (alive == 2) || (alive == 3) then
                    Alive
                else
                    Dead

            Dead ->
                if alive == 3 then
                    Alive
                else 
                    Dead
    in
        (pos, newState)

{--
Go through a list (that represents a grid) of PosStates
and update each one based on the update state rules
- Updated based on neighbour states
--}
iterate: PosStates -> PosState -> PosState
iterate states cell =
    let
        neighbourList = List.filter positionInsideGrid (buildNeighbourList (Tuple.first cell))
        neighbours = extractPositions neighbourList states
        aliveNeighbours = List.filter isAlive neighbours
        deadNeighbours = List.filter isDead neighbours
    in
        updateState 
            cell 
            (List.length aliveNeighbours) 
            (List.length deadNeighbours)
{--
Convert Grid to PosStates, update and flatten back again.
--}
iterateGrid: States -> States
iterateGrid states = 
    let 
        pstates = statesToPosStates states
        updateCell = iterate pstates
    in
        List.map updateCell pstates 
        |> List.map (Tuple.second) 

{--
Create random number of states
--}
createGrid: Int -> Int -> States
createGrid rows cols =
    let
        size = (rows * cols)
        -- create a 0 or 1 state
        seeds = createSeeds size (generateStep initialSeed) []
    in
        -- List of dead or alives
        List.map convertToCellState seeds

{--
Take a random int (0,1) and return a state instead
--}
convertToCellState: Int -> CellState
convertToCellState intValue =
    case intValue of

        1 -> Alive

        _ -> Dead 

{--
Generate list of seed ints (0,1) for the grid
--}
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


{-- A intial seed to use to generate the new ones from --}
initialSeed: Random.Seed
initialSeed = Random.initialSeed 1000

{-- Generate new seed from a previous seed --}
generateStep: Random.Seed -> (Int, Random.Seed)
generateStep seed = 
    Random.step (Random.int 0 1) seed

displayRow states = Debug.log "" (Debug.toString states)

textDisplay: Int -> States -> String
textDisplay cols states = 
    let
        row = List.take cols states 
        rest = List.drop cols states
    in
        case row of
            [] -> 
                ""
            _ ->
                displayRow row ++ (textDisplay cols rest)

displayIterations: Int -> Int -> States -> String
displayIterations cols iterations initialStates =
    let 
        info = Debug.log "Iteration: " iterations
        text = textDisplay cols initialStates
        nextStates = iterateGrid initialStates
    in
        case iterations of 
            0 ->
                "Finished"
            _ ->
                displayIterations cols (iterations - 1) nextStates

