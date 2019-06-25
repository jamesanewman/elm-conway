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

---- Index to Coordinate conversions

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
    Convert (array_index, state) to ((x,y), state)
--}
idxToPosState: Int -> (Int, CellState) -> PosState
idxToPosState rows (idx, state) =
    (idxToPos rows idx, state)


statesToPosStates: Int -> States -> PosStates
statesToPosStates rows states =
    let
        indexedStates = List.indexedMap Tuple.pair states
        convertToPosState = idxToPosState rows
    in
        List.map convertToPosState indexedStates
    
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

updateState2: CellState -> Int -> Int -> CellState
updateState2 state alive dead =
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
        newState

{--
Go through a list (that represents a grid) of PosStates
and update each one based on the update state rules
- Updated based on neighbour states

Start point
name            runs / second
Original 1      85,440,993
New 1           88,640,752
Original 5      88,266,707
New 5           88,974,565
Original 10     89,513,076
New 10          88,978,547







--}
iterate: Int -> Int -> PosStates -> PosState -> PosState
iterate rows cols states cell =
    let
        neighbourList = List.filter (validPos rows cols) (buildNeighbourList (Tuple.first cell))
        neighbours = extractPositions neighbourList states
        aliveNeighbours = List.filter isAlive neighbours
        deadNeighbours = List.filter isDead neighbours
    in
        updateState 
            cell 
            (List.length aliveNeighbours) 
            (List.length deadNeighbours) 

iterate2: Int -> Int -> PosStates -> PosState -> CellState
iterate2 rows cols states (cellPos, cellState) =
    let
        isValidPosition = validPos rows cols 
        neighbourList = List.filter isValidPosition (buildNeighbourList cellPos)
        neighbours = extractPositions neighbourList states
        (aliveNeighbours, deadNeighbours) = List.partition isAlive neighbours
    in
        updateState2 cellState  (List.length aliveNeighbours) (List.length deadNeighbours)
         
{--
Convert Grid to PosStates, update and flatten back again.
--}
iterateGrid: Int -> Int -> States -> States
iterateGrid rows cols states = 
    let 
        pstates = statesToPosStates rows states
        updateCell = iterate rows cols pstates
    in
        List.map updateCell pstates 
        |> List.map (Tuple.second) 

{--
2-3 % improvement by coverting to state in iterate function
--}
iterateGrid2: Int -> Int -> States -> States
iterateGrid2 rows cols states = 
    let 
        pstates = statesToPosStates rows states
        updateCell = iterate2 rows cols pstates
    in
        List.map updateCell pstates 
        

iterateGridTimes: Int -> Int -> Int -> States -> States
iterateGridTimes rows cols iterations states =
    case iterations of 
        0 -> 
            states
        _ -> 
            iterateGridTimes rows cols (iterations - 1) (iterateGrid rows cols states)

{-- 
Start Point very slow.
name        runs / second
Original 1  272
New 1       269


--}
iterateGridTimes2: Int -> Int -> Int -> States -> States
iterateGridTimes2 rows cols iterations states =
    let
        calcNextStateFrom = iterateGrid2 rows cols 
        iterator = (\_ currentState -> calcNextStateFrom currentState)
    in
    
        List.foldl iterator states (List.range 0 iterations)

{--
Create random number of states
    iterations = how many iterations to perform before returning the frid
--}
createGrid: Int -> Int -> Int -> Int -> States
createGrid startSeed rows cols iterations =
    let
        size = (rows * cols)
        -- create a 0 or 1 state
        seeds = createSeeds size (generateStep (initialSeed startSeed))
        -- List of dead or alives
        initialGrid = List.map convertToCellState seeds
    in
        iterateGridTimes rows cols iterations initialGrid        

createGridOpt: Int -> Int -> Int -> Int -> States
createGridOpt startSeed rows cols iterations =
    let
        size = (rows * cols)
        -- create a 0 or 1 state
        seeds = createSeeds size (generateStep (initialSeed startSeed))
        -- List of dead or alives
        initialGrid = List.map convertToCellState seeds
    in
        iterateGridTimes rows cols iterations initialGrid        



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
createSeedsOld: Int -> (Int, Random.Seed ) -> List Int -> List Int
createSeedsOld size seed seeds =
    let 
        -- Extract the seed
        newSeed = generateStep (Tuple.second seed)
        newValue = Tuple.first seed
    in
        case size of
            0 -> 
                seeds
            _ -> 
                createSeedsOld (size - 1) newSeed (List.append seeds [newValue])

{--

Destructure tuple in call 
    + val :: createSeeds2 (size - 1) newSeed
    vs createSeeds (size - 1) newSeed (List.append seeds [newValue])

    - Destructuring maybe a bit slower
name            runs / second
Original 1      6,099,462
Updated 1       7,586,293
Original 10     671,120
Updated 10      1,941,066
Original 20     208,240
Updated 20      1,044,817

--}
createSeeds: Int -> (Int, Random.Seed) -> List Int
createSeeds size (val, seed) =
    let 
        -- Extract the seed
        newSeed = generateStep seed
    in
        case size of
            0 -> 
                []
            _ -> 
                val :: createSeeds (size - 1) newSeed


{-- A intial seed to use to generate the new ones from --}
initialSeed: Int -> Random.Seed
initialSeed startValue = Random.initialSeed startValue

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

displayIterations: Int -> Int -> Int -> States -> String
displayIterations rows cols iterations initialStates =
    let 
        info = Debug.log "Iteration: " iterations
        text = textDisplay cols initialStates
        nextStates = iterateGrid rows cols initialStates
    in
        case iterations of 
            0 ->
                "Finished"
            _ ->
                displayIterations rows cols (iterations - 1) nextStates

