module PerformanceTest exposing (..)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark.Reporting exposing (fromBenchmark)
import Benchmark exposing (benchmark, Benchmark, scale )
import Benchmark as B2
import Grid as Grid


main : BenchmarkProgram
main =
    program suite

seeds = 
    let 
        x = Debug.log "Creating Grid ... " Nothing
    in
        Grid.createGrid 1000 10 10 1

posseeds = Grid.statesToPosStates 10 seeds

iterateGridTimes size =
    [
        (("Original " ++ String.fromInt size), runIteration (Grid.iterateGridTimes 10 10 size seeds)),
        (("New " ++ String.fromInt size), runIteration (Grid.iterateGridTimes2 10 10 size seeds))
    ]

iterateGrid size =
    [
        (("Original " ++ String.fromInt size), runIteration (Grid.iterateGrid 10 10 seeds)),
        (("New " ++ String.fromInt size), runIteration (Grid.iterateGrid2 10 10 seeds))
    ]

iterate size =
    [
        (("Original " ++ String.fromInt size), runIteration (Grid.iterate 10 10 posseeds ((3,3), Grid.Alive))),
        (("New " ++ String.fromInt size), runIteration (Grid.iterate 10 10 posseeds ((3,3), Grid.Alive)))
    ]

createSeeds size = 
    [
        ( 
            ("Original " ++ String.fromInt size), 
            runIteration (Grid.createSeedsOld size (Grid.generateStep (Grid.initialSeed 1000) ) [] )
        ),
        (
            ("Updated " ++ String.fromInt size), 
            runIteration (Grid.createSeeds size (Grid.generateStep (Grid.initialSeed 1000) ) )
        )
    ]

runIteration func _ = func 

runBench size = (String.fromInt size, \_ -> Grid.createSeeds size (Grid.generateStep (Grid.initialSeed 1000)))


suite = 
    Benchmark.describe "Seed creation"
        [
            scale "State Creation" (List.concat (List.map createSeeds  [1, 10, 20]))
            , scale "Grid Iteration (Times) " (List.concat (List.map iterateGridTimes  [1, 5, 10]))
            , scale "Grid Iteration" (List.concat (List.map iterateGrid  [1, 5, 10]))
            , scale "Iteration" (List.concat (List.map iterate [1, 5, 10]))
        ]
        
--             Benchmark.compare "Create grid benchmarks"
                -- "Original: "  func1
                -- "New: "  func2