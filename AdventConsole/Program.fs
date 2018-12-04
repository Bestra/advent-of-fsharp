// Learn more about F# at http://fsharp.org

open System
open Argu

type CLIArguments = 
 | Day of day:string

open Advent
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Day1.part1 |> ignore
    Day1.part2 |> ignore
    0 // return an integer exit code
