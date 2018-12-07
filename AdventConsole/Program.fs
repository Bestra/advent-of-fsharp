// Learn more about F# at http://fsharp.org

open System
open Argu

type CLIArguments = 
 | Day of day:int
 | Part of part:int
with
 interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "specify which date (1-25)"
            | Part _ -> "specify part 1 or part 2"

let parser = ArgumentParser.Create<CLIArguments>(programName = "advent.exe")
let usage = parser.PrintUsage()

open Advent
let runDay day part =
  match (day, part) with
  | (1, 1) -> Day1.part1 ()
  | (1, 2) -> Day1.part2 ()
  | (2, 1) -> Day2.part1 ()
  | (2, 2) -> Day2.part2 ()
  | _ -> "That day is not implemented yet"


[<EntryPoint>]
let main argv =
    let results = parser.ParseCommandLine argv

    let day = results.GetResult Day
    let part = results.GetResult Part
    runDay day part |> printfn "%s"
    0 // return an integer exit code
