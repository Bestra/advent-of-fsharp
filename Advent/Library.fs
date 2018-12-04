namespace Advent
open System.Collections.Generic

module Say =
    let hello name =
        printfn "Hello %s" name


module Day1 = 
    let parseEntry (e: string) = 
      int e

    let readFile = 
      let lines = System.IO.File.ReadLines("./inputs/1.txt")
      Seq.map parseEntry lines

    let part1 =
      let sum = Seq.sum readFile
      printfn "%i" sum

    type SearchState = {
      seen: Set<int>
      previous: int
    }

    let rec repeat items = 
      seq { yield! items  
            yield! repeat items }

    let rec reachedTwice (itemSeq: seq<int>) (state: SearchState) = 
      let currentFreq = Seq.head itemSeq 
      let newFreq = state.previous + currentFreq
      match Set.contains newFreq state.seen with
      | true ->
        newFreq
      | false -> 
        let nextState = {seen = Set.add newFreq state.seen; previous = newFreq}
        reachedTwice (Seq.tail itemSeq) nextState

    let part2 =
      reachedTwice (repeat [3; 3; 4; -2; -4]) {seen = Set.empty; previous = 0} |> printfn "%i"
