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
      Seq.map parseEntry lines |> Seq.toArray

    let part1 =
      printfn "Finding part 1"
      let sum = Seq.sum readFile
      printfn "%i" sum

    type SearchState = {
      seen: HashSet<int>
      previous: int
    }

    let rec reachedTwice (array: array<int>) count arrayLength (state: SearchState) = 
      let idx = count % arrayLength
      let currentFreq = array.[idx]
      let newFreq = state.previous + currentFreq
      printfn "%i, %i items seen" newFreq state.seen.Count
      match state.seen.Contains newFreq with
      | true ->
        newFreq
      | false -> 
        state.seen.Add newFreq |> ignore
        let nextState = {seen = state.seen; previous = newFreq}
        reachedTwice array (count + 1) arrayLength nextState

    let part2 =
      printfn "Finding part 2"
      reachedTwice readFile 0 (Array.length readFile) {seen = new HashSet<int>(); previous = 0} |> printfn "%i"
