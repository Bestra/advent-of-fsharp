namespace Advent
open System.Collections.Generic

module Day1 = 
    let parseEntry (e: string) = 
      int e

    let readFile () = 
      let lines = System.IO.File.ReadLines("./inputs/1.txt")
      Seq.map parseEntry lines |> Seq.toArray

    let part1 () =
      printfn "Finding part 1"
      let sum = Seq.sum (readFile ())
      sprintf "%i" sum

    type SearchState = {
      seen: HashSet<int>
      previous: int
    }

    let rec reachedTwice (array: array<int>) count arrayLength (state: SearchState) = 
      let idx = count % arrayLength
      let currentFreq = array.[idx]
      let newFreq = state.previous + currentFreq
      match state.seen.Contains newFreq with
      | true ->
        newFreq
      | false -> 
        state.seen.Add newFreq |> ignore
        let nextState = {seen = state.seen; previous = newFreq}
        reachedTwice array (count + 1) arrayLength nextState

    let part2 () =
      printfn "Finding part 2"
      let file = readFile ()
      reachedTwice file 0 (Array.length file) {seen = new HashSet<int>(); previous = 0} |> sprintf "%i"

module Day2 =
  let readFile () = 
    System.IO.File.ReadLines("./inputs/2.txt")


  let recordOccurence map item =
    match Map.tryFind item map with
    | None ->
      Map.add item 1 map
    | Some count ->
      Map.add item (1 + count) map

  let histogram seq =
      Seq.fold recordOccurence Map.empty seq

  let hasRepeats (s: string) =
    let ma =
      histogram s

    let hasCount k =
      match Seq.tryFind (fun (_, v) -> v = k) (Map.toSeq ma) with
      | Some _ -> 1
      | None -> 0

    (hasCount 2, hasCount 3)

  let checksum lines =
    let (t1, t2) =
      Seq.map hasRepeats lines
      |> Seq.fold (fun sum t -> 
        let (a, b) = sum
        let (x, y) = t
        (a + x, b + y)
      ) (0, 0)
    
    t1 * t2

  let part1 () =
    readFile () |> checksum |> sprintf "%i" 

  let stringDiff (s1: string) (s2: string) =
    Seq.map2 (<>) s1 s2


  let onlyOneCharacterOff s1 s2 =
    let diff = (stringDiff s1 s2)
    let offCount = 
      diff
      |> histogram
      |> Map.find true
    
    let removeIndex =
      match offCount with
        | 1 -> Seq.tryFindIndex (fun x -> x = true) diff
        | _ -> None

    Option.map (fun i -> s1.Remove(i, 1)) removeIndex

  /// does stuff
  let diffStringWithList string list =
    let result =
      Seq.map (fun x -> onlyOneCharacterOff string x) list
      |> Seq.tryFind (fun x ->
        match x with
        | Some _ -> true
        | None -> false
      )
    match result with
    | Some(Some(s)) -> Some s
    | _ -> None


  let findListMatch input =
    let rec findMatch string list =
      match diffStringWithList string list with
      | Some(s) -> s 
      | None -> 
        match list with
        | nextString :: rest -> findMatch nextString rest
        | _ -> failwith "empty list"

    match input with
    | firstString :: firstList -> 
      findMatch firstString firstList
    | _ -> failwith "Day 2 input is not a proper list"

  let part2 () =
    let input = readFile () |> Seq.toList
    findListMatch input
  

module Day3 =
  open FParsec
  let readFile () = 
    System.IO.File.ReadLines("./inputs/3.txt")
    

  type Claim = {
    id: int;
    coords: (int * int);
    size: (int * int)
  }

  let parseClaim: Parser<Claim, unit> =
    let tuple a b = (a, b)
    let pint = pint16 |>> (fun i -> int i)
    let str s = pstring s
    let claimId = str "#" >>. pint
    let coords = pipe2 (pint .>> str ",") (pint .>> str ":") tuple
    let size = pipe2 (pint .>> str("x")) pint tuple

    pipe3 (claimId .>> spaces .>> str "@" .>> spaces) (coords .>> spaces) size (fun claim co s -> 
      { id = claim; coords = co; size = s}
    )

  let claimFromString s = 
    match run parseClaim s with
    | Success(claim, _, _) -> claim
    | Failure(error, _, _) -> failwithf "Parsing failed: %A" error


