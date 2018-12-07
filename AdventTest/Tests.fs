module Tests

open System
open Xunit
open FsUnit

open Advent

module ``Day2 tests`` =
  let testInput = 
    [
    "abcdef";
    "bababc";
    "abbcde";
    "abcccd";
    "aabcdd";
    "abcdee";
    "ababab";
    ]

  [<Fact>]
  let noRepeats () =
    Day2.hasRepeats "abcdef" |> should equal (0, 0)
    Day2.hasRepeats "bababc" |> should equal (1, 1)
    Day2.hasRepeats "abbcde" |> should equal (1, 0)
    Day2.hasRepeats "abcccd" |> should equal (0, 1)
    Day2.hasRepeats "aabcdd" |> should equal (1, 0)
    Day2.hasRepeats "abcdee" |> should equal (1, 0)
    Day2.hasRepeats "ababab" |> should equal (0, 1)


  [<Fact>]
  let check () =
    Day2.checksum testInput |> should equal 12

  [<Fact>]
  let ``stringDiff works`` () =
    Day2.stringDiff "abcd" "acdd" |> should equal [false; true; true; false]

  [<Fact>]
  let onlyOneCharacterOff () =
    Day2.onlyOneCharacterOff "fghij" "fguij" |> should equal (Some "fgij")
    Day2.onlyOneCharacterOff "abc" "ccc" |> should equal None

module ``Day 3 tests`` =
  open FParsec


  let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

  [<Fact>]
  let ``parsing a claim`` () =
    
    let res = Day3.claimFromString "#1 @ 1,3: 4x4"
    let sample: Day3.Claim = {id = 1; coords = (1, 3); size = (4, 4);}

    res |> should equal sample
