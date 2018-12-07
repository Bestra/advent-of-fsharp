module Tests

open System
open Xunit
open FsUnit

open Advent

[<Fact>]
let ``My test`` () =
    Assert.True(true)

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