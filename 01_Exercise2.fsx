open System.IO
open System
open System.Text.RegularExpressions

let isDigit c = Char.IsDigit(c)

let nameToDigitChar str =
    match str with
    | "one" -> '1'
    | "two" -> '2'
    | "three" -> '3'
    | "four" -> '4'
    | "five" -> '5'
    | "six" -> '6'
    | "seven" -> '7'
    | "eight" -> '8'
    | _ -> '9'

let addDigitsForNames str =
    let patterns = [
        "one";"two";"three";"four";"five";"six";"seven";"eight";"nine"
    ]
    let matches =
        patterns
        |> List.collect (fun pattern ->
            Regex.Matches(str, pattern) |> Seq.toList)
        |> List.map (fun m ->
            (m.Index, m.Value))
        |> List.sortBy fst

    seq {
        for (index, c) in (Seq.indexed str) do
            let m = List.tryFind (fun (matchIndex, _) -> matchIndex = index) matches
            match m with
            | Some (i, m) -> yield! seq { (nameToDigitChar m); c }
            | None -> yield c
    }

let parseCalibrationValue str =
    let digits =
        str
        |> addDigitsForNames
        |> Seq.filter isDigit
    String.Concat [ Seq.head digits; Seq.last digits ]

let solve (lines: seq<string>) =
    lines
    |> Seq.map (parseCalibrationValue >> int)
    |> Seq.reduce (fun a b -> a + b)

let solveExampleInput () =
    [|
        "two1nine"
        "eightwothree"
        "abcone2threexyz"
        "xtwone3four"
        "4nineeightseven2"
        "zoneight234"
        "7pqrstsixteen"
    |] |> solve

let solvePuzzleInput () =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/inputs/01.txt" |> solve
