open System.IO
open System

let isDigit c = Char.IsDigit(c)

let parseCalibrationValue str =
    let digits = Seq.filter isDigit str |> Seq.toList
    String.Concat [ Seq.head digits; Seq.last digits ]

let solve (lines: seq<string>) =
    lines
    |> Seq.map (parseCalibrationValue >> int)
    |> Seq.reduce (fun a b -> a + b)

let solveExampleInput () =
    [|
        "1abc2"
        "pqr3stu8vwx"
        "a1b2c3d4e5f"
        "treb7uchet"
    |] |> solve

let solvePuzzleInput () =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/inputs/01.txt" |> solve


