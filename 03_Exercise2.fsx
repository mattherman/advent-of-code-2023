open System.IO
open System

type Part = {
    Symbol: char;
    Location: int * int;
}

let convertToArray (lines: string[]) =
    array2D lines

let row i (arr: 'T[,]) = arr.[i..i, *] |> Seq.cast<'T> |> Seq.toArray

let getParts schematic =
    seq {
        for i in 0..(Array2D.length1 schematic)-1 do
            for j in 0..(Array2D.length2 schematic)-1 do
                let c = Array2D.get schematic i j
                if not (Char.IsDigit(c)) && (c <> '.') then
                    yield { Symbol = c; Location = (i, j) }
    }

let getSurroundingCoordinates (x, y) maxX maxY =
    seq {
        (x - 1, y - 1)
        (x - 1, y)
        (x - 1, y + 1)
        (x, y - 1)
        (x, y + 1)
        (x + 1, y - 1)
        (x + 1, y)
        (x + 1, y + 1)
    }
    |> Seq.filter (fun (x, y) ->
        x >= 0 && x < maxX && y >= 0 && y < maxY)

let rec collectWhile getNextIndex predicate  (arr: 'a[]) (currentIndex: int) =
    if currentIndex < 0 || currentIndex >= arr.Length then
        []
    else
        let item = arr.[currentIndex]
        if predicate item then
            item :: (collectWhile  getNextIndex predicate arr (getNextIndex currentIndex))
        else
            []

let readEntireNumber schematic (x, y) =
    let collectLeft = collectWhile (fun index -> index - 1) Char.IsDigit
    let collectRight = collectWhile (fun index -> index + 1) Char.IsDigit

    let line = row x schematic
    let leftOfIndex = collectLeft line (y - 1)
    let rightOfIndex = collectRight line (y + 1)

    (List.rev leftOfIndex) @ [line.[y]] @ rightOfIndex
    |> List.toArray
    |> System.String
    |> int

let getPartNumbers schematic part =
    let candidateLocations = getSurroundingCoordinates part.Location (Array2D.length1 schematic) (Array2D.length2 schematic)

    let isDigit schematic (x, y) =
        Char.IsDigit(Array2D.get schematic x y)

    candidateLocations
    |> Seq.filter (isDigit schematic)
    |> Seq.map (readEntireNumber schematic)
    |> Seq.distinct

let solve lines =
    let schematic = convertToArray lines
    let parts = getParts schematic

    let calculateGearRatio (part: Part, partNumbers: int seq) =
        match (part, Seq.toList partNumbers) with
        | ({ Symbol = '*' }, [ a; b ]) ->
            Some (a * b)
        | _ -> None

    let gearRatios =
        parts
        |> Seq.map (fun part -> (part, getPartNumbers schematic part))
        |> Seq.map calculateGearRatio
        |> Seq.choose id

    gearRatios
    |> Seq.sum

let solveExampleInput () =
    let input = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
    input.Split([|'\n'|]) |> solve

let solvePuzzleInput () =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/inputs/03.txt" |> solve