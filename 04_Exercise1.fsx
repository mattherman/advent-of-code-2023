open System.IO

type Card = {
    WinningNumbers: Set<int>
    CardNumbers: Set<int>
}

let parseCard (line: string) =
    match (line.Split([| ':' |])) with
    | [| preamble; numbers |] ->
        match (numbers.Split([| '|' |])) with
        | [| winningNumbers; cardNumbers |] ->
            let splitToInts (s: string) =
                s.Split([| ' ' |])
                |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
                |> Array.map int
                |> Set.ofArray
            { WinningNumbers = (splitToInts winningNumbers);
              CardNumbers = (splitToInts cardNumbers) }
        | _ -> failwith "Unable to parse numbers section of card"
    | _ -> failwith "Unable to parse card"

let scoreCard (card: Card) =
    let matchingNumbers = Set.intersect card.WinningNumbers card.CardNumbers
    pown 2 (matchingNumbers.Count - 1)

let solve = Array.sumBy (parseCard >> scoreCard)

let solveExampleInput () =
    let input = @"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    input.Split([|'\n'|]) |> solve

let solvePuzzleInput () =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/inputs/04.txt" |> solve