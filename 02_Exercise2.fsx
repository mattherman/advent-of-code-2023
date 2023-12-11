open System.IO

type Cube =
    | Red
    | Blue
    | Green

type Round = {
    Cubes: (int * Cube)[]
}

type Game = {
    Id: int;
    Rounds: Round[]
}

let parseGamePrefix (str: string) =
    match str.Split([|' '|]) with
    | [|"Game"; id|] -> int id
    | _ -> failwith $"Unexpected game prefix format: {str}"

let parseCubes (str: string) =
    match str.Trim().Split([|' '|]) with
    | [| count; "blue" |] -> (int count, Blue)
    | [| count; "red" |] -> (int count, Red)
    | [| count; "green" |] -> (int count, Green)
    | _ -> failwith $"Encountered cubes of an unknown color: {str}"

let parseRound (str: string) =
    let cubes = str.Split([|','|]) |> Array.map parseCubes
    { Cubes = cubes }

let parseRounds (str: string) =
    str.Split([|';'|]) |> Array.map parseRound

let parseGame (str: string) =
    match str.Split([|':'|]) with
    | [| prefix; rounds |] ->
        let gameId = parseGamePrefix prefix
        let rounds = parseRounds rounds
        { Id = gameId; Rounds = rounds }
    | _ -> failwith $"Unable to parse game prefix and rounds: {str}"

let isRoundPossible (availableCubes: Map<Cube, int>) (round: Round) =
    round.Cubes
    |> Array.forall (fun (count, color) ->
        let possibleNumberOfCubes = Map.find color availableCubes
        count < possibleNumberOfCubes)

let isGamePossible (availableCubes: Map<Cube, int>) (game: Game) =
    game.Rounds |> Array.forall (isRoundPossible availableCubes)
    
let solve lines =
    let availableCubes = Map [|
        (Red, 12)
        (Green, 13)
        (Blue, 14)
    |]

    let possibleGames =
        lines
        |> Seq.map parseGame
        |> Seq.filter (isGamePossible availableCubes)

    Seq.fold (fun sum game -> sum + game.Id) 0 possibleGames

let solveExampleInput () =
    let input = @"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    input.Split([|'\n'|]) |> solve

let solvePuzzleInput () =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/inputs/02.txt" |> solve