type Coach = {
    Name: string
    FormerPlayer: bool
}

type Stats = {
    Wins: int
    Losses: int
}

type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

let teams : Team list = [
    { Name = "Atlanta Hawks"; Coach = { Name = "Quin Snyder"; FormerPlayer = true }; Stats = { Wins = 2891; Losses = 2964 } }
    { Name = "New York Knicks"; Coach = { Name = "Tom Thibodeau"; FormerPlayer = false }; Stats = { Wins = 2924; Losses = 3099 } }
    { Name = "Golden State Warriors"; Coach = { Name = "Steve Kerr"; FormerPlayer = false }; Stats = { Wins = 2923; Losses = 3098 } }
    { Name = "Toronto Raptors"; Coach = { Name = "Parko Rajakovic"; FormerPlayer = true }; Stats = { Wins = 1271; Losses = 1157 } }
    { Name = "Detroit Pistons"; Coach = { Name = "Monty Williams"; FormerPlayer = true }; Stats = { Wins = 2813; Losses = 3103 } }
]

let successfulTeams =
    teams
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)
    |> List.map (fun team -> team.Name)

//printfn "Successful Teams: %A" successfulTeams


let calculateSuccessPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Losses) * 100.0

let successPercentages =
    teams
    |> List.map (fun team -> (team.Name, calculateSuccessPercentage team))

//printfn "Success Percentages: %A" successPercentages


type Cuisine = 
| Korean
| Turkish

type MovieType =
| Regular
| IMAX
| DBOX
| RegularWithSnacks
| IMAXWithSnacks
| DBOXWithSnacks

type Activity =
| BoardGame
| Chill
| Movie of MovieType
| Restaurant of Cuisine
| LongDrive of int * float


let calculateBudget activity =
   match activity with 
   | BoardGame | Chill -> 0.0
   | Movie Regular -> 12.0
   | Movie IMAX -> 17.0
   | Movie DBOX -> 20.0
   | Movie RegularWithSnacks | Movie IMAXWithSnacks | Movie DBOXWithSnacks -> 12.0 + 5.0 
   | Restaurant Korean -> 70.0
   | Restaurant Turkish -> 65.0
   | LongDrive (kilometres, fuelcharge) -> float kilometres * fuelcharge

let eveningActivity = Restaurant Turkish
let budget = calculateBudget eveningActivity
printfn "Estimated Budget: %.2f CAD" budget


