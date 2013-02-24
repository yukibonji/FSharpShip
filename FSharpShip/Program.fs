open ItWorksOnMyMachine.FSharpShip

[<EntryPoint>]
let main argv = 
    let g = new Game (10, [2;3;5])
    let result = g.Shoot (1,1) |> ignore
    0
