open ItWorksOnMyMachine.FSharpShip

[<EntryPoint>]
let main argv = 
    let g = new Game ()
    let res1 = g.Shoot (1,2) 
    let res2 = g.Shoot (2,2)   
    0
