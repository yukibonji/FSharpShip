module ItWorksOnMyMachine.FSharpShip

open Microsoft.FSharp.Control

type Agent<'Msg> = MailboxProcessor<'Msg>

type ShootResult = 
    | Miss
    | Hit
    | Sunk
    | Won

type Game (shipList: (int * int) list list) = 
    let delete item = 
        Seq.where (fun x -> x <> item) >> Seq.toList

    let deleteHit target =
        let isNotSunk = (<>) []
        List.map (delete target) >> List.filter isNotSunk

    let onCollision (target: int * int) (shipList: (int * int) list list) =
        let newList = shipList |> deleteHit target
        match newList.Length with
        | 0 -> (Won, [])
        | n when n = shipList.Length -> (Hit, newList)
        | _ -> (Sunk, newList)

    static member private anyHit target shipList =
        let isShipHit = List.exists (fun x -> x = target)
        shipList |> List.exists isShipHit

    member private this.fleet = Agent.Start(fun inbox ->
        let rec loop (shipList: (int * int) list list) = 
            async {
                let! (replyChannel: AsyncReplyChannel<ShootResult>, target) = inbox.Receive()

                match Game.anyHit target shipList with
                | true -> 
                    let (output, newList) = onCollision target shipList
                    replyChannel.Reply output
                    return! loop newList
                | false -> 
                    replyChannel.Reply Miss
                    return! loop shipList
            }
        loop shipList )

    static member private generateShip (size, length) =
        let rnd = System.Random()
        let horizontal = rnd.Next 2 = 1
        let constant = rnd.Next size
        let vary = rnd.Next (size - length)
        let dots = [vary..vary + length - 1]
        let mapYFun = fun y -> (constant, y)
        let mapXFun = fun x -> (x, constant)
        if horizontal then
            dots |> List.map mapYFun
        else
            dots |> List.map mapXFun

    static member private generateLegalShip (size, length, shipList) =
        let ship = Game.generateShip (size, length)
        match ship |> List.exists (fun x -> Game.anyHit x shipList) with
        | true -> Game.generateLegalShip (size, length, shipList)
        | false -> ship

    static member private generateShips size lengthList = 
        let mapFun = fun ships x -> (Game.generateLegalShip (size, x, ships)) :: ships
        lengthList |> List.fold mapFun [] 

    new () = Game([[(1,2);(2,2)];[(4,5);(4,6)]])

    new (size: int, lengthList: int list) as this =
        let ships = Game.generateShips size lengthList
        Game ships

    member this.Shoot target =
        this.fleet.PostAndReply (fun replyChannel -> replyChannel, target)