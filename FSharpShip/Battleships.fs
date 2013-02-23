module ItWorksOnMyMachine.FSharpShip

open Microsoft.FSharp.Control

type Agent<'Msg> = MailboxProcessor<'Msg>

type ShootResult = 
    | Miss
    | Hit
    | Sunk
    | Won

type Game (shipList: (int * int) list list) = 
    let rec anyHit = function
        | (_,[]) -> false
        | (target, H :: T) ->
            match H |> List.exists (fun x -> x = target) with
            | true -> true
            | false -> anyHit (target, T)

    let delete item = Seq.where (fun x -> x <> item) >> Seq.toList

    let deleteHit (target: int * int) (shipList: (int * int) list list) =
        shipList |> List.map (fun ship -> ship |> delete target)
                 |> List.filter (fun ship -> ship <> [])

    let onCollision (target: int * int) (shipList: (int * int) list list) =
        let newList = shipList |> deleteHit target
        match newList.Length with
        | 0 -> (Won, [])
        | n when n = shipList.Length -> (Hit, newList)
        | _ -> (Sunk, newList)

    let fleet = Agent.Start(fun inbox ->
        let rec loop (shipList: (int * int) list list) = 
            async {
                let! (replyChannel: AsyncReplyChannel<ShootResult>, target) = inbox.Receive()

                match anyHit (target, shipList) with
                | true -> 
                    let (output, newList) = onCollision target shipList
                    replyChannel.Reply output
                    return! loop newList
                | false -> 
                    replyChannel.Reply Miss
                    return! loop shipList
            }
        loop shipList )

    member private this.generateShip (size, length) =
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

    member private this.generateShip (size, length, shipList) =
        let ship = this.generateShip (size, length)
        match ship |> List.exists (fun x -> anyHit (x, shipList)) with
        | true -> this.generateShip (size, length, shipList)
        | false -> ship

    member private this.generateShips size lengthList = 
        let mapFun = fun ships x -> (this.generateShip (size, x, ships)) :: ships
        lengthList |> List.fold mapFun [] 

    new () = Game([[(1,2);(2,2)];[(4,5);(4,6)]])

    new (size: int, lengthList: int list) as this = 
        let ships = this.generateShips size lengthList
        Game ships

    member this.Shoot target =
        fleet.PostAndReply (fun replyChannel -> replyChannel, target)