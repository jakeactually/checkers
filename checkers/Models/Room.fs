module Room

open System.Net.WebSockets
open System

[<Struct>]
type Point = { mutable X: int; mutable Y: int }

[<Struct>]
type Move = { mutable From: Point; mutable To: Point }

type Cell =
    None | Red | Blue | RedQueen | BlueQueen
    member __.IsRedPiece: bool = __ = Red || __ = RedQueen        
    member __.IsFoe (other: Cell): bool = __.IsRedPiece <> other.IsRedPiece
    member __.IsAlly (other: Cell): bool = __.IsRedPiece = other.IsRedPiece

let getDiff (a: Point) (b: Point) : Point =
    { X = b.X - a.X; Y = b.Y - a.Y }

type Room =
    {   Id: int
        Board: Cell[][]
        mutable Users: int list
        mutable Turn: int
        mutable Subscribers: WebSocket list }
    
    member __.Flip (): Room = {
        __ with Board = [|
            for y, row in Array.indexed __.Board do [|
                for x, _ in Array.indexed row do
                    __.Board.[7 - y].[7 - x]
            |]
        |]
    }

    member __.Swap (pointFrom: Point) (pointTo: Point): Unit =
        let a = __.Board.[pointFrom.Y].[pointFrom.X]
        let b = __.Board.[pointTo.Y].[pointTo.X]
        __.Board.[pointFrom.Y].[pointFrom.X] <- b
        __.Board.[pointTo.Y].[pointTo.X] <- a
    
    member __.Eat (isFirst: bool) (userPath: Point[]): Unit =
        for point in userPath do
            if isFirst && __.Board.[point.Y].[point.X] = Blue then
                __.Board.[point.Y].[point.X] <- None

            if not isFirst && __.Board.[point.Y].[point.X] = Red then
                __.Board.[point.Y].[point.X] <- None

    member __.ForUser (isFirst: bool): Room =
        if isFirst then __ else __.Flip()

    member __.UserPoint (isFirst: bool) (point: Point): Point =
        if isFirst then point else { X = 7 - point.X; Y = 7 - point.Y }

    member __.validatePath (isFirst: bool) (path: Point[]): bool =
        let mutable valid = true
        let mutable eated = false
        let mutable rested = true
        let mutable turned = false
        let mutable lastDir: Option<Point> = Option.None

        for i in 0..path.Length - 2 do
            let From = path.[i]
            let To = path.[i + 1]
            let diff = getDiff From To
            
            System.Diagnostics.Trace.WriteLine(sprintf "From %O To %O Diff %O" From To diff)

            if Math.Abs diff.Y > 1 then
                valid <- false

            if Option.exists (fun point -> point <> diff) lastDir then
                if eated then
                    turned <- true
                    eated <- false
                else
                    valid <- false

            let enemy = if isFirst then Blue else Red

            if __.Board.[To.Y].[To.X] = enemy then
                if rested then
                    rested <- false
                    eated <- true
                else
                    valid <- false

            if __.Board.[To.Y].[To.X] = None then
                rested <- true
                            
            System.Diagnostics.Trace.WriteLine(sprintf "i %O turned %O eated %O" i turned eated)

            if i = path.Length - 2 then
                if turned && not eated then
                    valid <- false

                if __.Board.[To.Y].[To.X] <> None then
                    valid <- false
                
                let start = path.[0]
                let player = __.Board.[start.Y].[start.X]

                if player = Red || player = Blue then                
                    System.Diagnostics.Trace.WriteLine(sprintf "len %O" path.Length)
                    if path.Length = 3 && not eated then
                        valid <- false

                    if path.Length > 3 then
                        valid <- false

            lastDir <- Some(diff)
        
        valid
