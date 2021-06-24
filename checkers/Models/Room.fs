module Room

open System.Net.WebSockets
open System

[<Struct>]
type Point = { mutable X: int; mutable Y: int }

[<Struct>]
type Move = { mutable From: Point; mutable To: Point }

type Cell =
    NoPiece | Red | Blue | RedQueen | BlueQueen
    member __.IsRedPiece: bool = __ = Red || __ = RedQueen    
    member __.IsBluePiece: bool = __ = Blue || __ = BlueQueen 
    member __.IsFoe (other: Cell): bool =
        __.IsRedPiece && other.IsBluePiece ||
        __.IsBluePiece && other.IsRedPiece
    member __.IsAlly (other: Cell): bool =
        __.IsRedPiece && other.IsRedPiece ||
        __.IsBluePiece && other.IsBluePiece

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
                __.Board.[point.Y].[point.X] <- NoPiece

            if not isFirst && __.Board.[point.Y].[point.X] = Red then
                __.Board.[point.Y].[point.X] <- NoPiece

    member __.ForUser (isFirst: bool): Room =
        if isFirst then __ else __.Flip()

    member __.UserPoint (isFirst: bool) (point: Point): Point =
        if isFirst then point else { X = 7 - point.X; Y = 7 - point.Y }

    member __.validatePath (isFirst: bool) (path: Point[]): bool =
        if path.Length < 2 then
            true
        else            
            let mutable valid = true
            let mutable prev = path.[0]
            let mutable prevPiece = __.Board.[prev.Y].[prev.X]
            let mutable first = prev
            let mutable firstPiece = prevPiece
            let mutable lastDir: Option<Point> = Option.None

            if prevPiece = Red || prevPiece = Blue then
                for i in 1..path.Length - 1 do
                    let current = path.[i]
                    let currentPiece = __.Board.[current.Y].[current.X]
                    let diff = getDiff prev current
                    
                    let yDir = if isFirst then -1 else 1

                    if diff.Y <> yDir || Math.Abs diff.X > 1 then
                        valid <- false

                    if prevPiece = NoPiece && currentPiece = NoPiece then
                        valid <- false
                    
                    if firstPiece.IsFoe prevPiece && firstPiece.IsFoe currentPiece then
                        valid <- false

                    if firstPiece.IsAlly currentPiece then
                        valid <- false

                    if Option.exists (fun point -> point <> diff) lastDir then
                        if prevPiece <> NoPiece then                            
                            valid <- false
                                        
                    System.Diagnostics.Trace.WriteLine(sprintf "prev %O current %O" prevPiece currentPiece)
                    System.Diagnostics.Trace.WriteLine(sprintf "diff %O valid %O" diff valid)

                    lastDir <- Some(diff)
                    prev <- current
                    prevPiece <- currentPiece
                                    
                let mutable last = path.[path.Length - 1]
                let mutable lastPiece = __.Board.[last.Y].[last.X]

                lastPiece = NoPiece && valid
            else
                for i in 1..path.Length - 1 do
                    let current = path.[i]
                    let currentPiece = __.Board.[current.Y].[current.X]
                    let diff = getDiff prev current

                    if Math.Abs diff.Y > 1 || Math.Abs diff.X > 1 then
                        valid <- false
                    
                    if firstPiece.IsFoe prevPiece && firstPiece.IsFoe currentPiece then
                        valid <- false
                    
                    if firstPiece.IsAlly currentPiece then
                        valid <- false

                    if Option.exists (fun point -> point <> diff) lastDir then
                        if prevPiece <> NoPiece then                            
                            valid <- false
                        if currentPiece = NoPiece then                            
                            valid <- false
                    
                    System.Diagnostics.Trace.WriteLine(sprintf "prev %O current %O" prevPiece currentPiece)
                    System.Diagnostics.Trace.WriteLine(sprintf "diff %O valid %O" diff valid)

                    lastDir <- Some(diff)
                    prev <- current
                    prevPiece <- currentPiece

                let mutable last = path.[path.Length - 1]
                let mutable lastPiece = __.Board.[last.Y].[last.X]

                lastPiece = NoPiece && valid
