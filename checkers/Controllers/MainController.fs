namespace checkers.Controllers

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open System.Text
open Microsoft.FSharp.Control
open System.Threading.Tasks
open Room
open System
open System.Net.WebSockets
open System.Threading

[<ApiController>]
[<Route("api/[action]")>]
type MainController (logger : ILogger<MainController>, context : IHttpContextAccessor) =
    inherit ControllerBase()

    static let mutable rooms = Map.empty<int, Room>
    static let mutable roomIndex = 0
    static let mutable userIndex = 0

    let helper = [None; Red; Blue; RedQueen; BlueQueen];
    
    let board =
        Data.boardData |>
        Array.map (fun row -> row |> Array.map (fun col -> helper.[col]))
  
    [<HttpGet>]
    member __.Index() : int =
        roomIndex <- roomIndex + 1
        rooms <- rooms.Add (roomIndex, {
            Id = roomIndex
            Board = board
            Users = List.empty
            Turn = 0
            Subscribers = List.empty
         })
        roomIndex

    [<HttpGet("{id}")>]
    member __.Room(id: int) : ObjectResult =
        if not <| rooms.ContainsKey id then
            upcast __.NotFound(())
        else
            let room = rooms.[id]
            let userId = context.HttpContext.Session.GetInt32("userId").GetValueOrDefault(-1)

            if room.Users |> List.contains userId then
                let isFirst = room.Users.[0] = userId
                upcast __.Ok(room.ForUser isFirst)
            else
                if room.Users.Length >= 2 then
                    upcast __.Unauthorized("Room is already playing")
                else if userId = -1 then                
                    userIndex <- userIndex + 1
                    room.Users <- List.append room.Users [userIndex]
                    context.HttpContext.Session.SetInt32("userId", userIndex)
                    let isFirst = room.Users.[0] = userIndex
                    upcast __.Ok(room.ForUser isFirst)
                else
                    room.Users <- List.append room.Users [userId]
                    let isFirst = room.Users.[0] = userId
                    upcast __.Ok(room.ForUser isFirst)

    [<HttpPost("{id}")>]
    member __.Turn(id: int, path: Point[]) : Async<ObjectResult> = async {
        let room = rooms.[id]
        let userId = context.HttpContext.Session.GetInt32("userId").GetValueOrDefault(-1)
        let isFirst = room.Users.[0] = userId
        let userPath = Array.map (room.UserPoint isFirst) path
        let From = userPath.[0]
        let To = userPath.[userPath.Length - 1]
        
        if room.Turn % 2 >= room.Users.Length || room.Users.[room.Turn % 2] <> userId then
            return upcast __.Unauthorized("Not your turn")         
        else if helper.[room.Turn % 2 + 1].IsFoe room.Board.[From.Y].[From.X] then
            return upcast __.BadRequest("You can't move enemy's pieces")
        else if not <| room.validatePath isFirst userPath then
            return upcast __.BadRequest("Invalid move")
        else
            room.Swap From To
            room.Turn <- room.Turn + 1
            room.Eat isFirst userPath

            let goal = if isFirst then 0 else 7

            if To.Y = goal then
                let queen = if isFirst then RedQueen else BlueQueen
                room.Board.[To.Y].[To.X] <- queen

            for ws in room.Subscribers do                
                let serverMsg = Encoding.UTF8.GetBytes("update")
                let! _ = ws.SendAsync(new ArraySegment<byte>(serverMsg, 0, serverMsg.Length), WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
                ()               

            return upcast __.Ok(room.ForUser isFirst)
    }

    [<HttpGet("{id}")>]
    member __.State(id: int): Async<ObjectResult> = async {
        let userId = context.HttpContext.Session.GetInt32("userId").GetValueOrDefault(-1)
        
        if not <| rooms.ContainsKey id then
            return upcast __.NotFound(())
        else
            let room = rooms.[id]

            if room.Users |> List.contains userId |> not then
                return upcast __.Unauthorized(())         
            else if context.HttpContext.WebSockets.IsWebSocketRequest then
                let! webSocket = Async.AwaitTask <| context.HttpContext.WebSockets.AcceptWebSocketAsync()
                room.Subscribers <- webSocket :: room.Subscribers
                let tsc = new TaskCompletionSource<unit>()
                let! _ = Async.AwaitTask tsc.Task
                return upcast __.Ok(())
            else
                return upcast __.NotFound(())
    }
