module CommandHandlers

open States
open Events
open System
open Domain
open Commands
open Chessie.ErrorHandling
open Errors


let handleOpenedTab tab = function
    | ClosedTab _ -> [TabOpened tab] |> ok
    | _ -> TabAlreadyOpened |> fail

let execute state command =
    match command with
    | OpenTab tab -> handleOpenedTab tab state
    | _ -> failwith "Todo"

let evolve state command =
    match execute state command with
    | Ok (events,_) ->
        let newState = List.fold States.apply state events
        (newState, events) |> ok
    | Bad err -> Bad err
