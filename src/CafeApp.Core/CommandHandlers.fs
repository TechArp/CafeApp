module CommandHandlers

open States
open Events
open System
open Domain
open Commands
open Chessie.ErrorHandling
open Errors


let handleOpenTab tab = function
    | ClosedTab _   -> [TabOpened tab] |> ok
    | _             -> TabAlreadyOpened |> fail

let handlePlaceOrder order  = function
    | OpenedTab _   -> 
        if List.isEmpty order.Foods && List.isEmpty order.Drinks then
            fail CanNotPlaceEmptyOrder
        else
            [OrderPlaced order] |> ok
    | ClosedTab _   -> CanNotOrderWithClosedTab |> fail
    | PlacedOrder _ -> OrderAlreadyPlaced |> fail 
    | _             -> CanNotOrderWithClosedTab |> fail

let handleServeDrink drink tabId = function
    | PlacedOrder order     -> [DrinkServed (drink,tabId)]  |> ok
    | _                     -> CanNotOrderWithClosedTab     |> fail


let execute state command =
    match command with
    | OpenTab tab                     -> handleOpenTab tab state
    | PlaceOrder order                -> handlePlaceOrder order state
    | ServeDrink (drink,tabId)        -> handleServeDrink drink tabId state
    | _                               -> failwith "Todo"

let evolve state command =
    match execute state command with
    | Ok (events,_) ->
        let newState = List.fold States.apply state events
        (newState, events) |> ok
    | Bad err -> Bad err
