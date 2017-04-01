module PlaceOrderTests

open NUnit.Framework
open CafeAppTestsDSL
open Domain
open System
open States
open Commands
open Events




let tab = {Id = Guid.NewGuid(); TableNumber = 1}
let coke = Drink {
            MenuNumber = 1
            Name = "Pepsi"
            Price = 1.5m}
let order ={Tab = tab; Foods = [];Drinks =[]}


[<Test>]
let ``Cab place only drinks order`` () =
    let order = {order with Drinks = [coke]}
    Given (OpenedTab tab)
    |> When (PlaceOrder order)
    |> ThenStateShouldBe (PlacedOrder order)
    |> WithEvents [OrderPlaced order]