module PrepareFoodTests


open Domain
open States
open NUnit.Framework
open TestData
open CafeAppTestsDSL
open Commands
open Events
open Errors

[<Test>]
let ``Can prepare food`` () =
    let order = {order with Foods = [salad]}
    let ExcpectedIpo = 
        {
            PlacedOrder = order
            ServedDrinks = []
            ServedFoods = []
            PreparedFoods = [salad]
        }
    Given (PlacedOrder order)
    |> When (PrepareFood (salad,order.Tab.Id))
    |> ThenStateShouldBe (OrderInProgress ExcpectedIpo)
    |> WithEvents ([FoodPrepared (salad,order.Tab.Id)])


[<Test>]
let ``Can not prepare food with closed tab`` () =
    let order = {order with Foods = [salad]}
    Given (ClosedTab None)
    |> When (PrepareFood (salad,order.Tab.Id))
    |> ShouldFailWith CanNotPrepareWithClosedTab

[<Test>]
let ``Can not prepare for non ordered food`` () =    
    let order = {order with Foods = [salad]}
    Given (PlacedOrder order)
    |> When (PrepareFood (pizza,order.Tab.Id))
    |> ShouldFailWith (CanNotPrepareNonOrderedFood pizza)

[<Test>]
let ``Can not prepare for non placed order`` () =
    let order = {order with Foods = [salad]}
    Given (OpenedTab order.Tab)
    |> When (PrepareFood (salad,order.Tab.Id))
    |> ShouldFailWith CanNotPrepareForNonPlacedOrder

