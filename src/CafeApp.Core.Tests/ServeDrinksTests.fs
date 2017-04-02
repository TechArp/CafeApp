module ServeDrinksTests


open Domain
open States
open Commands
open Events
open CafeAppTestsDSL
open NUnit.Framework
open TestData
open Errors


[<Test>]
let ``Can Serve Drink`` () =
    let order = {order with Drinks = [coke;lemonade]}
    let expected = {
        PlacedOrder = order
        ServedDrinks = [coke]
        PreparedFoods = []
        ServedFoods = []}
    Given (PlacedOrder order)
    |> When (ServeDrink (coke, order.Tab.Id))
    |> ThenStateShouldBe (OrderInProgress expected)
    |> WithEvents [DrinkServed (coke, order.Tab.Id)]


[<Test>]
let ``Can not serve a non ordered drink`` () =
    let order = {order with Drinks = [coke]}
    Given (PlacedOrder order)
    |> When (ServeDrink (appleJuice, order.Tab.Id))
    |> ShouldFailWith (CanNotServeNonOrderedDrink appleJuice)
    

[<Test>]
let ``Can not serve drink for already served order`` () =
    Given (ServedOrder order)
    |> When (ServeDrink (coke, order.Tab.Id))
    |> ShouldFailWith OrderAlreadyServed


[<Test>]
let ``Can not serve drink for non placed order`` () =
    Given (OpenedTab tab)
    |> When (ServeDrink (coke, tab.Id))
    |> ShouldFailWith CanNotServeForNonPlacedOrder


[<Test>]
let ``Can not serve drink for closed tab`` () =
    Given (ClosedTab None)
    |> When (ServeDrink (coke, tab.Id))
    |> ShouldFailWith CanNotServeWithClosedTab

[<Test>]
let ``Can serve drink for order containing only one drink`` () =
    let order = {order with Drinks = [coke]}
    let payment = {Tab = order.Tab; Amount = drinkPrice coke}

    Given (PlacedOrder order)
    |> When (ServeDrink (coke, order.Tab.Id))
    |> ThenStateShouldBe (ServedOrder order)
    |> WithEvents [
        DrinkServed (coke,order.Tab.Id)    
        OrderServed (order,payment)
        ] 