module PrepareFoodTests


open Domain
open States
open NUnit.Framework
open TestData
open CafeAppTestsDSL
open Commands
open Events

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
