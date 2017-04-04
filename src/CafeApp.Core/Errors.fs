module Errors

open Domain

type Error =
    | TabAlreadyOpened
    | CanNotOrderWithClosedTab
    | CanNotPlaceEmptyOrder
    | OrderAlreadyPlaced
    | CanNotServeNonOrderedDrink of Drink
    | OrderAlreadyServed
    | CanNotServeForNonPlacedOrder
    | CanNotServeWithClosedTab
    | CanNotPrepareNonOrderedFood of Food
    | CanNotPrepareForNonPlacedOrder
    | CanNotPrepareWithClosedTab