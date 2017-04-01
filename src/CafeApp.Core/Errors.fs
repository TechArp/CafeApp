module Errors

type Error =
    | TabAlreadyOpened
    | CanNotOrderWithClosedTab
    | CanNotPlaceEmptyOrder
    | OrderAlreadyPlaced