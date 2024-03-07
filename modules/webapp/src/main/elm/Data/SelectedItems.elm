module Data.SelectedItems exposing
    ( SelectedItems
    , isEmpty
    , empty
    , isMember
    )

import Data.ItemIds as ItemIds exposing (ItemIds)

type SelectedItems
    = Explicit ItemIds
    | AllMatching

empty : SelectedItems
empty =
    Explicit (ItemIds.empty)


isEmpty : SelectedItems -> Bool
isEmpty selectedItems =
    case selectedItems of
        Explicit itemIds ->
            ItemIds.isEmpty itemIds
        AllMatching ->
            False

isMember : SelectedItems -> String -> Bool
isMember selectedItems id =
    case selectedItems of
        Explicit itemIds ->
            ItemIds.isMember itemIds id
        AllMatching ->
            True
