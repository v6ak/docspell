{-
   Copyright 2020 Eike K. & Contributors

   SPDX-License-Identifier: AGPL-3.0-or-later
-}


module Data.ItemSelection exposing
    ( ItemSelection(..)
    , isActive
    , isSelected
    )

import Data.ItemIds exposing (ItemIds)
import Data.SelectedItems exposing (SelectedItems)


type ItemSelection
    = Inactive
    | Active SelectedItems


isSelected : String -> ItemSelection -> Bool
isSelected id set =
    case set of
        Inactive ->
            False
        Active ids ->
            Data.SelectedItems.isMember ids id


isActive : ItemSelection -> Bool
isActive sel =
    case sel of
        Active _ ->
            True

        Inactive ->
            False
