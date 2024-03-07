{-
   Copyright 2020 Eike K. & Contributors

   SPDX-License-Identifier: AGPL-3.0-or-later
-}


module Data.Environment exposing (..)

import Browser.Navigation as Nav
import Data.Flags exposing (Flags)
import Data.ItemIds exposing (ItemIds)
import Data.SelectedItems exposing (SelectedItems)
import Data.UiSettings exposing (UiSettings)


type alias Update =
    { key : Nav.Key
    , selectedItems : SelectedItems
    , flags : Flags
    , settings : UiSettings
    }


type alias View =
    { flags : Flags
    , sidebarVisible : Bool
    , settings : UiSettings
    , selectedItems : SelectedItems
    }
