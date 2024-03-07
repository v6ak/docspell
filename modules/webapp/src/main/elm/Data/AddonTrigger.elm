{-
   Copyright 2020 Eike K. & Contributors

   SPDX-License-Identifier: AGPL-3.0-or-later
-}


module Data.AddonTrigger exposing (..)

-- A copy of docspell.addons.AddonTrigger.scala


type AddonTrigger
    = FinalProcessItem
    | FinalReprocessItem
    | Scheduled
    | ExistingItem
    | FailedAttachment


all : List AddonTrigger
all =
    [ FinalProcessItem
    , FinalReprocessItem
    , Scheduled
    , ExistingItem
    , FailedAttachment
    ]


asString : AddonTrigger -> String
asString t =
    case t of
        FinalProcessItem ->
            "final-process-item"

        FinalReprocessItem ->
            "final-reprocess-item"

        Scheduled ->
            "scheduled"

        ExistingItem ->
            "existing-item"

        FailedAttachment ->
            "failed-attachment"


fromString : String -> Maybe AddonTrigger
fromString s =
    let
        name =
            String.toLower s

        x =
            List.filter (\e -> asString e == name) all
    in
    List.head x


fromList : List String -> List AddonTrigger
fromList list =
    List.filterMap fromString list
