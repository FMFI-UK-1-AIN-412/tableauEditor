module Helper exposing (..)

import Zipper


hasReference : Zipper.Zipper -> Bool
hasReference z =
    ((Zipper.zNode z).reference.str == "") && ((Zipper.zNode z).value /= "")


isPremise : Zipper.Zipper -> Bool
isPremise z =
    ((Zipper.zNode z).id |> toString) == (Zipper.zNode z).reference.str
