module Helpers.Helper exposing (..)

import Formula exposing (Formula)
import Formula.Signed exposing (Signed)
import Result
import Tableau
import Validate
import Validation.Common
import Zipper


hasReference : Zipper.Zipper -> Bool
hasReference z =
    (List.isEmpty (Zipper.zNode z).references) && ((Zipper.zNode z).value /= "")


isPremise : Zipper.Zipper -> Bool
isPremise z =
    case (Zipper.up z) |> Zipper.zTableau |> .ext of
        Tableau.Alpha _ ->
            List.length (Zipper.zNode z).references == 0
        
        _ ->
            False
    

{-| Like `Result.map2` but merges errors (which must be lists).
-}
merge2 : (a -> b -> value) -> Result (List x) a -> Result (List x) b -> Result (List x) value
merge2 func ra rb =
    case ( ra, rb ) of
        ( Ok a, Ok b ) ->
            Ok (func a b)

        ( Err xa, Err xb ) ->
            Err (xa ++ xb)

        ( Err x, _ ) ->
            Err x

        ( _, Err x ) ->
            Err x


{-| -}
merge3 : (a -> b -> c -> value) -> Result (List x) a -> Result (List x) b -> Result (List x) c -> Result (List x) value
merge3 func ra rb rc =
    case ( ra, rb, rc ) of
        ( Ok a, Ok b, Ok c ) ->
            Ok (func a b c)

        _ ->
            Err (errors ra ++ errors rb ++ errors rc)


errors : Result (List x) a -> List x
errors r =
    case r of
        Err x ->
            x

        Ok _ ->
            []


second : a1 -> a2 -> a2
second =
    \a b -> Tuple.second ( a, b )


isClosed : Zipper.Zipper -> Result (List Validation.Common.Problem) Bool
isClosed z =
    case (Zipper.zTableau z).ext of
        Tableau.Alpha t ->
            merge2 second
                (Validate.isCorrectNode z)
                (isClosed (Zipper.down z))

        Tableau.Beta lt rt ->
            merge3 (\_ b c -> b && c)
                (Validate.isCorrectNode z)
                (isClosed (Zipper.left z))
                (isClosed (Zipper.right z))

        Tableau.Gamma t s ->
            merge2 second
                (Validate.isCorrectNode z)
                (isClosed (Zipper.down z))

        Tableau.Delta t s ->
            merge2 second
                (Validate.isCorrectNode z)
                (isClosed (Zipper.down z))

        Tableau.Refl t ->
            merge2 second
                (Validate.isCorrectNode z)
                (isClosed (Zipper.down z))

        Tableau.Open ->
            Validate.isCorrectNode z |> Result.map (always False)

        Tableau.Closed r1 r2 ->
            Validate.isCorrectNode z |> Result.map (always True)


assumptions : Zipper.Zipper -> List (Signed Formula)
assumptions z =
    (++)
        (Maybe.map2 second
            ((Zipper.zFirstRef z).up
                |> Maybe.andThen
                    (\x ->
                        if x == 0 then
                            Just ()

                        else
                            Nothing
                    )
            )
            (z
                |> Zipper.zNode
                |> .formula
                |> Result.toMaybe
            )
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
        )
        (List.concatMap assumptions (Zipper.children z))
