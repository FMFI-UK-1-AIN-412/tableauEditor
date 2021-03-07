module Helpers.Helper exposing (..)

import Formula exposing (Formula)
import Formula.Signed exposing (Signed)
import Result
import Tableau exposing (Extension(..), ExtType(..))
import Validation
import Validation.Common exposing (Problem, ProblemType(..))
import Zipper
import Validation.Common exposing (semanticsProblem)


hasReference : Zipper.Zipper -> Bool
hasReference z =
    List.isEmpty (Zipper.zNode z).references && ((Zipper.zNode z).value /= "")


isPremise : Zipper.Zipper -> Bool
isPremise z =
    case Zipper.up z |> Zipper.zTableau |> .ext of
        Unary Alpha _ ->
            List.length (Zipper.zNode z).references == 0

        _ ->
            Zipper.up z == z


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


isClosed : Zipper.Zipper -> Result (List Problem) Bool
isClosed z =
    case (Zipper.zTableau z).ext of
        Unary _ _ ->
            merge2 second
                (Validation.isCorrectNode z)
                (isClosed (Zipper.down z))

        UnaryWithSubst _ _ _ ->
            merge2 second
                (Validation.isCorrectNode z)
                (isClosed (Zipper.down z))

        Binary _ _ _ ->
            merge3 (\_ b c -> b && c)
                (Validation.isCorrectNode z)
                (isClosed (Zipper.left z))
                (isClosed (Zipper.right z))

        Open ->
            Validation.isCorrectNode z |> Result.map (always False)

        Closed r1 r2 ->
            Validation.isCorrectNode z |> Result.map (always True)


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
