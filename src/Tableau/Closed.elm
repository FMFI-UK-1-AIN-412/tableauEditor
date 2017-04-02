module Tableau.Closed exposing (..)
import Tableau exposing (..)
import Formula exposing (Formula, Signed)
import Validate exposing (..)
import Errors exposing (..)


second = curry Tuple.second

isClosed : Zipper -> Result (List Problem) Bool
isClosed z =
  case zTableau z of
    Alpha _ st ->
      Errors.merge2 second
        (isCorrectNode z)
        (isClosed (down z))
    Beta _ lt rt ->
      Errors.merge3 (\_ b c -> b && c)
        (isCorrectNode z)
        (isClosed (left z))
        (isClosed (right z))
    Leaf _ Nothing ->
      isCorrectNode z |> Result.map (always False)
    Leaf _ (Just (r1, r2)) ->
      isCorrectNode z |> Result.map (always True)

assumptions : Zipper -> List (Signed Formula)
assumptions z =
  (++)
   ((Maybe.map2 second
       ((zNode z).ref.up
         |> Maybe.andThen (\x -> if (x==0) then Just () else Nothing)
       )
       (z
         |> zFormula
         |> Result.toMaybe
       )
     )
     |> Maybe.map List.singleton
     |> Maybe.withDefault []
   )
   (List.concatMap assumptions (children z))

{- vim: set sw=2 ts=2 sts=2 et : -}
