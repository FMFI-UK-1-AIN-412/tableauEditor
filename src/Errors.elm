module Errors exposing (..)

{-| Like `Result.map2` but merges errors (which must be lists).
-}
merge2 : (a -> b -> value) -> Result (List x) a -> Result (List x) b -> Result (List x) value
merge2 func ra rb =
    case (ra,rb) of
      (Ok a, Ok b) -> Ok (func a b)
      (Err xa, Err xb) -> Err (xa ++ xb)
      (Err x, _) -> Err x
      (_, Err x) -> Err x

{-|-}
merge3 : (a -> b -> c -> value) -> Result (List x) a -> Result (List x) b -> Result (List x) c -> Result (List x) value
merge3 func ra rb rc =
    case (ra,rb,rc) of
      (Ok a, Ok b, Ok c) -> Ok (func a b c)
      _ -> Err (errors ra ++ errors rb ++ errors rc)

errors : Result (List x) a -> List x
errors r =
  case r of
    Err x -> x
    Ok _ -> []

{-| Like `Result.mapError` but directly maps through the list
-}
map : (x -> y) -> Result (List x) a -> Result (List y) a
map f result =
    case result of
      Ok v -> Ok v
      Err es -> Err (List.map f es)

{-| An instance of Haskell's mapM for Result.
    Maps an operation that can fail over elements of a list,
    returning either the left-most error or the list of good results.
-}
mapResult : (a -> Result x b) -> List a -> Result x (List b)
mapResult f =
  List.foldr (Result.map2 (::) << f) (Ok [])

{- vim: set sw=2 ts=2 sts=2 et : -}
