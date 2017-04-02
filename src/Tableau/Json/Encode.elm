module Tableau.Json.Encode exposing (jsonTableau, encode)
import Tableau exposing (Tableau(Leaf, Alpha, Beta))
import Json.Encode exposing (..)

jsonRef r =
  string r.str

jsonClosed mc =
  case mc of
    Nothing -> []
    Just (r1, r2) ->
      [ ("closed",
        list
          [ jsonRef r1
          , jsonRef r2
          ]
      ) ]

jsonNode {num, text, ref} =
  object
    [ ("num", int num)
    , ("text", string text)
    , ("ref", jsonRef ref)
    ]

jsonNodeList n =
  [ ("node", jsonNode n) ]

jsonTblList t =
  case t of
    Leaf n mc -> [ ("type", string "leaf") ] ++ jsonNodeList n
      ++ jsonClosed mc
    Alpha n st -> [ ("type", string "alpha") ] ++ jsonNodeList n
      ++ [ ("child", jsonTableau st) ]
    Beta n lt rt -> [ ("type", string "beta") ] ++ jsonNodeList n
      ++ [ ("leftChild", jsonTableau lt), ("rightChild", jsonTableau rt) ]

jsonTableau t =
  object <| jsonTblList t

encode ind t = ( Json.Encode.encode ind (jsonTableau t) ) ++ "\n"

{- vim: set sw=2 ts=2 sts=2 et : -}
