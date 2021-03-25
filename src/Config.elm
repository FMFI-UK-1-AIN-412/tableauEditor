module Config exposing (..)

import Dict exposing (Dict)

type alias Config = 
    Dict String Bool

    
defaultConfig : Config
defaultConfig = 
    let
        rules = ["α","β","γ","δ","Reflexivity","Leibnitz", "MP","MT","Cut","HS","DS","NCS","ESTT","ESTF","ESFT","ESFF","ECDT","ECDF","γ*","δ*"]
    in
    Dict.fromList <| List.map (\r -> (r,True)) rules