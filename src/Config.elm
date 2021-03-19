module Config exposing (..)

import Dict exposing (Dict)

type alias Config = 
    Dict String Bool

    
defConfig : Config
defConfig = 
    let
        rules = ["α", "β","γ","δ","Reflexivity","Leibnitz", "MP","MT","Cut","HS","DS","NCS","ESTT","ESTF","ESFT","ESFF","ECDT","ECDF"]
    in
    Dict.fromList <| List.map (\r -> (r,False)) rules