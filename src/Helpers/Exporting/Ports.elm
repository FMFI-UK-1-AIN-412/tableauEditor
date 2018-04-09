port module Helpers.Exporting.Ports exposing (..)


type alias FileReaderPortData =
    { contents : String
    , filename : String
    , jsonImporting : Bool
    , jsonImportError : String
    , jsonImportId : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FileReaderPortData -> msg) -> Sub msg
