module Helpers.Parser exposing (deadEndsToString, deadEndsToStrings)


import Dict exposing (Dict)
import Parser
import Set


type alias Problems =
    { expecting: List String
    , expectingKeyword: List String
    , expectingSymbol: List String
    , other: List String
    }


noProblems : Problems
noProblems =
    { expecting = []
    , expectingKeyword = []
    , expectingSymbol = []
    , other = []
    }


type alias Matrix r c a =
    Dict r (Dict c a)


deadEndsToStrings : List Parser.DeadEnd -> List String
deadEndsToStrings =
    matrixToStrings
        << problemsMatrixToStringMatrix
        << deadEndsToProblemsMatrix


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString =
    String.join "." << deadEndsToStrings


deadEndsToProblemsMatrix : List Parser.DeadEnd -> Matrix Int Int Problems
deadEndsToProblemsMatrix =
    List.foldl
        (\{ row, col, problem } ->
            updateMatrix
                row
                col
                (Just
                    << addProblemToProblems problem
                    << Maybe.withDefault noProblems)
        )
        Dict.empty


problemsMatrixToStringMatrix : Matrix Int Int Problems -> Matrix Int Int String
problemsMatrixToStringMatrix =
    Dict.map (\_ cd -> Dict.map (\_ ps -> problemsToString ps) cd)


matrixToStrings : Matrix Int Int String -> List String
matrixToStrings =
    Dict.foldr
        (\r row pstrs ->
            List.foldr
                (\s psts1 ->
                    ("Row " ++ String.fromInt r ++ ", " ++ s) :: psts1)
                pstrs
                (rowToStrings row))
        []


rowToStrings : Dict Int String -> List String
rowToStrings =
    Dict.foldr
        (\c ps pstrs ->
            ("column " ++ String.fromInt c ++ ": " ++ ps) :: pstrs)
        []


updateMatrix :
    comparable -> comparable -> (Maybe a -> Maybe a) ->
    Matrix comparable comparable a -> Matrix comparable comparable a
updateMatrix r c update =
    Dict.update
        r
        (Just
            << Dict.update c update
            << Maybe.withDefault Dict.empty)

problemsToString : Problems -> String
problemsToString ps =
    let
        expectations =
            revAlternativesToMaybeString
                <| List.reverse ps.expecting
                    ++ expectingKindToString "keyword" ps.expectingKeyword
                    ++ expectingKindToString "symbol" ps.expectingSymbol
    in
        String.join "; "
            <| ((Maybe.withDefault []
                    <| Maybe.map (\s -> ["expecting " ++ s]) expectations)
                ++ List.reverse ps.other)


expectingKindToString : String -> List String -> List String
expectingKindToString kind syms =
    case List.map (\sym -> "‘" ++ sym ++ "’")
            <| Set.toList <| Set.fromList syms of
        [] ->
            []

        [ qsym ] ->
            [ kind ++ " " ++ qsym ]

        qsyms ->
            [ "one of " ++ kind ++ "s "
                ++ Maybe.withDefault "" (revAlternativesToMaybeString qsyms)
            ]


revAlternativesToMaybeString : List String -> Maybe String
revAlternativesToMaybeString alts =
    case alts of
        [] ->
            Nothing

        [ alt ] ->
            Just alt

        [ alt1, alt2 ] ->
            Just (alt2 ++ " or " ++ alt1)

        alt1 :: morealts ->
            Just ((String.join ", " <| List.reverse <| morealts)
                    ++ ", or " ++ alt1)


addProblemToProblems : Parser.Problem -> Problems -> Problems
addProblemToProblems p ps =
    case p of
        Parser.Expecting exp ->
            { ps | expecting = exp :: ps.expecting }
        Parser.ExpectingInt ->
            { ps | expecting = "an integer" :: ps.expecting }
        Parser.ExpectingHex ->
            { ps | expecting = "a hexadecimal number" :: ps.expecting }
        Parser.ExpectingOctal ->
            { ps | expecting = "an octal number" :: ps.expecting }
        Parser.ExpectingBinary ->
            { ps | expecting = "a binary number" :: ps.expecting }
        Parser.ExpectingFloat ->
            { ps | expecting = "a floating point number" :: ps.expecting }
        Parser.ExpectingNumber ->
            { ps | expecting = "a number" :: ps.expecting }
        Parser.ExpectingVariable ->
            { ps | expecting = "an identifier" :: ps.expecting }
        Parser.ExpectingSymbol sym ->
            { ps | expectingSymbol = sym :: ps.expectingSymbol }
        Parser.ExpectingKeyword kw ->
            { ps | expectingKeyword = kw :: ps.expectingKeyword }
        Parser.ExpectingEnd ->
            { ps | expecting = "end of input" :: ps.expecting }
        Parser.UnexpectedChar ->
            { ps | other = "unexpected character" :: ps.other }
        Parser.Problem prob ->
            { ps | other = prob :: ps.other }
        Parser.BadRepeat ->
            { ps | other = "bad repeat" :: ps.other }
