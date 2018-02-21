module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Zipper exposing (..)
import Tableau exposing (..)
import Editor exposing (topRenumbered)


passedTest : Test
passedTest =
    test "this test always passes" (\() -> (Expect.equal True True))


passedTest1 : Test
passedTest1 =
    test "this test allways passes" (\() -> (Expect.notEqual True False))


compareZippers : Zipper -> Zipper -> Expectation
compareZippers z expectedZ =
    Expect.equal z expectedZ



--TEST IMPLEMNTATIONS--


tableauExample =
    { node =
        { id = 1
        , value = ""
        , reference = { str = "", up = Nothing }
        }
    , ext = Open
    }


zipperExample =
    ( tableauExample, [] )


tableauWithAlpha =
    { node =
        { id = 1
        , value = ""
        , reference = { str = "", up = Nothing }
        }
    , ext =
        Alpha
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                }
            , ext = Open
            }
    }


zipperWithAlpha =
    ( tableauWithAlpha, [] )


zipperWithAplhaDown =
    ( tableauExample, [ AlphaCrumb { id = 1, value = "", reference = { str = "", up = Nothing } } ] )


zipperWithAlphaDownBetaLeft =
    ( tableauExample
    , [ BetaLeftCrumb { id = 1, value = "", reference = { str = "", up = Nothing } }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                }
            , ext = Open
            }
      , AlphaCrumb { id = 1, value = "", reference = { str = "", up = Nothing } }
      ]
    )


zipperOnlyAlphaOfRightBeta =
    ( tableauExample
    , [ AlphaCrumb { id = 1, value = "", reference = { str = "", up = Nothing } }
      , BetaRightCrumb { id = 1, value = "", reference = { str = "", up = Nothing } }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                }
            , ext = Open
            }
      ]
    )


onlyAlphaOfRightBetaRenumbered =
    { node = { id = 1, value = "", reference = { str = "", up = Nothing } }
    , ext =
        Beta
            { node = { id = 2, value = "", reference = { str = "", up = Nothing } }
            , ext = Open
            }
            { node = { id = 3, value = "", reference = { str = "", up = Nothing } }
            , ext =
                Alpha
                    { node = { id = 4, value = "", reference = { str = "", up = Nothing } }, ext = Open }
            }
    }


zipperOnlyAlphaOfRightBetaRenumbered =
    ( onlyAlphaOfRightBetaRenumbered, [] )


zipperZWalkPostExample =
    zipperExample |> extendAlpha |> down |> extendBeta |> left |> extendAlpha |> up |> right |> extendBeta |> left |> extendAlpha |> top |> topRenumbered |> zipper


zipperZWalkPostResult =
    zipper
        { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    }
                , ext =
                    Beta
                        { node =
                            { id = 3
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            }
                        , ext =
                            Alpha
                                { node =
                                    { id = 4
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    }
                                , ext = Open
                                }
                        }
                        { node =
                            { id = 5
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            }
                        , ext =
                            Beta
                                { node =
                                    { id = 6
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    }
                                , ext =
                                    Alpha
                                        { node =
                                            { id = 7
                                            , value = ""
                                            , reference = { str = "", up = Nothing }
                                            }
                                        , ext = Open
                                        }
                                }
                                { node =
                                    { id = 8
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    }
                                , ext = Open
                                }
                        }
                }
        }


suiteZipper : Test
suiteZipper =
    describe "The Zipper module"
        [ test "compare simple zippers" (\() -> (compareZippers (zipper tableauExample) zipperExample))
        , test "compare extended alpha" (\() -> (compareZippers (extendAlpha zipperExample) zipperWithAlpha))
        , test "compare extended alpha after going down once" (\() -> compareZippers (down (extendAlpha zipperExample)) zipperWithAplhaDown)
        , test "compare zippers: extend alpha, go down, extend beta"
            (\() -> compareZippers (left (extendBeta (down (extendAlpha zipperExample)))) zipperWithAlphaDownBetaLeft)
        , test "compare zippers, extend beta, do right, extend alpha, go down"
            (\() ->
                compareZippers
                    (zipperExample |> extendBeta |> right |> extendAlpha |> down)
                    zipperOnlyAlphaOfRightBeta
            )
        , test
            "renumber"
            (\() ->
                compareZippers
                    (zipperExample |> extendBeta |> right |> extendAlpha |> down |> top |> topRenumbered |> zipper)
                    zipperOnlyAlphaOfRightBetaRenumbered
            )
        , test "renumber another" (\() -> compareZippers zipperZWalkPostExample zipperZWalkPostResult)
        ]



-- todo tests:
-- extend on other than open
-- implementovat kvantifikatory
