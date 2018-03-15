module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Zipper exposing (..)
import Tableau exposing (..)
import Editor exposing (topRenumbered)
import Formula
import Validate
import Dict


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
        , reference = { str = "1", up = Just 0 }
        , formula = Formula.parseSigned ""
        }
    , ext = Open
    }


zipperExample =
    ( tableauExample, [] )


tableauWithAlpha =
    { node =
        { id = 1
        , value = ""
        , reference = { str = "1", up = Just 0 }
        , formula = Formula.parseSigned ""
        }
    , ext =
        Alpha
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                }
            , ext = Open
            }
    }


zipperWithAlpha =
    ( tableauWithAlpha, [] )


zipperWithAplhaDown =
    ( { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            }
      , ext = Open
      }
    , [ AlphaCrumb
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
      ]
    )


zipperWithAlphaDownBetaLeft =
    ( { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            }
      , ext = Open
      }
    , [ BetaLeftCrumb { id = 1, value = "", reference = { str = "", up = Nothing }, formula = Formula.parseSigned "" }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                }
            , ext = Open
            }
      , AlphaCrumb { id = 1, value = "", reference = { str = "1", up = Just 0 }, formula = Formula.parseSigned "" }
      ]
    )


zipperOnlyAlphaOfRightBeta =
    ( { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            }
      , ext = Open
      }
    , [ AlphaCrumb { id = 1, value = "", reference = { str = "", up = Nothing }, formula = Formula.parseSigned "" }
      , BetaRightCrumb { id = 1, value = "", reference = { str = "1", up = Just 0 }, formula = Formula.parseSigned "" }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                }
            , ext = Open
            }
      ]
    )


onlyAlphaOfRightBetaRenumbered =
    { node =
        { id = 1
        , value = ""
        , reference = { str = "1", up = Just 0 }
        , formula = Formula.parseSigned ""
        }
    , ext =
        Beta
            { node =
                { id = 2
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                }
            , ext = Open
            }
            { node =
                { id = 3
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                }
            , ext =
                Alpha
                    { node =
                        { id = 4
                        , value = ""
                        , reference = { str = "", up = Nothing }
                        , formula = Formula.parseSigned ""
                        }
                    , ext = Open
                    }
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
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    }
                , ext =
                    Beta
                        { node =
                            { id = 3
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            , formula = Formula.parseSigned ""
                            }
                        , ext =
                            Alpha
                                { node =
                                    { id = 4
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    }
                                , ext = Open
                                }
                        }
                        { node =
                            { id = 5
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            , formula = Formula.parseSigned ""
                            }
                        , ext =
                            Beta
                                { node =
                                    { id = 6
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    }
                                , ext =
                                    Alpha
                                        { node =
                                            { id = 7
                                            , value = ""
                                            , reference = { str = "", up = Nothing }
                                            , formula = Formula.parseSigned ""
                                            }
                                        , ext = Open
                                        }
                                }
                                { node =
                                    { id = 8
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    }
                                , ext = Open
                                }
                        }
                }
        }


testReferenceRewriting =
    zipperExample


testReferenceRewritingResult =
    zipper
        { node =
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    }
                , ext = Open
                }
        }


fixRefsTest =
    zipper
        { node =
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    }
                , ext = Open
                }
        }


fixRefsTestResult =
    zipper
        { node =
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    }
                , ext = Open
                }
        }


gammaExampleResult =
    zipper
        { node =
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            }
        , ext =
            Gamma
                { node =
                    { id = 1
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    }
                , ext = Open
                }
                { what = "", forWhat = "x" }
        }


validateGammaExample =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x)"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T P(k)"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T P(k)"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


getValueFromResult : Result x a -> Maybe a
getValueFromResult r =
    case r of
        Ok a ->
            Just a

        Err err ->
            Nothing


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
        , test "reference rewriting"
            (\() ->
                compareZippers testReferenceRewritingResult
                    (testReferenceRewriting |> Zipper.extendAlpha |> Editor.top |> Zipper.renumber |> zipper)
            )
        , test "reference rewriting 2" (\() -> compareZippers (fixRefs fixRefsTest) fixRefsTestResult)
        , test "gamma what test"
            (\() ->
                compareZippers gammaExampleResult
                    (zipperExample |> Zipper.extendGamma |> Zipper.changeVariable "x")
            )
        , test "substitution in gamma simple"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaExample
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaExample
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaExample
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        ]



-- todo tests:
-- extend on other than open
-- implementovat kvantifikatory
