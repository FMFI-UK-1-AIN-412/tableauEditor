module Example exposing (..)

import Dict
import Editor exposing (topRenumbered)
import Expect exposing (Expectation)
import Formula
import Fuzz exposing (Fuzzer, int, list, string)
import Tableau exposing (..)
import Test exposing (..)
import Validate
import Zipper exposing (..)


passedTest : Test
passedTest =
    test "this test always passes" (\() -> Expect.equal True True)


passedTest1 : Test
passedTest1 =
    test "this test allways passes" (\() -> Expect.notEqual True False)


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
        , gui = defGUI
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
        , gui = { controlsShown = False }
        }
    , ext =
        Alpha
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                , gui = defGUI
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
            , gui = defGUI
            }
      , ext = Open
      }
    , [ AlphaCrumb
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            , gui = { controlsShown = False }
            }
      ]
    )


zipperWithAlphaDownBetaLeft =
    ( { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            , gui = defGUI
            }
      , ext = Open
      }
    , [ BetaLeftCrumb
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            , gui = { controlsShown = False }
            }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
      , AlphaCrumb
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            , gui = { controlsShown = False }
            }
      ]
    )


zipperOnlyAlphaOfRightBeta =
    ( { node =
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            , gui = defGUI
            }
      , ext = Open
      }
    , [ AlphaCrumb
            { id = 1
            , value = ""
            , reference = { str = "", up = Nothing }
            , formula = Formula.parseSigned ""
            , gui = { controlsShown = False }
            }
      , BetaRightCrumb
            { id = 1
            , value = ""
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned ""
            , gui = { controlsShown = False }
            }
            { node =
                { id = 1
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                , gui = defGUI
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
        , gui = { controlsShown = False }
        }
    , ext =
        Beta
            { node =
                { id = 2
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
            { node =
                { id = 3
                , value = ""
                , reference = { str = "", up = Nothing }
                , formula = Formula.parseSigned ""
                , gui = { controlsShown = False }
                }
            , ext =
                Alpha
                    { node =
                        { id = 4
                        , value = ""
                        , reference = { str = "", up = Nothing }
                        , formula = Formula.parseSigned ""
                        , gui = defGUI
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
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Beta
                        { node =
                            { id = 3
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            , formula = Formula.parseSigned ""
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Alpha
                                { node =
                                    { id = 4
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    , gui = defGUI
                                    }
                                , ext = Open
                                }
                        }
                        { node =
                            { id = 5
                            , value = ""
                            , reference = { str = "", up = Nothing }
                            , formula = Formula.parseSigned ""
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Beta
                                { node =
                                    { id = 6
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Alpha
                                        { node =
                                            { id = 7
                                            , value = ""
                                            , reference = { str = "", up = Nothing }
                                            , formula = Formula.parseSigned ""
                                            , gui = defGUI
                                            }
                                        , ext = Open
                                        }
                                }
                                { node =
                                    { id = 8
                                    , value = ""
                                    , reference = { str = "", up = Nothing }
                                    , formula = Formula.parseSigned ""
                                    , gui = defGUI
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
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = defGUI
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
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = defGUI
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
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = defGUI
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
            , gui = { controlsShown = False }
            }
        , ext =
            Gamma
                { node =
                    { id = 1
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = defGUI
                    }
                , ext = Open
                }
                { term = "", var = "x" }
        }


getValueFromResult : Result x a -> Maybe a
getValueFromResult r =
    case r of
        Ok a ->
            Just a

        Err err ->
            Nothing


validateGammaSubstituteFunction =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(f(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(f(x))"
            , gui = { controlsShown = False }
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T P(f(Diana))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T P(f(Diana))"
                    , gui = defGUI
                    }
                , ext = Open
                }
                { term = "f(Diana)", var = "x" }
        }


validateGammaNewVariableSimilarToExistingFreeAbove =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T \\exists p Z(p, f(k))"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T \\exists p Z(p, f(k))"
                            , gui = defGUI
                            }
                        , ext = Open
                        }
                        { term = "k", var = "z" }
                }
        }


validateGammaNewVariableSimilarToExistingBoundAbove =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T P(z, k)"
                            , reference = { str = "1", up = Just 2 }
                            , formula = Formula.parseSigned "T P(z, k)"
                            , gui = defGUI
                            }
                        , ext = Open
                        }
                        { term = "z", var = "x" }
                }
        }


validateRenumberingAdding =
    zipper
        { node =
            { id = 1
            , value = "T (a \\/ b)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T (a \\/ b)"
            , gui = { controlsShown = True }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T (a /\\ (b \\/ c))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T (a /\\ b)"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Alpha
                        { node =
                            { id = 3
                            , value = "T (b \\/ c)"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T (b \\/ c)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Beta
                                { node =
                                    { id = 4
                                    , value = "T a"
                                    , reference = { str = "1", up = Just 3 }
                                    , formula = Formula.parseSigned "T a"
                                    , gui = defGUI
                                    }
                                , ext = Open
                                }
                                { node =
                                    { id = 4
                                    , value = "T b"
                                    , reference = { str = "1", up = Just 3 }
                                    , formula = Formula.parseSigned "T b"
                                    , gui = defGUI
                                    }
                                , ext = Open
                                }
                        }
                }
        }


validateRenumberingAddingResult =
    zipper
        { node =
            { id = 1
            , value = "T (a \\/ b)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T (a \\/ b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = ""
                    , reference = { str = "", up = Nothing }
                    , formula = Formula.parseSigned ""
                    , gui = { controlsShown = True }
                    }
                , ext =
                    Alpha
                        { node =
                            { id = 3
                            , value = "T (a /\\ (b \\/ c))"
                            , reference = { str = "3", up = Just 0 }
                            , formula = Formula.parseSigned "T (a /\\ b)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Alpha
                                { node =
                                    { id = 4
                                    , value = "T (b \\/ c)"
                                    , reference = { str = "3", up = Just 1 }
                                    , formula = Formula.parseSigned "T (b \\/ c)"
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Beta
                                        { node =
                                            { id = 5
                                            , value = "T a"
                                            , reference = { str = "1", up = Just 4 }
                                            , formula = Formula.parseSigned "T a"
                                            , gui = defGUI
                                            }
                                        , ext = Open
                                        }
                                        { node =
                                            { id = 6
                                            , value = "T b"
                                            , reference = { str = "1", up = Just 4 }
                                            , formula = Formula.parseSigned "T b"
                                            , gui = defGUI
                                            }
                                        , ext = Open
                                        }
                                }
                        }
                }
        }


validateRenumberingDeleting =
    zipper
        { node =
            { id = 1
            , value = "T (a\\/b)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T (a\\/b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T (b/\\(c\\/a))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T (b/\\(c\\/a))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Alpha
                        { node =
                            { id = 3
                            , value = "Tb"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T b"
                            , gui = { controlsShown = True }
                            }
                        , ext =
                            Alpha
                                { node =
                                    { id = 4
                                    , value = "T (c\\/a)"
                                    , reference = { str = "2", up = Just 2 }
                                    , formula = Formula.parseSigned "T (c\\/a)"
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Beta
                                        { node =
                                            { id = 5
                                            , value = "Tc"
                                            , reference = { str = "4", up = Just 1 }
                                            , formula = Formula.parseSigned "Tc"
                                            , gui = { controlsShown = True }
                                            }
                                        , ext = Open
                                        }
                                        { node =
                                            { id = 6
                                            , value = "Ta"
                                            , reference = { str = "4", up = Just 1 }
                                            , formula = Formula.parseSigned "Ta"
                                            , gui = { controlsShown = True }
                                            }
                                        , ext = Open
                                        }
                                }
                        }
                }
        }


validateRenumberingDeletingResult =
    zipper
        { node =
            { id = 1
            , value = "T (a\\/b)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T (a\\/b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T (b/\\(c\\/a))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T (b/\\(c\\/a))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Alpha
                        { node =
                            { id = 3
                            , value = "T (c\\/a)"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T (c\\/a)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Beta
                                { node =
                                    { id = 4
                                    , value = "Tc"
                                    , reference = { str = "3", up = Just 1 }
                                    , formula = Formula.parseSigned "Tc"
                                    , gui = { controlsShown = True }
                                    }
                                , ext = Open
                                }
                                { node =
                                    { id = 5
                                    , value = "Ta"
                                    , reference = { str = "3", up = Just 1 }
                                    , formula = Formula.parseSigned "Ta"
                                    , gui = { controlsShown = True }
                                    }
                                , ext = Open
                                }
                        }
                }
        }


suiteZipper : Test
suiteZipper =
    describe "The Zipper module"
        [ test "compare simple zippers" (\() -> compareZippers (zipper tableauExample) zipperExample)
        , test "compare extended alpha" (\() -> compareZippers (extendAlpha zipperExample) zipperWithAlpha)
        , test "compare extended alpha after going down once" (\() -> compareZippers (down (extendAlpha zipperExample)) zipperWithAplhaDown)
        , test "compare zippers: extend alpha, go down, extend beta"
            (\() -> compareZippers (left (extendBeta (down (extendAlpha zipperExample)))) zipperWithAlphaDownBetaLeft)
        , test "compare zippers, extend beta, go right, extend alpha, go down"
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
        , test "gamma term test"
            (\() ->
                compareZippers gammaExampleResult
                    (zipperExample |> Zipper.extendGamma |> Zipper.changeVariable "x")
            )
        , test "substitute function in gamma "
            (\() ->
                Expect.equal
                    (Validate.isNewVariableValid
                        (validateGammaSubstituteFunction
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                            |> Dict.values
                            |> List.head
                            |> Maybe.withDefault (Formula.Fun "f" [ Formula.Var "x" ])
                            |> Formula.strTerm
                        )
                        (validateGammaSubstituteFunction
                            |> Zipper.down
                        )
                    )
                    False
            )
        , test "substitution in gamma - substitute for existing bound above"
            (\() ->
                Expect.equal
                    (Validate.isNewVariableValid
                        (validateGammaNewVariableSimilarToExistingBoundAbove
                            |> Zipper.down
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                            |> Dict.values
                            |> List.head
                            |> Maybe.withDefault (Formula.Var "z")
                            |> Formula.strTerm
                        )
                        (validateGammaNewVariableSimilarToExistingBoundAbove
                            |> Zipper.down
                            |> Zipper.down
                        )
                    )
                    True
            )
        , test "substitution in gamma - substitute for existing free above"
            (\() ->
                Expect.equal
                    (Validate.isNewVariableValid
                        (validateGammaNewVariableSimilarToExistingFreeAbove
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                            |> Dict.values
                            |> List.head
                            |> Maybe.withDefault (Formula.Var "k")
                            |> Formula.strTerm
                        )
                        (validateGammaNewVariableSimilarToExistingFreeAbove
                            |> Zipper.down
                            |> Zipper.down
                        )
                    )
                    False
            )
        , test "renumbering justs when adding a node"
            (\() ->
                Expect.equal
                    validateRenumberingAddingResult
                    (validateRenumberingAdding
                        |> Zipper.extendAlpha
                        |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding
                        |> topRenumbered
                        |> zipper
                    )
            )
        , test "renumbering justs when deleting a node"
            (\() ->
                Expect.equal
                    validateRenumberingDeletingResult
                    (validateRenumberingDeleting
                        |> Zipper.down
                        |> Zipper.down
                        |> Zipper.deleteMe
                        |> renumberJustInReferences Zipper.renumberJustInRefWhenDeleting
                        |> topRenumbered
                        |> zipper
                    )
            )
        ]
