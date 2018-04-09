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


getValueFromResult : Result x a -> Maybe a
getValueFromResult r =
    case r of
        Ok a ->
            Just a

        Err err ->
            Nothing


validateGammaSubstituteForFunction =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(f(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(f(x))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T P(f(Diana))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T P(f(Diana))"
                    }
                , ext = Open
                }
                { what = "Diana", forWhat = "x" }
        }



--todo


validateGammaSubstituteFunction =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(f(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(f(x))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T P(f(Diana))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T P(f(Diana))"
                    }
                , ext = Open
                }
                { what = "f(Diana)", forWhat = "x" }
        }


validateGammaNewVariableSimilarToBound =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x \\exists k P(k, x)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x \\exists k P(k, x)"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T \\exists k P(k, k)"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T \\exists k P(k, k)"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaNewVariableSimilarToExistingFree =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T \\exists p Z(p, f(k))"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T \\exists p Z(p, f(k))"
                            }
                        , ext = Open
                        }
                        { what = "k", forWhat = "z" }
                }
        }


validateGammaNewVariableSimilarToExistingFree2 =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T P(z, k)"
                            , reference = { str = "1", up = Just 2 }
                            , formula = Formula.parseSigned "T P(z, k)"
                            }
                        , ext = Open
                        }
                        { what = "z", forWhat = "x" }
                }
        }


validateGammaNewVariableSimilarToExistingFree3 =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T \\exists p Z(p, f(k))"
                            , reference = { str = "2", up = Just 1 }
                            , formula = Formula.parseSigned "T \\exists p Z(p, f(k))"
                            }
                        , ext = Open
                        }
                        { what = "k", forWhat = "z" }
                }
        }


validateGammaNewVariableSimilarToExistingFree4 =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    }
                , ext =
                    Gamma
                        { node =
                            { id = 3
                            , value = "T P(k, k)"
                            , reference = { str = "1", up = Just 2 }
                            , formula = Formula.parseSigned "T P(k, k)"
                            }
                        , ext = Open
                        }
                        { what = "k", forWhat = "x" }
                }
        }



--TODO: nema vypisat chybu uz pri pisani tretej formuly? -- nie, ale keby su tam 2 rovnake volne, tak je problem
-- todo tests:
-- extend on other than open


validateParsingTheory =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x P(x, k)"
            }
        , ext =
            Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , reference = { str = "2", up = Just 0 }
                    , formula = Formula.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    }
                , ext =
                    Alpha
                        { node =
                            { id = 3
                            , value = "F \\exists k \\forall p L(k, f(p))"
                            , reference = { str = "3", up = Just 0 }
                            , formula = Formula.parseSigned "F \\exists k \\forall p L(k, f(p))"
                            }
                        , ext = Open
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
        , test "substitution of function in gamma "
            (\() ->
                Expect.equal
                    (Validate.isNewVariableValid
                        (validateGammaSubstituteForFunction
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                            |> Dict.values |>
                        )
                        (validateGammaSubstituteForFunction
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaSubstituteForFunction
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma - controll all bound variables"
            (\() ->
                Expect.equal
                    (Formula.removeQuantifierAndSubstitute
                        (validateGammaNewVariableSimilarToBound
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarToBound
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarToBound
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    False
            )
        , test "substitution in gamma - controll all free variables"
            (\() ->
                Expect.equal
                    (Formula.removeQuantifierAndSubstitute
                        (validateGammaNewVariableSimilarToExistingFree
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarToExistingFree
                            |> down
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarToExistingFree
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        && Validate.isNewVariableValid
                            (validateGammaNewVariableSimilarToExistingFree
                                |> Zipper.down
                                |> Zipper.zSubstitution
                                |> Maybe.map Validate.makeS
                                |> Maybe.withDefault (Dict.fromList [])
                                |> Dict.values
                                |> List.head
                                |> Maybe.withDefault (Formula.Var "default")
                                |> Formula.strTerm
                            )
                            (validateGammaNewVariableSimilarToExistingFree |> Zipper.down |> Zipper.down)
                    )
                    False
            )
        , test "substitution in gamma - controll all free variables 2"
            (\() ->
                Expect.equal
                    (Formula.removeQuantifierAndSubstitute
                        (validateGammaNewVariableSimilarToExistingFree2
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarToExistingFree2
                            |> down
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarToExistingFree2
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        && Validate.isNewVariableValid
                            (validateGammaNewVariableSimilarToExistingFree2
                                |> Zipper.down
                                |> Zipper.zSubstitution
                                |> Maybe.map Validate.makeS
                                |> Maybe.withDefault (Dict.fromList [])
                                |> Dict.values
                                |> List.head
                                |> Maybe.withDefault (Formula.Var "default")
                                |> Formula.strTerm
                            )
                            (validateGammaNewVariableSimilarToExistingFree2 |> Zipper.down |> Zipper.down)
                    )
                    False
            )
        , test "substitution in gamma - controll all free variables 3"
            (\() ->
                Expect.equal
                    (Formula.removeQuantifierAndSubstitute
                        (validateGammaNewVariableSimilarToExistingFree3
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarToExistingFree3
                            |> down
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarToExistingFree3
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        && Validate.isNewVariableValid
                            (validateGammaNewVariableSimilarToExistingFree3
                                |> Zipper.down
                                |> Zipper.zSubstitution
                                |> Maybe.map Validate.makeS
                                |> Maybe.withDefault (Dict.fromList [])
                                |> Dict.values
                                |> List.head
                                |> Maybe.withDefault (Formula.Var "default")
                                |> Formula.strTerm
                            )
                            (validateGammaNewVariableSimilarToExistingFree3 |> Zipper.down |> Zipper.down)
                    )
                    False
            )
        , test "substitution in gamma - controll all free variables 4"
            (\() ->
                Expect.equal
                    (Formula.removeQuantifierAndSubstitute
                        (validateGammaNewVariableSimilarToExistingFree4
                            |> Zipper.down
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarToExistingFree4
                            |> down
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarToExistingFree4
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        && Validate.isNewVariableValid
                            (validateGammaNewVariableSimilarToExistingFree4
                                |> Zipper.down
                                |> Zipper.zSubstitution
                                |> Maybe.map Validate.makeS
                                |> Maybe.withDefault (Dict.fromList [])
                                |> Dict.values
                                |> List.head
                                |> Maybe.withDefault (Formula.Var "default")
                                |> Formula.strTerm
                            )
                            (validateGammaNewVariableSimilarToExistingFree4 |> Zipper.down |> Zipper.down)
                    )
                    False
            )
        ]
