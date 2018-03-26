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


validateGammaSimple =
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


validateGammaSimpleDisjunction =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x (P(x) | P(z))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x (P(x) | P(z))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T (P(k) | P(z))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T (P(k) | P(z))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaSimpleDisjunction2 =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x (P(x) | R(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x (P(x) | R(x))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T (P(k) | R(k))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T (P(k) | R(k))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaBinaryWithInnerQuantifier =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x (P(x) | \\forall z P(z))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x (P(x) | \\forall z P(z))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T (P(k) | \\forall z P(z))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T (P(k) | \\forall z P(z))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaWithNonStartingVariable =
    zipper
        { node =
            { id = 1
            , value = "T \\forall z (P(x) | \\forall x P(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall z (P(x) | \\forall x P(x))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T \\forall z (P(k) | \\forall x P(k))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T \\forall z (P(k) | \\forall x P(k))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaExistingVariable =
    zipper
        { node =
            { id = 1
            , value = "T \\forall z (P(z) | P(k))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall z (P(z) | P(k))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T (P(k) | P(k))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T (P(k) | P(k))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "z" }
        }


validateGammaQuantifiedSameVariable =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x (P(x) | \\forall x R(x))"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x (P(x) | \\forall x R(x))"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T (P(k) | \\forall x R(k))"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T (P(k) | \\forall x R(k))"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


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


validateGammaTwoQuantifiersInTheBeginning =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x \\exists y P(x,y)"
            , reference = { str = "1", up = Just 0 }
            , formula = Formula.parseSigned "T \\forall x \\exists y P(x,y)"
            }
        , ext =
            Gamma
                { node =
                    { id = 2
                    , value = "T \\exists y P(k,y)"
                    , reference = { str = "1", up = Just 1 }
                    , formula = Formula.parseSigned "T \\exists y P(k,y)"
                    }
                , ext = Open
                }
                { what = "k", forWhat = "x" }
        }


validateGammaNewVariableSimilarAsBound =
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



--TODO: nema vypisat chybu uz pri pisani tretej formuly?


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



--validateGammaNewVariableSimilarToExistingAbove


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
        , test "substitution in gamma simple"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaSimple
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaSimple
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaSimple
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma simple disjunction"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaSimpleDisjunction
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaSimpleDisjunction
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaSimpleDisjunction
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma simple disjunction 2"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaSimpleDisjunction2
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaSimpleDisjunction2
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaSimpleDisjunction2
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma with inner quantifier"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaBinaryWithInnerQuantifier
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaBinaryWithInnerQuantifier
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaBinaryWithInnerQuantifier
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma with bad quantified variable in the start of string"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaWithNonStartingVariable
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaWithNonStartingVariable
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaWithNonStartingVariable
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    False
            )
        , test "substitution in gamma with existing variable"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaExistingVariable
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaExistingVariable
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaExistingVariable
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    False
            )
        , test "substitution in gamma with variable quantified more times"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaQuantifiedSameVariable
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaQuantifiedSameVariable
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaQuantifiedSameVariable
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    False
            )
        , test "substitution of function in gamma "
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaSubstituteForFunction
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
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
        , test "substitution in gamma - two quantifiers in row in the beginning"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaTwoQuantifiersInTheBeginning
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaTwoQuantifiersInTheBeginning
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaTwoQuantifiersInTheBeginning
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    True
            )
        , test "substitution in gamma - controll all free variables (above) 1"
            (\() ->
                Expect.equal
                    (Formula.substitutionIsValid
                        (validateGammaNewVariableSimilarAsBound
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map Validate.makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        (validateGammaNewVariableSimilarAsBound
                            |> down
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                        (validateGammaNewVariableSimilarAsBound
                            |> zNode
                            |> .formula
                            |> getValueFromResult
                            |> Maybe.withDefault (Formula.T (Formula.Atom "default" []))
                        )
                    )
                    False
            )
        ]



-- todo tests:
-- extend on other than open
