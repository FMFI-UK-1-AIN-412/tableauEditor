module TestZipper exposing (..)

import Dict
import Editor exposing (topRenumbered)
import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Tableau exposing (..)
import Test exposing (..)
import Validation
import Validation.Common exposing(..)
import Validation.Rules.Delta exposing (..)
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
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned ""
        , gui = defGUI
        }
    , ext = Open
    }


zipperExample =
    zipper tableauExample


tableauWithAlpha =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned ""
        , gui = { controlsShown = False }
        }
    , ext =
        Unary Alpha
            { node =
                { id = 1
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
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
            , references = []
            , formula = Formula.Parser.parseSigned ""
            , gui = defGUI
            }
      , ext = Open
      }
    , [ UnaryCrumb Alpha
            { id = 1
            , value = ""
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
      ]
    )


zipperWithAlphaDownBetaLeft =
    ( { node =
            { id = 1
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned ""
            , gui = defGUI
            }
      , ext = Open
      }
    , [ BinaryLeftCrumb Beta
            { id = 1
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
            { node =
                { id = 1
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
      , UnaryCrumb Alpha
            { id = 1
            , value = ""
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
      ]
    )


zipperOnlyAlphaOfRightBeta =
    ( { node =
            { id = 1
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned ""
            , gui = defGUI
            }
      , ext = Open
      }
    , [ UnaryCrumb Alpha
            { id = 1
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
      , BinaryRightCrumb Beta
            { id = 1
            , value = ""
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
            { node =
                { id = 1
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
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
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned ""
        , gui = { controlsShown = False }
        }
    , ext =
        Binary Beta
            { node =
                { id = 2
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
            { node =
                { id = 3
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
                , gui = { controlsShown = False }
                }
            , ext =
                Unary Alpha
                    { node =
                        { id = 4
                        , value = ""
                        , references = []
                        , formula = Formula.Parser.parseSigned ""
                        , gui = defGUI
                        }
                    , ext = Open
                    }
            }
    }


zipperOnlyAlphaOfRightBetaRenumbered =
    ( onlyAlphaOfRightBetaRenumbered, [] )


zipperZWalkPostExample =
    zipperExample |> extendUnary Alpha |> down |> extendBinary Beta |> left |> extendUnary Alpha |> up |> right |> extendBinary Beta |> left |> extendUnary Alpha |> top |> topRenumbered |> zipper


zipperZWalkPostResult =
    zipper
        { node =
            { id = 1
            , value = ""
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Binary Beta
                        { node =
                            { id = 3
                            , value = ""
                            , references = []
                            , formula = Formula.Parser.parseSigned ""
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Unary Alpha
                                { node =
                                    { id = 4
                                    , value = ""
                                    , references = []
                                    , formula = Formula.Parser.parseSigned ""
                                    , gui = defGUI
                                    }
                                , ext = Open
                                }
                        }
                        { node =
                            { id = 5
                            , value = ""
                            , references = []
                            , formula = Formula.Parser.parseSigned ""
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Binary Beta
                                { node =
                                    { id = 6
                                    , value = ""
                                    , references = []
                                    , formula = Formula.Parser.parseSigned ""
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Unary Alpha
                                        { node =
                                            { id = 7
                                            , value = ""
                                            , references = []
                                            , formula = Formula.Parser.parseSigned ""
                                            , gui = defGUI
                                            }
                                        , ext = Open
                                        }
                                }
                                { node =
                                    { id = 8
                                    , value = ""
                                    , references = []
                                    , formula = Formula.Parser.parseSigned ""
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned ""
            , gui = { controlsShown = False }
            }
        , ext =
            UnaryWithSubst Gamma
                { node =
                    { id = 1
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
                    , gui = defGUI
                    }
                , ext = Open
                }
                {str = "x->y", parsedSubst = Ok (Dict.fromList [("x", Var "y")])}
        }


getValueFromResult : Result x a -> Maybe a
getValueFromResult r =
    case r of
        Ok a ->
            Just a

        Err err ->
            Nothing


funInsteadOfVarZipper =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(f(x))"
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T \\forall x P(f(x))"
            , gui = { controlsShown = False }
            }
        , ext =
            UnaryWithSubst Gamma
                { node =
                    { id = 2
                    , value = "T P(f(Diana))"
                    , references = [{ str = "1", up = Just 1 }]
                    , formula = Formula.Parser.parseSigned "T P(f(Diana))"
                    , gui = defGUI
                    }
                , ext = Open
                }
                {str = "x -> f(Diana)", parsedSubst = Ok (Dict.fromList [("x", Fun "f" [Var "Diana"])])}
        }


newVariableSimilarToExistingFreeAboveZipper =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T \\forall x P(x, k)"
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , references = [{ str = "2", up = Just 0 }]
                    , formula = Formula.Parser.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    UnaryWithSubst Gamma
                        { node =
                            { id = 3
                            , value = "T \\exists p Z(p, f(k))"
                            , references = [{ str = "2", up = Just 1 }]
                            , formula = Formula.Parser.parseSigned "T \\exists p Z(p, f(k))"
                            , gui = defGUI
                            }
                        , ext = Open
                        }
                        {str = "z->k", parsedSubst = Ok (Dict.fromList [("z", Var "k")])}
                }
        }


newVariableSimilarToExistingBoundAboveZipper =
    zipper
        { node =
            { id = 1
            , value = "T \\forall x P(x, k)"
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T \\forall x P(x, k)"
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = "T \\forall z \\exists p Z(p, f(z))"
                    , references = [{ str = "2", up = Just 0 }]
                    , formula = Formula.Parser.parseSigned "T \\forall z \\exists p Z(p, f(z))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    UnaryWithSubst Gamma
                        { node =
                            { id = 3
                            , value = "T P(z, k)"
                            , references = [{ str = "1", up = Just 2 }]
                            , formula = Formula.Parser.parseSigned "T P(z, k)"
                            , gui = defGUI
                            }
                        , ext = Open
                        }
                        {str = "x->z", parsedSubst = Ok (Dict.fromList [("x", Var "z")])}
                }
        }


validateRenumberingAdding =
    zipper
        { node =
            { id = 1
            , value = "T (a \\/ b)"
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T (a \\/ b)"
            , gui = { controlsShown = True }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = "T (a /\\ (b \\/ c))"
                    , references = [{ str = "2", up = Just 0 }]
                    , formula = Formula.Parser.parseSigned "T (a /\\ b)"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Unary Alpha
                        { node =
                            { id = 3
                            , value = "T (b \\/ c)"
                            , references = [{ str = "2", up = Just 1 }]
                            , formula = Formula.Parser.parseSigned "T (b \\/ c)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Binary Beta
                                { node =
                                    { id = 4
                                    , value = "T a"
                                    , references = [{ str = "1", up = Just 3 }]
                                    , formula = Formula.Parser.parseSigned "T a"
                                    , gui = defGUI
                                    }
                                , ext = Open
                                }
                                { node =
                                    { id = 4
                                    , value = "T b"
                                    , references = [{ str = "1", up = Just 3 }]
                                    , formula = Formula.Parser.parseSigned "T b"
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T (a \\/ b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = ""
                    , references = []
                    , formula = Formula.Parser.parseSigned ""
                    , gui = { controlsShown = True }
                    }
                , ext =
                    Unary Alpha
                        { node =
                            { id = 3
                            , value = "T (a /\\ (b \\/ c))"
                            , references = [{ str = "3", up = Just 0 }]
                            , formula = Formula.Parser.parseSigned "T (a /\\ b)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Unary Alpha
                                { node =
                                    { id = 4
                                    , value = "T (b \\/ c)"
                                    , references = [{ str = "3", up = Just 1 }]
                                    , formula = Formula.Parser.parseSigned "T (b \\/ c)"
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Binary Beta
                                        { node =
                                            { id = 5
                                            , value = "T a"
                                            , references = [{ str = "1", up = Just 4 }]
                                            , formula = Formula.Parser.parseSigned "T a"
                                            , gui = defGUI
                                            }
                                        , ext = Open
                                        }
                                        { node =
                                            { id = 6
                                            , value = "T b"
                                            , references = [{ str = "1", up = Just 4 }]
                                            , formula = Formula.Parser.parseSigned "T b"
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T (a\\/b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = "T (b/\\(c\\/a))"
                    , references = [{ str = "2", up = Just 0 }]
                    , formula = Formula.Parser.parseSigned "T (b/\\(c\\/a))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Unary Alpha
                        { node =
                            { id = 3
                            , value = "Tb"
                            , references = [{ str = "2", up = Just 1 }]
                            , formula = Formula.Parser.parseSigned "T b"
                            , gui = { controlsShown = True }
                            }
                        , ext =
                            Unary Alpha
                                { node =
                                    { id = 4
                                    , value = "T (c\\/a)"
                                    , references = [{ str = "2", up = Just 2 }]
                                    , formula = Formula.Parser.parseSigned "T (c\\/a)"
                                    , gui = { controlsShown = False }
                                    }
                                , ext =
                                    Binary Beta
                                        { node =
                                            { id = 5
                                            , value = "Tc"
                                            , references = [{ str = "4", up = Just 1 }]
                                            , formula = Formula.Parser.parseSigned "Tc"
                                            , gui = { controlsShown = True }
                                            }
                                        , ext = Open
                                        }
                                        { node =
                                            { id = 6
                                            , value = "Ta"
                                            , references = [{ str = "4", up = Just 1 }]
                                            , formula = Formula.Parser.parseSigned "Ta"
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
            , references = [{ str = "1", up = Just 0 }]
            , formula = Formula.Parser.parseSigned "T (a\\/b)"
            , gui = { controlsShown = False }
            }
        , ext =
            Unary Alpha
                { node =
                    { id = 2
                    , value = "T (b/\\(c\\/a))"
                    , references = [{ str = "2", up = Just 0 }]
                    , formula = Formula.Parser.parseSigned "T (b/\\(c\\/a))"
                    , gui = { controlsShown = False }
                    }
                , ext =
                    Unary Alpha
                        { node =
                            { id = 3
                            , value = "T (c\\/a)"
                            , references = [{ str = "2", up = Just 1 }]
                            , formula = Formula.Parser.parseSigned "T (c\\/a)"
                            , gui = { controlsShown = False }
                            }
                        , ext =
                            Binary Beta
                                { node =
                                    { id = 4
                                    , value = "Tc"
                                    , references = [{ str = "3", up = Just 1 }]
                                    , formula = Formula.Parser.parseSigned "Tc"
                                    , gui = { controlsShown = True }
                                    }
                                , ext = Open
                                }
                                { node =
                                    { id = 5
                                    , value = "Ta"
                                    , references = [{ str = "3", up = Just 1 }]
                                    , formula = Formula.Parser.parseSigned "Ta"
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
        , test "compare extended alpha" (\() -> compareZippers (extendUnary Alpha zipperExample) zipperWithAlpha)
        , test "compare extended alpha after going down once" (\() -> compareZippers (down (extendUnary Alpha zipperExample)) zipperWithAplhaDown)
        , test "compare zippers: extend alpha, go down, extend beta"
            (\() -> compareZippers (left (extendBinary Beta (down (extendUnary Alpha zipperExample)))) zipperWithAlphaDownBetaLeft)
        , test "compare zippers, extend beta, go right, extend alpha, go down"
            (\() ->
                compareZippers
                    (zipperExample |> extendBinary Beta |> right |> extendUnary Alpha |> down)
                    zipperOnlyAlphaOfRightBeta
            )
        , test
            "renumber"
            (\() ->
                compareZippers
                    (zipperExample |> extendBinary Beta |> right |> extendUnary Alpha |> down |> top |> topRenumbered |> zipper)
                    zipperOnlyAlphaOfRightBetaRenumbered
            )
        , test "renumber another" (\() -> compareZippers zipperZWalkPostExample zipperZWalkPostResult)
        , test "reference rewriting"
            (\() ->
                compareZippers testReferenceRewritingResult
                    (testReferenceRewriting |> Zipper.extendUnary Alpha |> Editor.top |> Zipper.renumber |> zipper)
            )
        , test "reference rewriting 2" (\() -> compareZippers (fixRefs fixRefsTest) fixRefsTestResult)
        , test "gamma term test"
            (\() ->
                compareZippers gammaExampleResult
                    (zipperExample |> Zipper.extendUnaryWithSubst Gamma |> Zipper.setSubstitution "x->y")
            )
        , test "function instead of variable "
            (\() ->
                Expect.err
                    (checkNewVariables
                        (funInsteadOfVarZipper
                            |> Zipper.down
                        )
                    )
            )
        , test "substitution in delta - substitute for existing bound above"
            (\() ->
                Expect.ok
                    (checkNewVariables
                        (newVariableSimilarToExistingBoundAboveZipper
                            |> Zipper.down
                            |> Zipper.down
                        )
                    )
            )
        , test "substitution in delta - substitute for existing free above"
            (\() ->
                Expect.err
                    (checkNewVariables
                        (newVariableSimilarToExistingFreeAboveZipper
                            |> Zipper.down
                            |> Zipper.down
                        )
                    )
            )
        , test "renumbering justs when adding a node"
            (\() ->
                Expect.equal
                    validateRenumberingAddingResult
                    (validateRenumberingAdding
                        |> Zipper.extendUnary Alpha
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

