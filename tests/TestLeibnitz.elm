module TestLeibnitz exposing (..)

import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Test exposing (..)
import Validation.Common exposing (semanticsProblem)
import Validation.Rules.Leibnitz exposing (checkSubsts, commonSignedTemplate, validate)
import Zipper exposing (..)


suiteCommonTemplate : Test
suiteCommonTemplate =
    describe "The commonSignedTemplate function"
        [ test "replace right in eq atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (EqAtom (Var "b") (Var "a")))
                        (T (EqAtom (Var "b") (Var "c")))
                    )
                    (Ok (T (EqAtom (Var "b") (Var "[]"))))
            )
        , test "replace left in eq atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (EqAtom (Var "a") (Var "b")))
                        (T (EqAtom (Var "c") (Var "b")))
                    )
                    (Ok (T (EqAtom (Var "[]") (Var "b"))))
            )
        , test "replace both in eq atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (EqAtom (Var "a") (Var "a")))
                        (T (EqAtom (Var "c") (Var "c")))
                    )
                    (Ok (T (EqAtom (Var "[]") (Var "[]"))))
            )
        , test "replace none in eq atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (EqAtom (Var "a") (Var "b")))
                        (T (EqAtom (Var "a") (Var "b")))
                    )
                    (Ok (T (EqAtom (Var "a") (Var "b"))))
            )
        , test "replace in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Var "b", Var "a", Var "x" ]))
                        (T (PredAtom "p" [ Var "b", Var "c", Var "x" ]))
                    )
                    (Ok (T (PredAtom "p" [ Var "b", Var "[]", Var "x" ])))
            )
        , test "replace all in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Var "a", Var "a", Var "a" ]))
                        (T (PredAtom "p" [ Var "c", Var "c", Var "c" ]))
                    )
                    (Ok (T (PredAtom "p" [ Var "[]", Var "[]", Var "[]" ])))
            )
        , test "replace none in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Var "a", Var "c", Var "b" ]))
                        (T (PredAtom "p" [ Var "a", Var "c", Var "b" ]))
                    )
                    (Ok (T (PredAtom "p" [ Var "a", Var "c", Var "b" ])))
            )
        , test "replace in function in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "a" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Var "c" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "[]" ] ])))
            )
        , test "replace none in function in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "a", Var "b", Var "c" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Var "a", Var "b", Var "c" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "a", Var "b", Var "c" ] ])))
            )
        , test "replace function with variable in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Fun "f" [ Var "b" ])
                        (Var "b")
                        (T (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "b" ], Var "b" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Var "b", Var "b" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "[]", Var "b" ] ])))
            )
        , test "replace variable with function in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "b")
                        (Fun "f" [ Var "b" ])
                        (T (PredAtom "p" [ Fun "f" [ Var "b", Var "b" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "b" ], Var "b" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "[]", Var "b" ] ])))
            )
        , test "replace function with different function in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Fun "f" [ Var "b" ])
                        (Fun "g" [ Var "b" ])
                        (T (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "b" ], Var "b" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Fun "g" [ Var "b" ], Var "b" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "[]", Var "b" ] ])))
            )
        , test "replace function with the same function with a different argument in predicate atom"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Fun "f" [ Var "a" ])
                        (Fun "f" [ Var "c" ])
                        (T (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "a" ], Var "b" ], Fun "f" [ Var "a" ], Fun "f" [ Var "a" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "c" ], Var "b" ], Fun "f" [ Var "c" ], Fun "f" [ Var "a" ] ]))
                    )
                    (Ok (T (PredAtom "p" [ Fun "f" [ Var "[]", Var "b" ], Var "[]", Fun "f" [ Var "a" ] ])))
            )
        , test "replace in predicate atom with non-matching arity"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Var "a" ]))
                        (T (PredAtom "p" [ Var "c", Var "x" ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "have different structure "
                            ++ "(p(a) vs. p(c, x))"
                    )
            )
        , test "replace in non-matching predicate atoms (different preds)"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Var "a" ]))
                        (T (PredAtom "q" [ Var "c" ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "have different structure "
                            ++ "(p(a) vs. q(c))"
                    )
            )
        , test "replace in predicate atom with non-matching argument (arity)"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "a" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Var "c", Var "x" ] ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "contain non-matching subterms "
                            ++ "at corresponding locations "
                            ++ "(f(a) vs. f(c, x))"
                    )
            )
        , test "replace in predicate atom with non-matching argument (vars)"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "c" ] ]))
                        (T (PredAtom "p" [ Fun "f" [ Var "a" ] ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "contain non-matching subterms "
                            ++ "at corresponding locations "
                            ++ "(c vs. a)"
                    )
            )
        , test "replace in predicate atom with non-matching argument (var vs. fun)"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "a" ] ]))
                        (T (PredAtom "p" [ Var "c" ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "contain non-matching subterms "
                            ++ "at corresponding locations "
                            ++ "(f(a) vs. c)"
                    )
            )
        , test "replace in predicate atom with non-matching argument (different funcs)"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T (PredAtom "p" [ Fun "f" [ Var "a" ] ]))
                        (T (PredAtom "p" [ Fun "g" [ Var "c" ] ]))
                    )
                    (Err <|
                        "The 2nd referenced formula and current formula "
                            ++ "contain non-matching subterms "
                            ++ "at corresponding locations "
                            ++ "(f(a) vs. g(c))"
                    )
            )
        , test "no change"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "a")
                        (Var "c")
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "a" ]) (Var "c"))
                            )
                        )
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "a" ]) (Var "c"))
                            )
                        )
                    )
                    (Ok
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "a" ]) (Var "c"))
                            )
                        )
                    )
            )
        , test "replace in conjunction"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "c")
                        (Var "x")
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b", Var "c" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "c" ]) (Var "c"))
                            )
                        )
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b", Var "x" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "x" ]) (Var "c"))
                            )
                        )
                    )
                    (Ok
                        (T
                            (Conj
                                (EqAtom (Fun "f" [ Var "b", Var "[]" ]) (Var "x"))
                                (EqAtom (Fun "f" [ Var "[]" ]) (Var "c"))
                            )
                        )
                    )
            )
        , test "replace in for all"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "x")
                        (Var "c")
                        (T
                            (ForAll "x"
                                (Impl
                                    (EqAtom (Fun "f" [ Var "c", Var "x" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "x", Var "b", Var "x" ]
                                        , Fun "h" [ Var "x" ]
                                        ]
                                    )
                                )
                            )
                        )
                        (T
                            (ForAll "x"
                                (Impl
                                    (EqAtom (Fun "f" [ Var "c", Var "c" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "c", Var "b", Var "c" ]
                                        , Fun "h" [ Var "x" ]
                                        ]
                                    )
                                )
                            )
                        )
                    )
                    (Ok
                        (T
                            (ForAll "x"
                                (Impl
                                    (EqAtom (Fun "f" [ Var "c", Var "[]" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "[]", Var "b", Var "[]" ]
                                        , Fun "h" [ Var "x" ]
                                        ]
                                    )
                                )
                            )
                        )
                    )
            )
        , test "replace in exists"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (Var "c")
                        (Var "x")
                        (T
                            (Exists "x"
                                (Disj
                                    (EqAtom (Fun "f" [ Var "c", Var "z" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "c", Var "z" ]
                                        , Fun "g" [ Var "c" ]
                                        ]
                                    )
                                )
                            )
                        )
                        (T
                            (Exists "x"
                                (Disj
                                    (EqAtom (Fun "f" [ Var "x", Var "z" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "c", Var "z" ]
                                        , Fun "g" [ Var "x" ]
                                        ]
                                    )
                                )
                            )
                        )
                    )
                    (Ok
                        (T
                            (Exists "x"
                                (Disj
                                    (EqAtom (Fun "f" [ Var "[]", Var "z" ]) (Var "y"))
                                    (PredAtom "p"
                                        [ Fun "f" [ Var "c", Var "z" ]
                                        , Fun "g" [ Var "[]" ]
                                        ]
                                    )
                                )
                            )
                        )
                    )
            )
        ]


sfFrom : String -> Signed Formula
sfFrom sfString =
    Result.withDefault (F <| PredAtom "ERR" []) <|
        Formula.Parser.parseSigned sfString


zipperWith : String -> Zipper
zipperWith sfString =
    zipper
        { node =
            { id = 1
            , value = ""
            , references = [ { str = "1", up = Just 0 } ]
            , formula = Formula.Parser.parseSigned sfString
            , gui = defGUI
            }
        , ext = Open
        }


zipper1 =
    zipperWith "T ∀x (p(c) & (x = c))"


zipper2 =
    zipperWith "T ∀x (p(f(x)) | ∀c(c = c))"


zipper3 =
    zipperWith "F ∃x∃y (∀z p(z) | ∀z(r(z) & f(z,x) != y))"


zipper4 =
    zipperWith "T ∃x (p(b) & r(x))"


zipper5 =
    zipperWith "T (p(f(f(a))) | ∀c r(c))"


zipper6 =
    zipperWith "T ∃x∃y ((r(z) & c = f(a,x)) | p(z))"


suiteCheckSubsts : Test
suiteCheckSubsts =
    describe "The checkSubsts function"
        [ test "not substitutable 1"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [ Var "a" ]) (Var "x")))
                        (T
                            (ForAll "x"
                                (Conj
                                    (PredAtom "p" [ Var "c" ])
                                    (EqAtom (Fun "f" [ Var "a" ]) (Var "c"))
                                )
                            )
                        )
                        zipper1
                    )
                    (Err (semanticsProblem zipper1 "Cannot substitute f(a) with x in the 2nd referenced formula; variable x is bound"))
            )
        , test "not substitutable 2"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "c")))
                        (T (ForAll "x" (Disj (PredAtom "p" [ Fun "f" [ Var "x" ] ]) (ForAll "c" (EqAtom (Var "y") (Var "c"))))))
                        zipper2
                    )
                    (Err (semanticsProblem zipper2 "Cannot substitute y with c in the 2nd referenced formula; variable c is bound"))
            )
        , test "not substitutable 3"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [ Var "z" ])) (ForAll "z" (Conj (PredAtom "r" [ Var "z" ]) (Neg (EqAtom (Fun "f" [ Var "z", Var "y" ]) (Var "y")))))))))
                        zipper3
                    )
                    (Err (semanticsProblem zipper3 "Cannot substitute y with x in the 2nd referenced formula; variable y is bound"))
            )
        , test "not substitutable 4"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "a") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [ Var "z" ])) (ForAll "z" (Conj (PredAtom "r" [ Var "z" ]) (Neg (EqAtom (Fun "f" [ Var "z", Var "a" ]) (Var "y")))))))))
                        zipper3
                    )
                    (Err (semanticsProblem zipper3 "Cannot substitute a with x in the 2nd referenced formula; variable x is bound"))
            )
        , test "not substitutable 5"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [ Var "z", Var "a" ]) (Fun "f" [ Var "z", Var "x" ])))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [ Var "z" ])) (ForAll "z" (Conj (PredAtom "r" [ Var "z" ]) (Neg (EqAtom (Fun "f" [ Var "z", Var "a" ]) (Var "y")))))))))
                        zipper3
                    )
                    (Err (semanticsProblem zipper3 "Cannot substitute f(z, a) with f(z, x) in the 2nd referenced formula; variable z is bound"))
            )
        , test "not substitutable 6"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "a") (Fun "f" [ Var "z", Var "x" ])))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [ Var "z" ])) (ForAll "z" (Conj (PredAtom "r" [ Var "z" ]) (Neg (EqAtom (Var "a") (Var "y")))))))))
                        zipper3
                    )
                    (Err (semanticsProblem zipper3 "Cannot substitute a with f(z, x) in the 2nd referenced formula; variables x, z are bound"))
            )
        , test "different sign"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [ Var "a" ]) (Var "x")))
                        (F (ForAll "x" (Conj (PredAtom "p" [ Var "c" ]) (EqAtom (Fun "f" [ Var "a" ]) (Var "c")))))
                        zipper1
                    )
                    (Err (semanticsProblem zipper1 "The 2nd referenced formula and current formula have different sign"))
            )
        , test "different structure 1"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [ Var "a" ]) (Var "x")))
                        (T (ForAll "x" (Disj (PredAtom "p" [ Var "c" ]) (EqAtom (Fun "f" [ Var "a" ]) (Var "c")))))
                        zipper1
                    )
                    (Err (semanticsProblem zipper1 "The 2nd referenced formula and current formula have different structure (( p(c) ∨ f(a) ≐ c ) vs. ( p(c) ∧ x ≐ c ))"))
            )
        , test "different structure 2"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "c")))
                        (T (ForAll "x" (Disj (PredAtom "p" [ Fun "f" [ Var "x" ] ]) (ForAll "c" (PredAtom "a" [ Var "y" ])))))
                        zipper2
                    )
                    (Err (semanticsProblem zipper2 "The 2nd referenced formula and current formula have different structure (a(y) vs. c ≐ c)"))
            )
        , test "different structure 3"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [ Var "z" ])) (Exists "z" (Conj (PredAtom "r" [ Var "z" ]) (Neg (EqAtom (Fun "f" [ Var "z", Var "y" ]) (Var "y")))))))))
                        zipper3
                    )
                    (Err (semanticsProblem zipper3 "The 2nd referenced formula and current formula have different structure (∃z ( r(z) ∧ ¬ f(z, y) ≐ y ) vs. ∀z ( r(z) ∧ ¬ f(z, x) ≐ y ))"))
            )
        , test "different structure 4"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "c") (Var "a")))
                        (T (Exists "x" (Conj (PredAtom "p" [ Var "c" ]) (PredAtom "r" [ Var "x" ]))))
                        zipper4
                    )
                    (Err (semanticsProblem zipper4 "The 2nd referenced formula and current formula contain non-matching subterms at corresponding locations (c vs. b)"))
            )
        , test "different structure 5"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "x") (Var "z")))
                        (T (Disj (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "x" ] ] ]) (ForAll "c" (PredAtom "r" [ Var "c" ]))))
                        zipper5
                    )
                    (Err (semanticsProblem zipper5 "The 2nd referenced formula and current formula contain non-matching subterms at corresponding locations (x vs. a)"))
            )
        , test "different structure 6"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "z") (Var "b")))
                        (T (Exists "x" (Exists "y" (Disj (Conj (PredAtom "r" [ Var "z" ]) (EqAtom (Var "c") (Fun "f" [ Var "z", Var "x" ]))) (PredAtom "p" [ Var "z" ])))))
                        zipper6
                    )
                    (Err (semanticsProblem zipper6 "The 2nd referenced formula and current formula contain non-matching subterms at corresponding locations (z vs. a)"))
            )
        , test "ok 1"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "c") (Var "b")))
                        (T (Exists "x" (Conj (PredAtom "p" [ Var "c" ]) (PredAtom "r" [ Var "x" ]))))
                        zipper4
                    )
                    (Ok zipper4)
            )
        , test "ok 2"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "x") (Var "a")))
                        (T (Disj (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "x" ] ] ]) (ForAll "c" (PredAtom "r" [ Var "c" ]))))
                        zipper5
                    )
                    (Ok zipper5)
            )
        , test "ok 3"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "z") (Var "a")))
                        (T (Exists "x" (Exists "y" (Disj (Conj (PredAtom "r" [ Var "z" ]) (EqAtom (Var "c") (Fun "f" [ Var "z", Var "x" ]))) (PredAtom "p" [ Var "z" ])))))
                        zipper6
                    )
                    (Ok zipper6)
            )
        , test "ok 4"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [ Var "x" ]) (Fun "f" [ Var "a" ])))
                        (T (Disj (PredAtom "p" [ Fun "f" [ Fun "f" [ Var "x" ] ] ]) (ForAll "c" (PredAtom "r" [ Var "c" ]))))
                        zipper5
                    )
                    (Ok zipper5)
            )
        , test "ok 5"
            (\() ->
                Expect.equal
                    (checkSubsts
                        (sfFrom "T g(a) ≐ g(b)")
                        (sfFrom "T P(g(a))")
                        (zipperWith "T P(g(b))")
                    )
                    (Ok (zipperWith "T P(g(b))"))
            )
        ]


zipperWithEqsAndLeibnitz =
    zipperFromTableauBreadCrumbs
        { node =
            { id = 3
            , value = ""
            , references = [ { str = "1", up = Just 2 }, { str = "2", up = Just 1 } ]
            , formula = Formula.Parser.parseSigned "T b = a"
            , gui = { controlsShown = False }
            }
        , ext = Open
        }
        [ UnaryCrumb Alpha
            { id = 2
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned "T a = a"
            , gui = { controlsShown = False }
            }
        , UnaryCrumb Alpha
            { id = 1
            , value = ""
            , references = []
            , formula = Formula.Parser.parseSigned "T a = b"
            , gui = defGUI
            }
        ]


suiteValidate : Test
suiteValidate =
    describe "validation"
        [ test "validate"
            (\() ->
                Expect.equal
                    (validate
                        zipperWithEqsAndLeibnitz
                    )
                    (Ok zipperWithEqsAndLeibnitz)
            )
        ]
