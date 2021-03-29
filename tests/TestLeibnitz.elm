module TestLeibnitz exposing (..)

import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Test exposing (..)
import Validation.Common exposing (semanticsProblem)
import Validation.Rules.Leibnitz exposing (commonSignedTemplate, checkSubsts, validate)
import Zipper exposing (..)


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


suiteCommonTemplate : Test
suiteCommonTemplate =
    describe "The commonSignedTemplate function"
        [ test "replace in eq atom" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (EqAtom (Var "b") (Var "a")))
                        (T (EqAtom (Var "b") (Var "c")))
                    ) 
                    (Ok (T (EqAtom (Var "b") (Var "[]")))) 
            )
        , test "replace in predicate atom" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (PredAtom "p" [Fun "f"[Var "b"]] ))
                        (T (PredAtom "p" [Fun "f"[Var "b", Var "x"]] ))
                    ) 
                    (Ok (T (PredAtom "p" [Var "[]"] )))
            )
        , test "replace function with variable in predicate atom 2"
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (PredAtom "p" [Fun "f"[Fun "f"[Var "b"], Var "b"]] ))
                        (T (PredAtom "p" [Fun "f"[Var "b", Var "b"]] ))
                    ) 
                    (Ok (T (PredAtom "p" [Fun "f"[Var "[]", Var "b"]] )))
            )
        , test "no change" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )
                        (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )
                    ) 
                    (Ok (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )) 
            )
        , test "replace in conjunction" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (Conj (EqAtom (Fun "f"[Var "b", Var "c"]) (Var "x")) (EqAtom (Fun "f"[Var "c"]) (Var "b")) ) )
                        (T (Conj (EqAtom (Fun "f"[Var "b", Var "x"]) (Var "x")) (EqAtom (Fun "f"[Var "x"]) (Var "x")) ) )
                    ) 
                    (Ok (T (Conj (EqAtom (Fun "f"[Var "b", Var "[]"]) (Var "x")) (EqAtom (Fun "f"[Var "[]"]) (Var "[]")) ) )) 
            )
        , test "replace in for all" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "a", Var "b", Var "c"], Fun "g"[Var "a"] ]))))
                       (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "a"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "o", Var "o", Var "o"], Fun "h"[Var "a"] ]))))
                    ) 
                    (Ok (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "[]"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "[]", Var "[]", Var "[]"], Var "[]" ]))))) 
            )
        , test "replace in exists" 
            (\() ->
                Expect.equal
                    (commonSignedTemplate
                        (T (Exists "x" (Conj (EqAtom (Fun "f"[Var "c", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Fun "g"[Var "a"] ]))))
                       (T (Exists "x" (Conj (EqAtom (Fun "f"[Var "x", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Fun "h"[Var "a"] ]))))
                    ) 
                    (Ok (T (Exists "x" (Conj (EqAtom (Fun "f"[Var "[]", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Var "[]" ]))))) 
            )
        ]


tableau1 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "T ∀x (p(c) & (x = c))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper1 =
    zipper tableau1


tableau2 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "T ∀x (p(f(x)) | ∀c(c = c))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper2 =
    zipper tableau2


tableau3 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "F ∃x∃y (∀z p(z) | ∀z(r(z) & f(z,x) != y))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper3 =
    zipper tableau3


tableau4 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "T ∃x (p(b) & r(x))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper4 =
    zipper tableau4


tableau5 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "T (p(f(f(a))) | ∀c r(c))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper5 =
    zipper tableau5


tableau6 =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned "T ∃x∃y ((r(z) & c = f(a,x)) | p(z))"
        , gui = defGUI
        }
    , ext = Open
    }


zipper6 =
    zipper tableau6


suiteCheckSubsts : Test
suiteCheckSubsts =
    describe "The checkSubsts function"
        [ test "not substitutable 1" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [Var "a"]) (Var "x")))
                        (T (ForAll "x" (Conj (PredAtom "p" [Var "c"]) (EqAtom (Fun "f" [Var "a"]) (Var "c")))))
                        zipper1
                    ) 
                    (Err (semanticsProblem zipper1 "Cannot substitute x for [] in 2nd referenced formula; variable x is bound"))
            )
        , test "not substitutable 2" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "c")))
                        (T (ForAll "x" (Disj (PredAtom "p" [Fun "f" [Var "x"]]) (ForAll "c" (EqAtom (Var "y") (Var "c"))))))
                        zipper2
                    ) 
                    (Err (semanticsProblem zipper2 "Cannot substitute c for [] in 2nd referenced formula; variable c is bound"))
            )
        , test "not substitutable 3" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [Var "z"])) (ForAll "z" (Conj (PredAtom "r" [Var "z"]) (Neg(EqAtom (Fun "f" [Var "z", Var "y"]) (Var "y")))))))))
                        zipper3
                    ) 
                    (Err (semanticsProblem zipper3 "Cannot substitute y for [] in 2nd referenced formula; variable y is bound"))
            )
        , test "not substitutable 4" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "a") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [Var "z"])) (ForAll "z" (Conj (PredAtom "r" [Var "z"]) (Neg(EqAtom (Fun "f" [Var "z", Var "a"]) (Var "y")))))))))
                        zipper3
                    ) 
                    (Err (semanticsProblem zipper3 "Cannot substitute x for [] in 2nd referenced formula; variable x is bound"))
            )
        , test "different sign" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [Var "a"]) (Var "x")))
                        (F (ForAll "x" (Conj (PredAtom "p" [Var "c"]) (EqAtom (Fun "f" [Var "a"]) (Var "c")))))
                        zipper1
                    ) 
                    (Err (semanticsProblem zipper1 "The 2nd referenced formula and current formula have different sign"))
            )
        , test "different structure 1" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Fun "f" [Var "a"]) (Var "x")))
                        (T (ForAll "x" (Disj(PredAtom "p" [Var "c"]) (EqAtom (Fun "f" [Var "a"]) (Var "c")))))
                        zipper1
                    ) 
                    (Err (semanticsProblem zipper1 "The 2nd referenced formula and current formula have different structure"))
            )
        , test "different structure 2" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "c")))
                        (T (ForAll "x" (Disj (PredAtom "p" [Fun "f" [Var "x"]]) (ForAll "c" (PredAtom "a" [Var "y"])))))
                        zipper2
                    ) 
                    (Err (semanticsProblem zipper2 "The 2nd referenced formula and current formula have different structure"))
            )
        , test "different structure 3" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "y") (Var "x")))
                        (F (Exists "x" (Exists "y" (Disj (ForAll "z" (PredAtom "p" [Var "z"])) (Exists "z" (Conj (PredAtom "r" [Var "z"]) (Neg(EqAtom (Fun "f" [Var "z", Var "y"]) (Var "y")))))))))
                        zipper3
                    ) 
                    (Err (semanticsProblem zipper3 "The 2nd referenced formula and current formula have different structure"))
            )
        , test "invalid substitution 1" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "c") (Var "a")))
                        (T (Exists "x" (Conj (PredAtom "p" [Var "c"]) (PredAtom "r" [Var "x"]))))
                        zipper4
                    ) 
                    (Err (semanticsProblem zipper4 "Substitution invalid"))
            )
        , test "invalid substitution 2" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "x") (Var "z")))
                        (T (Disj (PredAtom "p" [Fun "f" [Fun "f" [Var "x"]]]) (ForAll "c" (PredAtom "r" [Var "c"]))))
                        zipper5
                    ) 
                    (Err (semanticsProblem zipper5 "Substitution invalid"))
            )
        , test "invalid substitution 3" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "z") (Var "b")))
                        (T (Exists "x" (Exists "y" (Disj (Conj (PredAtom "r" [Var "z"]) (EqAtom (Var "c") (Fun "f" [Var "z", Var "x"]))) (PredAtom "p" [Var "z"])))))
                        zipper6
                    ) 
                    (Err (semanticsProblem zipper6 "Substitution invalid"))
            )
        , test "ok 1" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "c") (Var "b")))
                        (T (Exists "x" (Conj (PredAtom "p" [Var "c"]) (PredAtom "r" [Var "x"]))))
                        zipper4
                    ) 
                    (Ok zipper4)
            )
        , test "ok 2" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "x") (Var "a")))
                        (T (Disj (PredAtom "p" [Fun "f" [Fun "f" [Var "x"]]]) (ForAll "c" (PredAtom "r" [Var "c"]))))
                        zipper5
                    ) 
                    (Ok zipper5)
            )
        , test "ok 3" 
            (\() ->
                Expect.equal
                    (checkSubsts
                        (T (EqAtom (Var "z") (Var "a")))
                        (T (Exists "x" (Exists "y" (Disj (Conj (PredAtom "r" [Var "z"]) (EqAtom (Var "c") (Fun "f" [Var "z", Var "x"]))) (PredAtom "p" [Var "z"])))))
                        zipper6
                    ) 
                    (Ok zipper6)
            )
        ]


zipperWithEqsAndLeibnitz =
    ( { node =
            { id = 3
            , value = ""
            , references = [{ str = "1", up = Just 2 }, { str = "2", up = Just 1 }]
            , formula = Formula.Parser.parseSigned "T b = a"
            , gui = { controlsShown = False }
            }
      , ext = Open
      } 
    , [ UnaryCrumb Alpha
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
    )


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