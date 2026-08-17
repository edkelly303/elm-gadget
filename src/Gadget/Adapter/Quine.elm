module Gadget.Adapter.Quine exposing (quine)

import Gadget.IR as IR exposing (Type(..), VariantType(..))
import Pretty as P


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    IR.irType gadget
        |> quineHelp
        |> P.pretty columns


quineHelp : Type -> P.Doc t
quineHelp irType =
    case irType of
        UnitType _ ->
            P.string "Gadget.unit"

        BoolType _ ->
            P.string "Gadget.bool"

        IntType _ ->
            P.string "Gadget.int"

        FloatType _ ->
            P.string "Gadget.float"

        CharType _ ->
            P.string "Gadget.char"

        StringType _ ->
            P.string "Gadget.string"

        RecordType _ namedFields ->
            let
                ctor =
                    P.string
                        ("(\\"
                            ++ (List.map (\( name, fld ) -> name) namedFields
                                    |> String.join " "
                               )
                            ++ " -> { "
                            ++ (List.map (\( name, fld ) -> name ++ " = " ++ name) namedFields
                                    |> String.join ", "
                               )
                            ++ " })"
                        )

                pizza =
                    P.string "|> "

                field =
                    P.string "Gadget.field"

                fields =
                    P.join P.line
                        (List.map
                            (\( name, fld ) ->
                                P.tightline
                                    |> P.a pizza
                                    |> P.a field
                                    |> P.a P.line
                                    |> P.a (P.string ("\"" ++ name ++ "\""))
                                    |> P.group
                                    |> P.a P.line
                                    |> P.a (P.string ("." ++ name))
                                    |> P.group
                                    |> P.a P.line
                                    |> P.a
                                        (if needsParens fld then
                                            parens (quineHelp fld)

                                         else
                                            quineHelp fld
                                        )
                                    |> P.nest 4
                                    |> P.group
                            )
                            namedFields
                        )
                        |> P.group

                parens inner =
                    P.group
                        (P.flatAlt
                            (P.char '(' |> P.a inner |> P.a (P.char ')'))
                            (P.char '(' |> P.a inner |> P.a P.line |> P.a (P.char ')'))
                        )

                needsParens fld =
                    case fld of
                        UnitType _ ->
                            False

                        BoolType _ ->
                            False

                        CharType _ ->
                            False

                        StringType _ ->
                            False

                        IntType _ ->
                            False

                        FloatType _ ->
                            False

                        _ ->
                            True

                endRecord =
                    P.string "Gadget.endRecord"
            in
            P.string "Gadget.record"
                |> P.a
                    (P.line
                        |> P.a ctor
                        |> P.nest 4
                    )
                |> P.group
                |> P.a
                    (P.line
                        |> P.a fields
                        |> P.nest 4
                    )
                |> P.a
                    (P.line
                        |> P.a pizza
                        |> P.a endRecord
                        |> P.nest 4
                    )

        CustomType _ _ _ ->
            Debug.todo "branch 'CustomType _ _ _' not implemented"

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"
