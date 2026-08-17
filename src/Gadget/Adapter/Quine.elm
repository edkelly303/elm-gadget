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
                    P.string "|>"

                field =
                    P.string "Gadget.field"

                fields =
                    P.separators " "
                        (List.map
                            (\( name, fld ) ->
                                field
                                    |> P.a (P.string ("\"" ++ name ++ "\""))
                                    |> P.a (P.string ("." ++ name))
                                    |> P.a (quineHelp fld)
                            )
                            namedFields
                        )

                endRecord =
                    P.string "Gadget.endRecord"
            in
            P.string "Gadget.record"
                |> P.a ctor
                |> P.a pizza
                |> P.a fields
                |> P.a pizza
                |> P.a endRecord

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
