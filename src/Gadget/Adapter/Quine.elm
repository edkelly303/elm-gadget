module Gadget.Adapter.Quine exposing (quine)

import Gadget.IR as IR exposing (Type(..), VariantType(..))


quine : IR.Gadget a -> String
quine gadget =
    IR.irType gadget
        |> quineHelp 4


quineHelp : Int -> Type -> String
quineHelp indent irType =
    let
        spaces =
            String.repeat indent " "
    in
    case irType of
        UnitType _ ->
            "Gadget.unit"

        BoolType _ ->
            "Gadget.bool"

        IntType _ ->
            "Gadget.int"

        FloatType _ ->
            "Gadget.float"

        CharType _ ->
            "Gadget.char"

        StringType _ ->
            "Gadget.string"

        RecordType _ namedFields ->
            "Gadget.record (\\"
                ++ (List.map (\( name, field ) -> name) namedFields
                        |> String.join " "
                   )
                ++ " -> { "
                ++ (List.map (\( name, field ) -> name ++ " = " ++ name) namedFields
                        |> String.join ", "
                   )
                ++ " })\n"
                ++ spaces
                ++ "|> "
                ++ (List.map
                        (\( name, field ) ->
                            "Gadget.field \""
                                ++ name
                                ++ "\" ."
                                ++ name
                                ++ "\n"
                                ++ spaces
                                ++ "    "
                                ++ quineHelp (indent + 4) field
                        )
                        namedFields
                        |> String.join
                            ("\n"
                                ++ spaces
                                ++ "|> "
                            )
                   )
                ++ "\n"
                ++ spaces
                ++ "|> Gadget.endRecord"

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
