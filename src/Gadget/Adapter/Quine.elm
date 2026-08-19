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
                recordFunctionCall =
                    P.string "Gadget.record"

                ctor =
                    anonymousFunction
                        { args = List.map Tuple.first namedFields
                        , body = anonymousRecord (List.map (\( n, _ ) -> ( n, n )) namedFields)
                        }

                pizza =
                    P.string "|>"

                fieldFunctionCall =
                    P.string "Gadget.field"

                fields =
                    P.join P.line
                        (List.map
                            (\( name, fld ) ->
                                P.group
                                    (P.nest 4
                                        (P.words
                                            [ pizza
                                            , fieldFunctionCall
                                            , P.group
                                                (P.lines
                                                    [ P.group
                                                        (P.lines
                                                            [ fieldName name
                                                            , fieldGetter name
                                                            ]
                                                        )
                                                    , fieldGadget fld
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                            )
                            namedFields
                        )
                        |> P.group

                fieldName name =
                    P.string ("\"" ++ name ++ "\"")

                fieldGetter name =
                    P.string ("." ++ name)

                fieldGadget fld =
                    if isPrimitive fld then
                        parens (quineHelp fld)

                    else
                        quineHelp fld

                isPrimitive fld =
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

                endRecordFunctionCall =
                    P.string "Gadget.endRecord"
            in
            P.group
                (P.nest 4
                    (P.lines
                        [ P.group
                            (P.lines
                                [ recordFunctionCall
                                , ctor
                                ]
                            )
                        , fields
                        , P.words [ pizza, endRecordFunctionCall ]
                        ]
                    )
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


anonymousRecord : List ( String, String ) -> P.Doc t
anonymousRecord fieldNamesAndValues =
    case fieldNamesAndValues of
        [] ->
            P.string "{}"

        _ ->
            let
                printField ( name, value ) =
                    P.string (name ++ " =")
                        |> P.a P.line
                        |> P.nest 4
                        |> P.a (P.string value)
                        |> P.group
            in
            P.surround
                (P.string "{ ")
                (P.line |> P.a (P.string "}"))
                (P.separators ", " (List.map printField fieldNamesAndValues))
                |> P.group
                |> P.align


anonymousFunction : { args : List String, body : P.Doc t } -> P.Doc t
anonymousFunction { args, body } =
    P.words
        [ P.string ("\\" ++ String.join " " args)
        , P.string "->"
        ]
        |> P.a P.line
        |> P.a body
        |> P.nest 4
        |> parens


parens : P.Doc t -> P.Doc t
parens inner =
    P.group
        (P.flatAlt
            (P.char '(' |> P.a inner |> P.a (P.char ')'))
            (P.char '(' |> P.a inner |> P.a P.line |> P.a (P.char ')'))
        )
