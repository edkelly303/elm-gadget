module Gadget.Adapter.Quine exposing (quine)

import Gadget.Adapter.Glam as G
import Gadget.IR as IR exposing (Type(..), VariantType(..))
import Pretty as P


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    IR.irType gadget
        |> quineHelp2
        |> G.toString columns



-- |> P.pretty columns


quineHelp2 irType =
    case irType of
        UnitType _ ->
            G.fromString "Gadget.unit"

        BoolType _ ->
            G.fromString "Gadget.bool"

        IntType _ ->
            G.fromString "Gadget.int"

        FloatType _ ->
            G.fromString "Gadget.float"

        CharType _ ->
            G.fromString "Gadget.char"

        StringType _ ->
            G.fromString "Gadget.string"

        CustomType _ ( fstName, fstType ) rstNamesAndTypes ->
            let
                names =
                    fstName :: List.map Tuple.first rstNamesAndTypes

                types =
                    fstType :: List.map Tuple.second rstNamesAndTypes

                variantToArgs v =
                    argsToList v
                        |> List.indexedMap (\i _ -> "arg" ++ String.fromInt (i + 1))
                        |> String.join " "

                variantSize v =
                    List.length (argsToList v)

                argsToList v =
                    case v of
                        Variant0Type ->
                            []

                        Variant1Type arg1 ->
                            [ arg1 ]

                        Variant2Type arg1 arg2 ->
                            [ arg1, arg2 ]

                        Variant3Type arg1 arg2 arg3 ->
                            [ arg1, arg2, arg3 ]

                        Variant4Type arg1 arg2 arg3 arg4 ->
                            [ arg1, arg2, arg3, arg4 ]

                        Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                            [ arg1, arg2, arg3, arg4, arg5 ]

                dtor =
                    anonymousFunction2
                        { args = List.map lowerInitial names ++ [ "variant" ]
                        , body =
                            G.nest 4
                                (G.lines
                                    [ G.fromString "case variant of"
                                    , G.lines
                                        (List.map2
                                            (\name variant ->
                                                let
                                                    args =
                                                        variantToArgs variant
                                                in
                                                G.nest 4
                                                    (G.concat
                                                        [ G.fromString name
                                                        , G.fromString " "
                                                        , G.fromString args
                                                        , G.fromString " ->"
                                                        , G.line
                                                        , G.fromString (lowerInitial name)
                                                        , G.fromString " "
                                                        , G.fromString args
                                                        ]
                                                    )
                                            )
                                            names
                                            types
                                        )
                                    ]
                                )
                        }
            in
            G.nest 4
                (G.group
                    (G.concat
                        [ G.fromString "Gadget.custom"
                        , G.line
                        , dtor
                        , G.line
                        , G.concat
                            (List.map2
                                (\n v ->
                                    G.group
                                        (G.concat
                                            [ pizza2
                                            , G.fromString (" Gadget.variant" ++ String.fromInt (variantSize v))
                                            , G.nest 4
                                                (G.concat
                                                    [ G.space
                                                    , G.fromString ("\"" ++ n ++ "\"")
                                                    , G.space
                                                    , G.fromString n
                                                    , G.space
                                                    , G.group (G.concat (List.map maybeParens2 (argsToList v)))
                                                    ]
                                                )
                                            , G.line
                                            ]
                                        )
                                )
                                names
                                types
                            )
                        , pizza2
                        , G.fromString " "
                        , G.fromString "Gadget.endCustom"
                        ]
                    )
                )

        _ ->
            Debug.todo "not implemented yet"


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
                        quineHelp fld

                    else
                        parens (quineHelp fld)

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

        CustomType _ ( fstName, fstType ) rstNamesAndTypes ->
            let
                names =
                    fstName :: List.map Tuple.first rstNamesAndTypes

                types =
                    fstType :: List.map Tuple.second rstNamesAndTypes

                variantToArgs v =
                    argsToList v
                        |> List.indexedMap (\i _ -> "arg" ++ String.fromInt (i + 1))
                        |> String.join " "

                variantSize v =
                    List.length (argsToList v)

                argsToList v =
                    case v of
                        Variant0Type ->
                            []

                        Variant1Type arg1 ->
                            [ arg1 ]

                        Variant2Type arg1 arg2 ->
                            [ arg1, arg2 ]

                        Variant3Type arg1 arg2 arg3 ->
                            [ arg1, arg2, arg3 ]

                        Variant4Type arg1 arg2 arg3 arg4 ->
                            [ arg1, arg2, arg3, arg4 ]

                        Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                            [ arg1, arg2, arg3, arg4, arg5 ]

                dtor =
                    anonymousFunction
                        { args = List.map lowerInitial names ++ [ "variant" ]
                        , body =
                            P.nest 4
                                (P.lines
                                    [ P.string "case variant of"
                                    , P.lines
                                        (List.map2
                                            (\name variant ->
                                                P.nest 4
                                                    (P.lines
                                                        [ P.words
                                                            [ P.string name
                                                            , P.string (variantToArgs variant)
                                                            , P.string "->"
                                                            ]
                                                        , P.words
                                                            [ P.string (lowerInitial name)
                                                            , P.string (variantToArgs variant)
                                                            ]
                                                        ]
                                                    )
                                            )
                                            names
                                            types
                                        )
                                    ]
                                )
                        }
            in
            P.group
                (P.nest 4
                    (P.lines
                        [ P.lines
                            [ P.string "Gadget.custom"
                            , dtor
                            ]
                        , P.lines
                            (List.map2
                                (\n v ->
                                    P.group
                                        (P.nest 4
                                            (P.lines
                                                [ P.words
                                                    [ pizza
                                                    , P.string ("Gadget.variant" ++ String.fromInt (variantSize v))
                                                    , P.group
                                                        (P.lines
                                                            [ P.string ("\"" ++ n ++ "\"")
                                                            , P.string n
                                                            ]
                                                        )
                                                    ]
                                                , P.lines (List.map maybeParens (argsToList v))
                                                ]
                                            )
                                        )
                                )
                                names
                                types
                            )
                        , P.words
                            [ pizza
                            , P.string "Gadget.endCustom"
                            ]
                        ]
                    )
                )

        ListType _ itemType ->
            P.group
                (P.nest 4
                    (P.lines
                        [ P.string "Gadget.list"
                        , maybeParens itemType
                        ]
                    )
                )

        LazyType _ inner ->
            quineHelp (inner ())

        TupleType _ a b ->
            P.lines [ P.string "Gadget.tuple", quineHelp a, quineHelp b ]

        TripleType _ a b c ->
            P.nest 4 (P.lines [ P.string "Gadget.triple", quineHelp a, quineHelp b, quineHelp c ])


lowerInitial : String -> String
lowerInitial s =
    case String.uncons s of
        Just ( fst, rest ) ->
            String.cons (Char.toLower fst) rest

        Nothing ->
            s


maybeParens : Type -> P.Doc t
maybeParens itemType =
    if isPrimitive itemType then
        quineHelp itemType

    else
        parens (quineHelp itemType)


maybeParens2 : Type -> G.Document
maybeParens2 itemType =
    if isPrimitive itemType then
        quineHelp2 itemType

    else
        parens2 (quineHelp2 itemType)


isPrimitive : Type -> Bool
isPrimitive irType =
    case irType of
        UnitType _ ->
            True

        BoolType _ ->
            True

        CharType _ ->
            True

        StringType _ ->
            True

        IntType _ ->
            True

        FloatType _ ->
            True

        _ ->
            False


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


anonymousFunction2 : { args : List String, body : G.Document } -> G.Document
anonymousFunction2 { args, body } =
    parens2
        (G.fromString ("\\" ++ String.join " " args ++ " ->")
            |> G.prepend G.line
            |> G.prepend body
            |> G.nest 4
        )


parens2 : G.Document -> G.Document
parens2 inner =
    G.fromString "("
        |> G.prepend inner
        |> G.prepend G.line
        |> G.prepend (G.fromString ")")


parens : P.Doc t -> P.Doc t
parens inner =
    P.group
        (P.flatAlt
            (P.char '(' |> P.a inner |> P.a (P.char ')'))
            (P.char '(' |> P.a inner |> P.a P.line |> P.a (P.char ')'))
        )


pizza2 : G.Document
pizza2 =
    G.fromString "|>"


pizza : P.Doc t
pizza =
    P.string "|>"
