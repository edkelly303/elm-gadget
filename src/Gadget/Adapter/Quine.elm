module Gadget.Adapter.Quine exposing (quine)

import Gadget.Adapter.Glam as G
import Gadget.IR as IR exposing (Type(..), VariantType(..))


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    breakable
        [ G.fromString "gadget ="
        , quineHelp True (IR.irType gadget)
        ]
        |> G.nest 4
        |> G.group
        |> G.toString columns


quineHelp : Bool -> Type -> G.Document
quineHelp isRoot irType =
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

        RecordType _ namedFields ->
            let
                prettyCtor =
                    anonymousFunction
                        { args = List.map Tuple.first namedFields
                        , body = G.group (anonymousRecord (List.map (\( n, _ ) -> ( n, n )) namedFields))
                        }

                prettyFields =
                    G.group <|
                        breakable (List.map prettyField namedFields)

                prettyField ( name, fld ) =
                    G.group <|
                        G.nest 4 <|
                            breakable
                                [ G.group <|
                                    breakable
                                        [ unbreakable
                                            [ pizza
                                            , G.fromString "Gadget.field"
                                            ]
                                        , G.fromString ("\"" ++ name ++ "\"")
                                        , G.fromString ("." ++ name)
                                        ]
                                , quineHelp False fld
                                ]
            in
            parenthesizeIfNotRoot isRoot <|
                G.group <|
                    G.nest 4 <|
                        G.forceBreak <|
                            breakable
                                [ G.group <|
                                    breakable
                                        [ G.fromString "Gadget.record"
                                        , prettyCtor
                                        ]
                                , prettyFields
                                , unbreakable
                                    [ pizza
                                    , G.fromString "Gadget.endRecord"
                                    ]
                                ]

        CustomType _ ( firstName, firstVariant ) restNamesAndVariants ->
            let
                names =
                    firstName :: List.map Tuple.first restNamesAndVariants

                variants =
                    firstVariant :: List.map Tuple.second restNamesAndVariants

                variantSize v =
                    List.length (variantToArgsList v)

                variantToArgsList v =
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

                prettyDtor =
                    anonymousFunction
                        { args = List.map lowerInitial names ++ [ "variant" ]
                        , body =
                            G.group <|
                                G.nest 4 <|
                                    G.forceBreak <|
                                        breakable
                                            [ G.fromString "case variant of"
                                            , breakable
                                                (List.map2 prettyDtorCase names variants)
                                            ]
                        }

                prettyDtorCase name variant =
                    let
                        args =
                            variantToArgsList variant
                                |> List.indexedMap (\i _ -> G.fromString ("arg" ++ String.fromInt (i + 1)))
                    in
                    G.nest 4 <|
                        breakable
                            [ unbreakable
                                [ G.fromString name
                                , unbreakable args
                                , G.fromString "->"
                                ]
                            , G.group <|
                                G.nest 4 <|
                                    breakable
                                        [ G.fromString (lowerInitial name)
                                        , breakable args
                                        ]
                            ]

                prettyVariant name variant =
                    G.group <|
                        G.nest 4 <|
                            breakable
                                [ G.group <|
                                    breakable
                                        [ unbreakable
                                            [ pizza
                                            , G.fromString ("Gadget.variant" ++ String.fromInt (variantSize variant))
                                            ]
                                        , G.fromString ("\"" ++ name ++ "\"")
                                        , G.fromString name
                                        ]
                                , case variantToArgsList variant of
                                    [] ->
                                        G.empty

                                    args ->
                                        G.group <|
                                            breakable (List.map (quineHelp False) args)
                                ]
            in
            parenthesizeIfNotRoot isRoot <|
                G.nest 4 <|
                    G.forceBreak <|
                        breakable
                            [ G.fromString "Gadget.custom"
                            , prettyDtor
                            , breakable (List.map2 prettyVariant names variants)
                            , unbreakable
                                [ pizza
                                , G.fromString "Gadget.endCustom"
                                ]
                            ]

        ListType _ itemType ->
            parenthesizeIfNotRoot isRoot <|
                G.nest 4 <|
                    G.group <|
                        breakable
                            [ G.fromString "Gadget.list"
                            , quineHelp False itemType
                            ]

        LazyType _ inner ->
            quineHelp isRoot (inner ())

        TupleType _ a b ->
            parenthesizeIfNotRoot isRoot <|
                G.nest 4 <|
                    G.group <|
                        breakable
                            [ G.fromString "Gadget.tuple"
                            , quineHelp False a
                            , quineHelp False b
                            ]

        TripleType _ a b c ->
            parenthesizeIfNotRoot isRoot <|
                G.nest 4 <|
                    G.group <|
                        breakable
                            [ G.fromString "Gadget.triple"
                            , quineHelp False a
                            , quineHelp False b
                            , quineHelp False c
                            ]


breakable : List G.Document -> G.Document
breakable =
    G.join G.space


unbreakable : List G.Document -> G.Document
unbreakable =
    G.join (G.fromString " ")


pizza : G.Document
pizza =
    G.fromString "|>"


parens : G.Document -> G.Document
parens inner =
    G.concat
        [ G.fromString "("
        , inner
        , G.softBreak
        , G.fromString ")"
        ]


parenthesizeIfNotRoot : Bool -> G.Document -> G.Document
parenthesizeIfNotRoot isRoot doc =
    if isRoot then
        doc

    else
        parens doc


lowerInitial : String -> String
lowerInitial s =
    case String.uncons s of
        Just ( fst, rest ) ->
            String.cons (Char.toLower fst) rest

        Nothing ->
            s


anonymousRecord : List ( String, String ) -> G.Document
anonymousRecord fieldNamesAndValues =
    case fieldNamesAndValues of
        [] ->
            G.fromString "{}"

        _ ->
            let
                printField ( name, value ) =
                    G.group <|
                        G.nest 4 <|
                            breakable
                                [ G.fromString (name ++ " =")
                                , G.fromString value
                                ]
            in
            G.group <|
                G.concat
                    [ G.fromString "{ "
                    , G.join
                        (G.concat
                            [ G.softBreak
                            , G.fromString ", "
                            ]
                        )
                        (List.map printField fieldNamesAndValues)
                    , G.space
                    , G.fromString "}"
                    ]


anonymousFunction : { args : List String, body : G.Document } -> G.Document
anonymousFunction { args, body } =
    G.group <|
        parens <|
            G.nest 4 <|
                breakable
                    [ G.fromString ("\\" ++ String.join " " args ++ " ->")
                    , body
                    ]
