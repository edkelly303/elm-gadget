module Gadget.Adapter.Quine exposing (quine)

import Dict
import Gadget.IR as IR exposing (Metadata, Type(..), VariantType(..))
import Glam as G


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Quine"


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    breakable
        [ G.fromString "gadget ="
        , quineHelp False (IR.irType gadget)
        ]
        |> nested
        |> G.toString columns


quineHelp : Bool -> Type -> G.Document
quineHelp isChildNode irType =
    let
        prettyData =
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
                            nested <|
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
                                    , quineHelp True fld
                                    ]
                    in
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
                                    nested <|
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
                                -- we want this to always break, so no G.group here, just G.nest.
                                breakable
                                    [ unbreakable
                                        [ G.fromString name
                                        , unbreakable args
                                        , G.fromString "->"
                                        ]
                                    , nested <|
                                        breakable
                                            [ G.fromString (lowerInitial name)
                                            , breakable args
                                            ]
                                    ]

                        prettyVariant name variant =
                            nested <|
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
                                                breakable (List.map (quineHelp True) args)
                                    ]
                    in
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
                    breakable
                        [ G.fromString "Gadget.list"
                        , quineHelp True itemType
                        ]

                LazyType _ inner ->
                    quineHelp isChildNode (inner ())

                TupleType _ a b ->
                    breakable
                        [ G.fromString "Gadget.tuple"
                        , quineHelp True a
                        , quineHelp True b
                        ]

                TripleType _ a b c ->
                    breakable
                        [ G.fromString "Gadget.triple"
                        , quineHelp True a
                        , quineHelp True b
                        , quineHelp True c
                        ]

        debuggedMetadata =
            tools.extract irType
                |> tools.debug

        prettyMetadata =
            debuggedMetadata
                |> List.concatMap
                    (\( adapterName, fnNames ) ->
                        List.map
                            (\( fnName, value ) ->
                                nested <|
                                    breakable
                                        [ unbreakable
                                            [ pizza
                                            , G.fromString (adapterName ++ "." ++ fnName)
                                            ]
                                        , G.fromString value
                                        ]
                            )
                            fnNames
                    )
                |> breakable

        hasMetadata =
            [] /= debuggedMetadata

        isCombinator =
            case irType of
                UnitType m ->
                    False

                BoolType m ->
                    False

                CharType m ->
                    False

                StringType m ->
                    False

                IntType m ->
                    False

                FloatType m ->
                    False

                _ ->
                    True
    in
    if hasMetadata && isChildNode then
        parens <|
            breakable
                [ prettyData
                , prettyMetadata
                ]

    else if hasMetadata then
        nested <|
            breakable
                [ prettyData
                , prettyMetadata
                ]

    else if isCombinator && isChildNode then
        parens prettyData

    else
        nested prettyData


nested : G.Document -> G.Document
nested doc =
    doc
        |> G.nest 4
        |> G.group


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
        , G.nest 4 inner
        , G.softBreak
        , G.fromString ")"
        ]
        |> G.group


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
                    nested <|
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
    parens <|
        breakable
            [ G.fromString ("\\" ++ String.join " " args ++ " ->")
            , body
            ]
