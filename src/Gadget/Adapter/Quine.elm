module Gadget.Adapter.Quine exposing (quine)

import Gadget.Adapter.Glam as G
import Gadget.IR as IR exposing (Type(..), VariantType(..))
import Pretty as P


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    IR.irType gadget
        |> quineHelp True
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
                ctor =
                    anonymousFunction
                        { alwaysBreak = False
                        , args = List.map Tuple.first namedFields
                        , body = G.group (anonymousRecord (List.map (\( n, _ ) -> ( n, n )) namedFields))
                        }

                prettyFields =
                    G.group <| joinWithSpaces (List.map prettyField namedFields)

                prettyField ( name, fld ) =
                    G.group <|
                        G.nest 4 <|
                            joinWithSpaces
                                [ G.group <|
                                    joinWithSpaces
                                        [ joinWithNonBreakingSpaces
                                            [ pizza
                                            , G.fromString "Gadget.field"
                                            ]
                                        , G.fromString ("\"" ++ name ++ "\"")
                                        , G.fromString ("." ++ name)
                                        ]
                                , quineHelp False fld
                                ]

                pretty =
                    G.group <|
                        G.nest 4 <|
                            G.forceBreak <|
                                joinWithSpaces
                                    [ G.group <|
                                        joinWithSpaces
                                            [ G.fromString "Gadget.record"
                                            , ctor
                                            ]
                                    , prettyFields
                                    , joinWithNonBreakingSpaces
                                        [ pizza
                                        , G.fromString "Gadget.endRecord"
                                        ]
                                    ]
            in
            if isRoot then
                pretty

            else
                pretty
                    |> G.prepend (G.fromString "(")
                    |> G.append (G.softBreak |> G.append (G.fromString ")"))
                    |> G.group

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
                        { alwaysBreak = True
                        , args = List.map lowerInitial names ++ [ "variant" ]
                        , body =
                            G.group <|
                                G.nest 4 <|
                                    G.forceBreak <|
                                        joinWithSpaces
                                            [ G.fromString "case variant of"
                                            , joinWithSpaces
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
                        joinWithSpaces
                            [ joinWithNonBreakingSpaces
                                [ G.fromString name
                                , joinWithNonBreakingSpaces args
                                , G.fromString "->"
                                ]
                            , joinWithSpaces
                                [ G.fromString (lowerInitial name)
                                , joinWithSpaces args
                                ]
                                |> G.nest 4
                                |> G.group
                            ]

                prettyVariant name variant =
                    G.group <|
                        G.nest 4 <|
                            joinWithSpaces
                                [ G.group <|
                                    joinWithSpaces
                                        [ joinWithNonBreakingSpaces
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
                                            joinWithSpaces (List.map (quineHelp False) args)
                                ]

                pretty =
                    G.nest 4 <|
                        G.forceBreak <|
                            joinWithSpaces
                                [ G.fromString "Gadget.custom"
                                , prettyDtor
                                , joinWithSpaces (List.map2 prettyVariant names variants)
                                , joinWithNonBreakingSpaces
                                    [ pizza
                                    , G.fromString "Gadget.endCustom"
                                    ]
                                ]
            in
            if isRoot then
                pretty

            else
                pretty
                    |> G.prepend (G.fromString "(")
                    |> G.append (G.softBreak |> G.append (G.fromString ")"))
                    |> G.group

        ListType _ itemType ->
            let
                pretty =
                    G.nest 4 <|
                        G.group <|
                            joinWithSpaces
                                [ G.fromString "Gadget.list"
                                , quineHelp False itemType
                                ]
            in
            if isRoot then
                pretty

            else
                pretty
                    |> G.prepend (G.fromString "(")
                    |> G.append (G.softBreak |> G.append (G.fromString ")"))
                    |> G.group

        LazyType _ inner ->
            quineHelp isRoot (inner ())

        TupleType _ a b ->
            let
                pretty =
                    G.nest 4 <|
                        G.group <|
                            joinWithSpaces
                                [ G.fromString "Gadget.tuple"
                                , quineHelp False a
                                , quineHelp False b
                                ]
            in
            if isRoot then
                pretty

            else
                pretty
                    |> G.prepend (G.fromString "(")
                    |> G.append (G.softBreak |> G.append (G.fromString ")"))
                    |> G.group

        TripleType _ a b c ->
            let
                pretty =
                    G.nest 4 <|
                        G.group <|
                            joinWithSpaces
                                [ G.fromString "Gadget.tuple"
                                , quineHelp False a
                                , quineHelp False b
                                , quineHelp False c
                                ]
            in
            if isRoot then
                pretty

            else
                pretty
                    |> G.prepend (G.fromString "(")
                    |> G.append (G.softBreak |> G.append (G.fromString ")"))
                    |> G.group


joinWithSpaces : List G.Document -> G.Document
joinWithSpaces =
    G.join G.space


joinWithNonBreakingSpaces : List G.Document -> G.Document
joinWithNonBreakingSpaces =
    G.join (G.fromString " ")


lowerInitial : String -> String
lowerInitial s =
    case String.uncons s of
        Just ( fst, rest ) ->
            String.cons (Char.toLower fst) rest

        Nothing ->
            s


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


anonymousRecord : List ( String, String ) -> G.Document
anonymousRecord fieldNamesAndValues =
    case fieldNamesAndValues of
        [] ->
            G.fromString "{}"

        first :: rest ->
            let
                printField ( name, value ) =
                    G.group <|
                        G.nest 4 <|
                            joinWithSpaces
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


anonymousFunction : { alwaysBreak : Bool, args : List String, body : G.Document } -> G.Document
anonymousFunction { alwaysBreak, args, body } =
    G.group <|
        parens <|
            G.nest 4 <|
                (if alwaysBreak then
                    G.forceBreak

                 else
                    identity
                )
                <|
                    joinWithSpaces
                        [ G.fromString ("\\" ++ String.join " " args ++ " ->")
                        , body
                        ]


parens : G.Document -> G.Document
parens inner =
    G.concat
        [ G.fromString "("
        , inner
        , G.space
        , G.fromString ")"
        ]


pizza : G.Document
pizza =
    G.fromString "|>"
