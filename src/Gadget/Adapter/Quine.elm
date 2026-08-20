module Gadget.Adapter.Quine exposing (quine)

import Gadget.Adapter.Glam as G
import Gadget.IR as IR exposing (Type(..), VariantType(..))
import Pretty as P


quine : Int -> IR.Gadget a -> String
quine columns gadget =
    IR.irType gadget
        |> quineHelp
        |> G.toString columns


quineHelp : Type -> G.Document
quineHelp irType =
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

                fields =
                    G.group <|
                        joinWithLines
                            (List.map
                                (\( name, fld ) ->
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
                                                , maybeParens fld
                                                ]
                                )
                                namedFields
                            )
            in
            G.group <|
                G.nest 4 <|
                    joinWithLines
                        [ G.group <|
                            joinWithLines
                                [ G.fromString "Gadget.record"
                                , ctor
                                ]
                        , fields
                        , joinWithNonBreakingSpaces
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
                        { alwaysBreak = True
                        , args = List.map lowerInitial names ++ [ "variant" ]
                        , body =
                            G.group <|
                                G.nest 4 <|
                                    joinWithLines
                                        [ G.fromString "case variant of"
                                        , joinWithLines
                                            (List.map2 prettyDtorCase names variants)
                                        ]
                        }

                prettyDtorCase name variant =
                    let
                        argsToString args =
                            args
                                |> List.indexedMap (\i _ -> "arg" ++ String.fromInt (i + 1))
                                |> String.join " "

                        argNamesOrEmpty =
                            case variantToArgsList variant of
                                [] ->
                                    G.empty

                                args ->
                                    G.fromString (argsToString args)
                    in
                    G.nest 4 <|
                        joinWithSpaces
                            [ joinWithNonBreakingSpaces
                                [ G.fromString name
                                , argNamesOrEmpty
                                , G.fromString "->"
                                ]
                            , joinWithNonBreakingSpaces
                                [ G.fromString (lowerInitial name)
                                , argNamesOrEmpty
                                ]
                            ]

                prettyVariant name variant =
                    G.group <|
                        G.nest 4 <|
                            joinWithLines
                                [ G.group <|
                                    joinWithSpaces
                                        [ G.concat
                                            [ pizza
                                            , G.fromString (" Gadget.variant" ++ String.fromInt (variantSize variant))
                                            ]
                                        , G.fromString ("\"" ++ name ++ "\"")
                                        , G.fromString name
                                        ]
                                , case variantToArgsList variant of
                                    [] ->
                                        G.empty

                                    args ->
                                        joinWithSpaces (List.map maybeParens args)
                                ]
            in
            G.nest 4 <|
                joinWithLines
                    [ G.fromString "Gadget.custom"
                    , prettyDtor
                    , joinWithLines (List.map2 prettyVariant names variants)
                    , joinWithNonBreakingSpaces
                        [ pizza
                        , G.fromString "Gadget.endCustom"
                        ]
                    ]

        _ ->
            Debug.todo "not implemented yet"


joinWithSpaces : List G.Document -> G.Document
joinWithSpaces =
    G.join G.space


joinWithNonBreakingSpaces : List G.Document -> G.Document
joinWithNonBreakingSpaces =
    G.join (G.fromString " ")


joinWithLines : List G.Document -> G.Document
joinWithLines =
    G.join G.line


lowerInitial : String -> String
lowerInitial s =
    case String.uncons s of
        Just ( fst, rest ) ->
            String.cons (Char.toLower fst) rest

        Nothing ->
            s


maybeParens : Type -> G.Document
maybeParens itemType =
    if isPrimitive itemType then
        quineHelp itemType

    else
        parens (quineHelp itemType)


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
        (if alwaysBreak then
            parens

         else
            flexParens
        )
        <|
            G.nest 4 <|
                (if alwaysBreak then
                    joinWithLines

                 else
                    joinWithSpaces
                )
                    [ G.fromString ("\\" ++ String.join " " args ++ " ->")
                    , body
                    ]


flexParens : G.Document -> G.Document
flexParens inner =
    G.concat
        [ G.fromString "("
        , inner
        , G.break "" ""
        , G.fromString ")"
        ]


parens : G.Document -> G.Document
parens inner =
    G.concat
        [ G.fromString "("
        , inner
        , G.line
        , G.fromString ")"
        ]


pizza : G.Document
pizza =
    G.fromString "|>"
