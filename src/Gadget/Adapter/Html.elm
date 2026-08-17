module Gadget.Adapter.Html exposing (view)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to render an Elm value as HTML.

It's not very beautiful though! I wrote this adapter mainly to help me with
debugging while I was writing this package.


## API

@docs view

-}

import Gadget.IR as IR exposing (Type(..), Value(..), VariantType(..), VariantValue(..))
import Html as H
import Html.Attributes as HA
import List.Extra


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Html"


{-| Convert a value into an `Html msg`.

    import Gadget
    import Gadget.Adapter.Html
    import Html

    gadget =
        Gadget.int

    view =
        Gadget.Adapter.Html.view gadget 1

    view --: Html.Html msg

-}
view : IR.Gadget a -> a -> H.Html msg
view gadget input =
    let
        value =
            IR.fromInput gadget input

        irType =
            IR.irType gadget
    in
    htmlAdapter value irType
        |> List.singleton
        |> H.article [ HA.class "elm-gadget" ]


htmlAdapter : Value -> Type -> H.Html msg
htmlAdapter irValue irType =
    let
        viewMetadata metadata =
            case tools.debug metadata of
                [] ->
                    H.text ""

                debugged ->
                    H.aside [ HA.class "metadata" ]
                        [ H.strong [] [ H.text "Metadata" ]
                        , H.table []
                            (debugged
                                |> List.concatMap
                                    (\( adapterId, kvs ) ->
                                        H.tr []
                                            [ H.th [] [ H.text "Adapter" ]
                                            , H.th [] [ H.text "Key" ]
                                            , H.th [] [ H.text "Value" ]
                                            ]
                                            :: List.map
                                                (\( k, v ) ->
                                                    H.tr []
                                                        [ H.td [] [ H.text ("\"" ++ adapterId ++ "\"") ]
                                                        , H.td [] [ H.text ("\"" ++ k ++ "\"") ]
                                                        , H.td [] [ H.text v ]
                                                        ]
                                                )
                                                kvs
                                    )
                            )
                        ]

        viewValue metadataHtml v t =
            case ( v, t ) of
                ( UnitValue, UnitType metadata ) ->
                    unquotedPrimitive (metadataHtml metadata)
                        "Unit"
                        "()"

                ( BoolValue b, BoolType metadata ) ->
                    unquotedPrimitive (metadataHtml metadata)
                        "Bool"
                        (if b then
                            "True"

                         else
                            "False"
                        )

                ( CharValue c, CharType metadata ) ->
                    quotedPrimitive (metadataHtml metadata) "'" "Char" (String.fromChar c)

                ( StringValue s, StringType metadata ) ->
                    quotedPrimitive (metadataHtml metadata) "\"" "String" s

                ( IntValue i, IntType metadata ) ->
                    unquotedPrimitive (metadataHtml metadata) "Int" (String.fromInt i)

                ( FloatValue f, FloatType metadata ) ->
                    unquotedPrimitive (metadataHtml metadata) "Float" (String.fromFloat f)

                ( CustomValue idx ( name, variant ), CustomType metadata fst rst ) ->
                    let
                        variantType =
                            List.Extra.getAt idx (fst :: rst)
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault Variant0Type

                        args =
                            case variant of
                                Variant0Value ->
                                    []

                                Variant1Value arg ->
                                    [ arg ]

                                Variant2Value arg1 arg2 ->
                                    [ arg1
                                    , arg2
                                    ]

                                Variant3Value arg1 arg2 arg3 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    ]

                                Variant4Value arg1 arg2 arg3 arg4 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    , arg4
                                    ]

                                Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    , arg4
                                    , arg5
                                    ]

                        argTypes =
                            case variantType of
                                Variant0Type ->
                                    []

                                Variant1Type arg ->
                                    [ arg ]

                                Variant2Type arg1 arg2 ->
                                    [ arg1
                                    , arg2
                                    ]

                                Variant3Type arg1 arg2 arg3 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    ]

                                Variant4Type arg1 arg2 arg3 arg4 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    , arg4
                                    ]

                                Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    , arg4
                                    , arg5
                                    ]

                        numArgs =
                            List.length args
                    in
                    combinator
                        (metadataHtml metadata)
                        "Variant"
                        ("\""
                            ++ name
                            ++ "\" with "
                            ++ String.fromInt numArgs
                            ++ " argument"
                            ++ (if numArgs == 1 then
                                    ""

                                else
                                    "s"
                               )
                        )
                        (List.map2 (\arg argType -> ( "", arg, argType )) args argTypes)

                ( RecordValue fields, RecordType metadata fieldTypes ) ->
                    let
                        count =
                            List.length fields
                    in
                    combinator
                        (metadataHtml metadata)
                        "Record"
                        ("with "
                            ++ String.fromInt count
                            ++ " field"
                            ++ (if count == 1 then
                                    ""

                                else
                                    "s"
                               )
                        )
                        (List.map2
                            (\( name, field ) ( _, fieldType ) -> ( name, field, fieldType ))
                            fields
                            fieldTypes
                        )

                ( ListValue items, ListType metadata itemType ) ->
                    let
                        count =
                            List.length items
                    in
                    combinator
                        (metadataHtml metadata)
                        "List"
                        ("with "
                            ++ String.fromInt count
                            ++ " item"
                            ++ (if count == 1 then
                                    ""

                                else
                                    "s"
                               )
                        )
                        (List.map (\item -> ( "", item, itemType )) items)

                ( TupleValue a b, TupleType metadata aType bType ) ->
                    combinator
                        (metadataHtml metadata)
                        "Tuple"
                        ""
                        (List.map2
                            (\elem elemType -> ( "", elem, elemType ))
                            [ a, b ]
                            [ aType, bType ]
                        )

                ( TripleValue a b c, TripleType metadata aType bType cType ) ->
                    combinator
                        (metadataHtml metadata)
                        "Triple"
                        ""
                        (List.map2
                            (\elem elemType -> ( "", elem, elemType ))
                            [ a, b, c ]
                            [ aType, bType, cType ]
                        )

                _ ->
                    H.text "Error!"
    in
    viewValue viewMetadata irValue irType


primitive : H.Html msg -> H.Html msg -> (String -> H.Html msg) -> String -> String -> H.Html msg
primitive metadataHtml quoteHtml valueWrapper typeName value =
    H.div [ HA.class "primitive", HA.class typeName ]
        [ H.em [ HA.class "type-name" ] [ H.text typeName ]
        , H.span [ HA.class "value" ] [ quoteHtml, valueWrapper value, quoteHtml ]
        , metadataHtml
        ]


quotedPrimitive : H.Html msg -> String -> String -> String -> H.Html msg
quotedPrimitive metadataHtml quote =
    primitive metadataHtml (H.span [ HA.class "quote" ] [ H.text quote ]) (\value -> H.code [] [ H.text value ])


unquotedPrimitive : H.Html msg -> String -> String -> H.Html msg
unquotedPrimitive metadataHtml =
    primitive metadataHtml (H.text "") (\value -> H.code [] [ H.text value ])


combinator : H.Html msg -> String -> String -> List ( String, Value, Type ) -> H.Html msg
combinator metadataHtml typeName typeInfo items =
    if List.isEmpty items then
        H.div [ HA.class "combinator", HA.class typeName ]
            [ H.summary []
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ typeInfo)
                ]
            , metadataHtml
            ]

    else
        H.details [ HA.class "combinator", HA.class typeName ]
            [ H.summary [ HA.class "type-name" ]
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ typeInfo)
                ]
            , H.div []
                [ H.ol []
                    (List.map (\( name, item, type_ ) -> H.li [] [ H.text name, htmlAdapter item type_ ]) items)
                , metadataHtml
                ]
            ]
