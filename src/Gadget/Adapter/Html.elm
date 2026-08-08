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

import Gadget.IR as IR
import Html as H
import Html.Attributes as HA


type alias IRValue =
    IR.IR IR.Value


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
view gadget value =
    IR.fromInput gadget value
        |> htmlAdapter
        |> List.singleton
        |> H.article [ HA.class "elm-gadget" ]


htmlAdapter : IRValue -> H.Html msg
htmlAdapter (IR.IR metadata irValue) =
    let
        viewMetadata =
            case tools.export metadata of
                [] ->
                    H.text ""

                _ ->
                    H.aside [ HA.class "metadata" ]
                        [ H.strong [] [ H.text "Metadata" ]
                        , H.table []
                            (tools.export metadata
                                |> List.concatMap
                                    (\( adapterId, kvs ) ->
                                        H.tr []
                                            [ H.th [] [ H.text "Adapter" ]
                                            , H.th [] [ H.text "Key" ]
                                            , H.th [] [ H.text "Value" ]
                                            ]
                                            :: List.map
                                                (\( k, IR.IR _ v ) ->
                                                    H.tr []
                                                        [ H.td [] [ H.text ("\"" ++ adapterId ++ "\"") ]
                                                        , H.td [] [ H.text ("\"" ++ k ++ "\"") ]
                                                        , H.td [] [ H.text (viewMetadataValue v) ]
                                                        ]
                                                )
                                                kvs
                                    )
                            )
                        ]

        argsToList variant =
            case variant of
                IR.Variant0Value ->
                    []

                IR.Variant1Value arg ->
                    [ arg ]

                IR.Variant2Value arg1 arg2 ->
                    [ arg1
                    , arg2
                    ]

                IR.Variant3Value arg1 arg2 arg3 ->
                    [ arg1
                    , arg2
                    , arg3
                    ]

                IR.Variant4Value arg1 arg2 arg3 arg4 ->
                    [ arg1
                    , arg2
                    , arg3
                    , arg4
                    ]

                IR.Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                    [ arg1
                    , arg2
                    , arg3
                    , arg4
                    , arg5
                    ]

        viewMetadataValue metadataValue =
            case metadataValue of
                IR.UnitValue ->
                    "() : Unit"

                IR.BoolValue b ->
                    if b then
                        "True : Bool"

                    else
                        "False : Bool"

                IR.CharValue c ->
                    "'" ++ String.fromChar c ++ "' : Char"

                IR.StringValue s ->
                    "\"" ++ s ++ "\" : String"

                IR.IntValue i ->
                    String.fromInt i ++ " : Int"

                IR.FloatValue f ->
                    String.fromFloat f ++ " : Float"

                IR.CustomValue selected ( name, variant ) ->
                    name ++ String.fromInt selected ++ " " ++ String.join " " (List.map (\(IR.IR _ v) -> viewMetadataValue v) (argsToList variant))

                IR.RecordValue fields ->
                    String.join " " (List.map (\( _, IR.IR _ v ) -> viewMetadataValue v) fields)

                IR.ListValue items ->
                    String.join " " (List.map (\(IR.IR _ v) -> viewMetadataValue v) items)

                IR.TupleValue a b ->
                    String.join " " (List.map (\(IR.IR _ v) -> viewMetadataValue v) [ a, b ])

                IR.TripleValue a b c ->
                    String.join " " (List.map (\(IR.IR _ v) -> viewMetadataValue v) [ a, b, c ])

        viewValue metadataHtml v =
            case v of
                IR.UnitValue ->
                    unquotedPrimitive metadataHtml
                        "Unit"
                        "()"

                IR.BoolValue b ->
                    unquotedPrimitive metadataHtml
                        "Bool"
                        (if b then
                            "True"

                         else
                            "False"
                        )

                IR.CharValue c ->
                    quotedPrimitive metadataHtml "'" "Char" (String.fromChar c)

                IR.StringValue s ->
                    quotedPrimitive metadataHtml "\"" "String" s

                IR.IntValue i ->
                    unquotedPrimitive metadataHtml "Int" (String.fromInt i)

                IR.FloatValue f ->
                    unquotedPrimitive metadataHtml "Float" (String.fromFloat f)

                IR.CustomValue _ ( name, variant ) ->
                    let
                        args =
                            case variant of
                                IR.Variant0Value ->
                                    []

                                IR.Variant1Value arg ->
                                    [ arg ]

                                IR.Variant2Value arg1 arg2 ->
                                    [ arg1
                                    , arg2
                                    ]

                                IR.Variant3Value arg1 arg2 arg3 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    ]

                                IR.Variant4Value arg1 arg2 arg3 arg4 ->
                                    [ arg1
                                    , arg2
                                    , arg3
                                    , arg4
                                    ]

                                IR.Variant5Value arg1 arg2 arg3 arg4 arg5 ->
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
                        metadataHtml
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
                        (List.map (Tuple.pair "") args)

                IR.RecordValue fields ->
                    let
                        count =
                            List.length fields
                    in
                    combinator
                        metadataHtml
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
                        fields

                IR.ListValue items ->
                    let
                        count =
                            List.length items
                    in
                    combinator
                        metadataHtml
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
                        (List.map (Tuple.pair "") items)

                IR.TupleValue a b ->
                    combinator
                        metadataHtml
                        "Tuple"
                        ""
                        (List.map (Tuple.pair "") [ a, b ])

                IR.TripleValue a b c ->
                    combinator
                        metadataHtml
                        "Triple"
                        ""
                        (List.map (Tuple.pair "") [ a, b, c ])
    in
    viewValue viewMetadata irValue


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


combinator : H.Html msg -> String -> String -> List ( String, IRValue ) -> H.Html msg
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
                    (List.map (\( name, item ) -> H.li [] [ H.text name, htmlAdapter item ]) items)
                , metadataHtml
                ]
            ]
