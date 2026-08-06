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


tools : IR.MetadataTools a
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
        dataHtml =
            htmlAdapterHelper irValue
    in
    case tools.export metadata of
        [] ->
            dataHtml

        items ->
            H.dl []
                [ H.div [ HA.class "with-metadata" ]
                    [ H.dt []
                        [ H.em [] [ H.text "Metadata" ]
                        , items
                            |> List.map
                                (\( adapterId, kvs ) ->
                                    H.li []
                                        [ H.text (adapterId ++ ": ")
                                        , H.ol [] (List.map (\( k, v ) -> H.li [] [ H.text k, htmlAdapterHelper v ]) kvs)
                                        ]
                                )
                            |> H.ol []
                        ]
                    , H.dd [] [ dataHtml ]
                    ]
                ]


htmlAdapterHelper : IR.Value -> H.Html msg
htmlAdapterHelper irValue =
    case irValue of
        IR.BoolValue b ->
            unquotedPrimitive "Bool"
                (if b then
                    "True"

                 else
                    "False"
                )

        IR.CharValue c ->
            quotedPrimitive "'" "Char" (String.fromChar c)

        IR.StringValue s ->
            quotedPrimitive "\"" "String" s

        IR.IntValue i ->
            unquotedPrimitive "Int" (String.fromInt i)

        IR.FloatValue f ->
            unquotedPrimitive "Float" (String.fromFloat f)

        IR.CustomValue selected variant ->
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
                "Custom"
                ("variant #"
                    ++ String.fromInt selected
                    ++ " with "
                    ++ String.fromInt numArgs
                    ++ " argument"
                    ++ (if numArgs == 1 then
                            ""

                        else
                            "s"
                       )
                )
                args

        IR.ProductValue fields ->
            let
                count =
                    List.length fields
            in
            combinator
                "Product"
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
                items


primitive : H.Html msg -> (String -> H.Html msg) -> String -> String -> H.Html msg
primitive quoteHtml valueWrapper typeName value =
    H.dl []
        [ H.div [ HA.class "primitive", HA.class typeName ]
            [ H.dt [] [ H.em [] [ H.text typeName ] ]
            , H.dd [] [ H.span [] [ quoteHtml, valueWrapper value, quoteHtml ] ]
            ]
        ]


quotedPrimitive : String -> String -> String -> H.Html msg
quotedPrimitive quote =
    primitive (H.span [ HA.class "quote" ] [ H.text quote ]) (\value -> H.code [] [ H.text value ])


unquotedPrimitive : String -> String -> H.Html msg
unquotedPrimitive =
    primitive (H.text "") (\value -> H.code [] [ H.text value ])


combinator : String -> String -> List IRValue -> H.Html msg
combinator typeName typeInfo items =
    if List.isEmpty items then
        H.div [ HA.class "combinator", HA.class typeName ]
            [ H.summary []
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ typeInfo)
                ]
            ]

    else
        H.details [ HA.class "combinator", HA.class typeName ]
            [ H.summary []
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ typeInfo)
                ]
            , H.ol []
                (List.map (\item -> H.li [] [ htmlAdapter item ]) items)
            ]
