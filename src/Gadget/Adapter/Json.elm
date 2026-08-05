module Gadget.Adapter.Json exposing (encode, decoder)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to convert Elm values into a JSON representation, and vice
versa.


## API

@docs encode, decoder

-}

import Gadget.IR as IR
import Json.Decode as JD
import Json.Encode as JE


type alias IRValue =
    IR.IR IR.Value


{-| Convert an Elm value into a `Json.Encode.Value`.

    import Gadget
    import Gadget.Adapter.Json
    import Json.Encode

    gadget =
        Gadget.int

    json =
        Gadget.Adapter.Json.encode gadget 1

    json --: Json.Encode.Value

    jsonString =
        Json.Encode.encode 0 json

    jsonString --> "{\"int\":1}"

-}
encode : IR.Gadget a -> a -> JE.Value
encode gadget value =
    value
        |> IR.fromInput gadget
        |> encodeAdapter


{-| Convert a Gadget into a `Json.Decode.Decoder`.

    import Gadget
    import Gadget.Adapter.Json
    import Json.Decode

    gadget =
        Gadget.int

    jsonString =
        "{\"int\":1}"

    decoder =
        Gadget.Adapter.Json.decoder gadget

    result =
        Json.Decode.decodeString decoder jsonString

    result --> Ok 1

-}
decoder : IR.Gadget a -> JD.Decoder a
decoder gadget =
    decodeAdapter
        |> JD.andThen
            (\irValue ->
                case IR.toOutput gadget irValue of
                    Ok s ->
                        JD.succeed s

                    Err e ->
                        JD.fail e
            )


encodeAdapter : IRValue -> JE.Value
encodeAdapter (IR.IR _ irValue) =
    case irValue of
        IR.BoolValue b ->
            JE.object
                [ ( "bool", JE.bool b ) ]

        IR.CharValue c ->
            JE.object
                [ ( "char", JE.string (String.fromChar c) ) ]

        IR.StringValue s ->
            JE.object
                [ ( "string", JE.string s ) ]

        IR.IntValue i ->
            JE.object
                [ ( "int", JE.int i ) ]

        IR.FloatValue f ->
            JE.object
                [ ( "float", JE.float f ) ]

        IR.CustomValue selected variant ->
            JE.object
                [ ( "custom"
                  , JE.object
                        [ ( "tag", JE.int selected )
                        , ( "args"
                          , JE.list encodeAdapter
                                (case variant of
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
                                )
                          )
                        ]
                  )
                ]

        IR.ProductValue fields ->
            JE.object
                [ ( "product"
                  , JE.list encodeAdapter fields
                  )
                ]

        IR.ListValue items ->
            JE.object
                [ ( "list"
                  , JE.list encodeAdapter items
                  )
                ]


decodeAdapter : JD.Decoder IRValue
decodeAdapter =
    JD.map IR.ir <|
        JD.oneOf
            [ JD.field "bool" JD.bool |> JD.map IR.BoolValue
            , JD.field "char" JD.string
                |> JD.andThen
                    (\s ->
                        case String.uncons s of
                            Nothing ->
                                JD.fail "not a char"

                            Just ( c, _ ) ->
                                JD.succeed (IR.CharValue c)
                    )
            , JD.field "string" JD.string |> JD.map IR.StringValue
            , JD.field "int" JD.int |> JD.map IR.IntValue
            , JD.field "float" JD.float |> JD.map IR.FloatValue
            , JD.field "custom"
                (JD.map2
                    (\selected args ->
                        Maybe.map (IR.CustomValue selected) <|
                            case args of
                                [] ->
                                    Just IR.Variant0Value

                                [ arg ] ->
                                    Just (IR.Variant1Value arg)

                                [ arg1, arg2 ] ->
                                    Just (IR.Variant2Value arg1 arg2)

                                [ arg1, arg2, arg3 ] ->
                                    Just (IR.Variant3Value arg1 arg2 arg3)

                                [ arg1, arg2, arg3, arg4 ] ->
                                    Just (IR.Variant4Value arg1 arg2 arg3 arg4)

                                [ arg1, arg2, arg3, arg4, arg5 ] ->
                                    Just (IR.Variant5Value arg1 arg2 arg3 arg4 arg5)

                                _ ->
                                    Nothing
                    )
                    (JD.field "tag" JD.int)
                    (JD.field "args" (JD.list (JD.lazy (\() -> decodeAdapter))))
                    |> JD.andThen
                        (\maybeIR ->
                            case maybeIR of
                                Nothing ->
                                    JD.fail ""

                                Just ir ->
                                    JD.succeed ir
                        )
                )
            , JD.field "product"
                (JD.list (JD.lazy (\() -> decodeAdapter))
                    |> JD.map IR.ProductValue
                )
            , JD.field "list"
                (JD.list (JD.lazy (\() -> decodeAdapter))
                    |> JD.map IR.ListValue
                )
            ]
