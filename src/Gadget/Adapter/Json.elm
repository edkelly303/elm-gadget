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

import Dict
import Gadget.IR as IR exposing (IR(..), Type(..), Value(..), VariantType(..), VariantValue(..))
import Json.Decode as JD
import Json.Decode.Extra
import Json.Encode as JE


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
    decodeAdapter (IR.irType gadget)
        |> JD.andThen
            (\ir ->
                case IR.toOutput gadget ir of
                    Ok s ->
                        JD.succeed s

                    Err e ->
                        JD.fail e
            )


encodeAdapter : IR Value -> JE.Value
encodeAdapter (IR.IR _ irValue) =
    case irValue of
        IR.BoolValue b ->
            JE.bool b

        IR.CharValue c ->
            JE.string (String.fromChar c)

        IR.StringValue s ->
            JE.string s

        IR.IntValue i ->
            JE.int i

        IR.FloatValue f ->
            JE.float f

        IR.CustomValue selected ( name, variant ) ->
            JE.object
                [ ( "tag", JE.string name )
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

        IR.ProductValue fields ->
            JE.object (List.map (\( name, field ) -> ( name, encodeAdapter field )) fields)

        IR.ListValue items ->
            JE.list encodeAdapter items


decodeAdapter : IR Type -> JD.Decoder (IR Value)
decodeAdapter (IR metadata irType) =
    case irType of
        BoolType ->
            JD.bool |> JD.map IR.BoolValue |> JD.map (IR metadata)

        CharType ->
            JD.string
                |> JD.andThen
                    (\s ->
                        case String.uncons s of
                            Nothing ->
                                JD.fail "not a char"

                            Just ( c, _ ) ->
                                JD.succeed (IR metadata (CharValue c))
                    )

        StringType ->
            JD.string |> JD.map IR.StringValue |> JD.map (IR metadata)

        IntType ->
            JD.int |> JD.map IR.IntValue |> JD.map (IR metadata)

        FloatType ->
            JD.float |> JD.map IR.FloatValue |> JD.map (IR metadata)

        CustomType fst rst ->
            let
                dict =
                    (fst :: rst)
                        |> List.indexedMap
                            (\idx ( name, variant ) ->
                                ( name, ( idx, variant ) )
                            )
                        |> Dict.fromList
            in
            JD.field "tag" JD.string
                |> JD.andThen
                    (\name ->
                        case Dict.get name dict of
                            Just ( idx, type_ ) ->
                                JD.map (\v -> CustomValue idx ( name, v )) <|
                                    case type_ of
                                        Variant0Type ->
                                            JD.succeed Variant0Value

                                        Variant1Type arg1Type ->
                                            JD.map Variant1Value
                                                (JD.field "args" (JD.index 0 (JD.lazy (\() -> decodeAdapter arg1Type))))

                                        Variant2Type arg1Type arg2Type ->
                                            JD.map2 Variant2Value
                                                (JD.field "args" (JD.index 0 (JD.lazy (\() -> decodeAdapter arg1Type))))
                                                (JD.field "args" (JD.index 1 (JD.lazy (\() -> decodeAdapter arg2Type))))

                                        Variant3Type arg1Type arg2Type arg3Type ->
                                            JD.map3 Variant3Value
                                                (JD.field "args" (JD.index 0 (JD.lazy (\() -> decodeAdapter arg1Type))))
                                                (JD.field "args" (JD.index 1 (JD.lazy (\() -> decodeAdapter arg2Type))))
                                                (JD.field "args" (JD.index 2 (JD.lazy (\() -> decodeAdapter arg3Type))))

                                        Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                                            JD.map4 Variant4Value
                                                (JD.field "args" (JD.index 0 (JD.lazy (\() -> decodeAdapter arg1Type))))
                                                (JD.field "args" (JD.index 1 (JD.lazy (\() -> decodeAdapter arg2Type))))
                                                (JD.field "args" (JD.index 2 (JD.lazy (\() -> decodeAdapter arg3Type))))
                                                (JD.field "args" (JD.index 3 (JD.lazy (\() -> decodeAdapter arg4Type))))

                                        Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                                            JD.map5 Variant5Value
                                                (JD.field "args" (JD.index 0 (JD.lazy (\() -> decodeAdapter arg1Type))))
                                                (JD.field "args" (JD.index 1 (JD.lazy (\() -> decodeAdapter arg2Type))))
                                                (JD.field "args" (JD.index 2 (JD.lazy (\() -> decodeAdapter arg3Type))))
                                                (JD.field "args" (JD.index 3 (JD.lazy (\() -> decodeAdapter arg4Type))))
                                                (JD.field "args" (JD.index 4 (JD.lazy (\() -> decodeAdapter arg5Type))))

                            Nothing ->
                                JD.fail (name ++ " is not a valid variant name for this type")
                    )
                |> JD.map (IR metadata)

        ProductType fieldTypes ->
            List.map
                (\( name, fieldType ) ->
                    JD.field name
                        (JD.lazy
                            (\() ->
                                JD.map
                                    (Tuple.pair name)
                                    (decodeAdapter fieldType)
                            )
                        )
                )
                fieldTypes
                |> Json.Decode.Extra.combine
                |> JD.map ProductValue
                |> JD.map (IR metadata)

        ListType itemType ->
            JD.list (JD.lazy (\() -> decodeAdapter itemType))
                |> JD.map ListValue
                |> JD.map (IR metadata)

        LazyType innerType ->
            JD.lazy (\() -> decodeAdapter (innerType ()))
