module Gadget.Adapter.Random exposing
    ( generator
    , intRange, floatRange, listLength
    )

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to create a `Random.Generator` for use with functions from the
`elm/random` package.


## API

@docs generator


### Configuring generators

@docs intRange, floatRange, listLength

-}

import Gadget.IR as IR
import Random
import Random.Char
import Random.Extra
import Random.Float
import Random.Int
import Random.String


tools : IR.MetadataTools a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Random"


type alias IRValue =
    IR.IR IR.Value


type alias IRType =
    IR.IR IR.Type


{-| Limit the output of a Gadget's `Random.Generator` by setting minimum and maximum
values for the `Int` that it generates.

    import Gadget
    import Gadget.Adapter.Random
    import Random -- `elm/random`

    intGadget =
        Gadget.int
            |> Gadget.Adapter.Random.intRange 5 10

    intGenerator =
        Gadget.Adapter.Random.generator intGadget

    randomInt =
        Random.step
            intGenerator
            (Random.initialSeed 0)
            |> Tuple.first

    randomInt --> 6

-}
intRange : Int -> Int -> IR.Gadget a -> IR.Gadget a
intRange lo hi g =
    g
        |> tools.attach "int_lo" (IR.IntValue lo)
        |> tools.attach "int_hi" (IR.IntValue hi)


{-| Limit the output of a Gadget's `Random.Generator` by setting minimum and maximum
values for the `Float` that it generates.

    import Gadget
    import Gadget.Adapter.Random

    floatGadget =
        Gadget.float
            |> Gadget.Adapter.Random.floatRange 0.0 1.0

-}
floatRange : Float -> Float -> IR.Gadget a -> IR.Gadget a
floatRange lo hi g =
    g
        |> tools.attach "float_lo" (IR.FloatValue lo)
        |> tools.attach "float_hi" (IR.FloatValue hi)


{-| Limit the output of a Gadget's `Random.Generator` by setting minimum and maximum
values for the length of the `List` that it generates.

    import Gadget
    import Gadget.Adapter.Random
    import Random -- `elm/random`

    listGadget =
        Gadget.list Gadget.bool
            |> Gadget.Adapter.Random.listLength 2 4

    listGenerator =
        Gadget.Adapter.Random.generator listGadget

    randomList =
        Random.step
            listGenerator
            (Random.initialSeed 0)
            |> Tuple.first

    randomList --> [ True, False, False ]

-}
listLength : Int -> Int -> IR.Gadget a -> IR.Gadget a
listLength lo hi g =
    g
        |> tools.attach "list_lo" (IR.IntValue lo)
        |> tools.attach "list_hi" (IR.IntValue hi)


{-| Turn a Gadget into a `Random.Generator`.

    import Gadget
    import Gadget.Adapter.Random
    import Random -- `elm/random`

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field "name" .name Gadget.string
            |> Gadget.field "age" .age Gadget.int
            |> Gadget.endRecord

    personGenerator =
        Gadget.Adapter.Random.generator personGadget

    randomPerson =
        Random.step
            personGenerator
            (Random.initialSeed 2)
            |> Tuple.first

    randomPerson --> { age = -1353461051, name = "" }

-}
generator : IR.Gadget a -> Random.Generator a
generator gadget =
    generatorWithOverridesHelp gadget
        |> Random.andThen
            (\res ->
                case res of
                    Ok b ->
                        Random.constant b

                    Err _ ->
                        -- let's hope this never happens...
                        generator gadget
            )


generatorWithOverridesHelp : IR.Gadget a -> Random.Generator (Result IR.Error a)
generatorWithOverridesHelp gadget =
    IR.irType gadget
        |> randomAdapter
        |> Random.map (IR.toOutput gadget)


randomAdapter : IRType -> Random.Generator IRValue
randomAdapter (IR.IR metadata irType) =
    case irType of
        IR.LazyType constructType ->
            randomAdapter (constructType ())

        IR.BoolType ->
            Random.uniform False [ True ]
                |> Random.map IR.BoolValue
                |> Random.map (IR.IR metadata)

        IR.CharType ->
            Random.Char.basicLatin
                |> Random.map IR.CharValue
                |> Random.map (IR.IR metadata)

        IR.StringType ->
            Random.String.rangeLengthString 0 10 Random.Char.basicLatin
                |> Random.map IR.StringValue
                |> Random.map (IR.IR metadata)

        IR.IntType ->
            Random.map (IR.IR metadata) <|
                Random.map IR.IntValue <|
                    case ( tools.get "int_lo" metadata, tools.get "int_hi" metadata ) of
                        ( Just (IR.IntValue lo), Just (IR.IntValue hi) ) ->
                            Random.int lo hi

                        ( Just (IR.IntValue lo), _ ) ->
                            Random.int lo Random.maxInt

                        ( _, Just (IR.IntValue hi) ) ->
                            Random.int Random.maxInt hi

                        _ ->
                            Random.Int.anyInt

        IR.FloatType ->
            Random.map (IR.IR metadata) <|
                Random.map IR.FloatValue <|
                    case ( tools.get "float_lo" metadata, tools.get "float_hi" metadata ) of
                        ( Just (IR.FloatValue lo), Just (IR.FloatValue hi) ) ->
                            Random.float lo hi

                        ( Just (IR.FloatValue lo), _ ) ->
                            Random.float lo (toFloat Random.maxInt)

                        ( _, Just (IR.FloatValue hi) ) ->
                            Random.float (toFloat Random.maxInt) hi

                        _ ->
                            Random.Float.anyFloat

        IR.CustomType firstVariant restVariants ->
            let
                variantTypeToGenerator idx ( name, variant ) =
                    case variant of
                        IR.Variant0Type ->
                            Random.constant
                                (IR.CustomValue idx ( name, IR.Variant0Value ))

                        IR.Variant1Type arg ->
                            Random.map
                                (\a -> IR.CustomValue idx ( name, IR.Variant1Value a ))
                                (randomAdapter arg)

                        IR.Variant2Type arg1 arg2 ->
                            Random.map2
                                (\a1 a2 -> IR.CustomValue idx ( name, IR.Variant2Value a1 a2 ))
                                (randomAdapter arg1)
                                (randomAdapter arg2)

                        IR.Variant3Type arg1 arg2 arg3 ->
                            Random.map3
                                (\a1 a2 a3 -> IR.CustomValue idx ( name, IR.Variant3Value a1 a2 a3 ))
                                (randomAdapter arg1)
                                (randomAdapter arg2)
                                (randomAdapter arg3)

                        IR.Variant4Type arg1 arg2 arg3 arg4 ->
                            Random.map4
                                (\a1 a2 a3 a4 -> IR.CustomValue idx ( name, IR.Variant4Value a1 a2 a3 a4 ))
                                (randomAdapter arg1)
                                (randomAdapter arg2)
                                (randomAdapter arg3)
                                (randomAdapter arg4)

                        IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                            Random.map5
                                (\a1 a2 a3 a4 a5 -> IR.CustomValue idx ( name, IR.Variant5Value a1 a2 a3 a4 a5 ))
                                (randomAdapter arg1)
                                (randomAdapter arg2)
                                (randomAdapter arg3)
                                (randomAdapter arg4)
                                (randomAdapter arg5)
            in
            Random.Extra.choices
                (variantTypeToGenerator 0 firstVariant)
                (List.indexedMap (\idx v -> variantTypeToGenerator (idx + 1) v) restVariants)
                |> Random.map (IR.IR metadata)

        IR.ProductType fields ->
            fields
                |> Random.Extra.traverse
                    (\( name, fld ) ->
                        randomAdapter fld
                            |> Random.map (Tuple.pair name)
                    )
                |> Random.map IR.ProductValue
                |> Random.map (IR.IR metadata)

        IR.ListType itemType ->
            let
                min =
                    case tools.get "list_lo" metadata of
                        Just (IR.IntValue lo) ->
                            lo

                        _ ->
                            0

                max =
                    case tools.get "list_hi" metadata of
                        Just (IR.IntValue lo) ->
                            lo

                        _ ->
                            10
            in
            Random.int min max
                |> Random.andThen (\int -> Random.list int (randomAdapter itemType))
                |> Random.map IR.ListValue
                |> Random.map (IR.IR metadata)

        IR.TupleType aType bType ->
            Random.map2 (\a b -> IR.IR metadata (IR.TupleValue a b))
                (randomAdapter aType)
                (randomAdapter bType)

        IR.TripleType aType bType cType ->
            Random.map3 (\a b c -> IR.IR metadata (IR.TripleValue a b c))
                (randomAdapter aType)
                (randomAdapter bType)
                (randomAdapter cType)
