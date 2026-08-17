module Gadget.Adapter.Random exposing
    ( generator
    , range, listLength, choose
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

@docs range, listLength, choose

-}

import Gadget
import Gadget.IR as IR
import Random
import Random.Char
import Random.Extra
import Random.Float
import Random.Int
import Random.String


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Random"


{-| Limit the output of a Gadget's `Random.Generator` by setting minimum and maximum
values for the `Int` that it generates.

    import Gadget
    import Gadget.Adapter.Random
    import Random -- `elm/random`

    intGadget =
        Gadget.int
            |> Gadget.Adapter.Random.range 5 10

    intGenerator =
        Gadget.Adapter.Random.generator intGadget

    randomInt =
        Random.step
            intGenerator
            (Random.initialSeed 0)
            |> Tuple.first

    randomInt --> 6

-}
range : number -> number -> IR.Gadget number -> IR.Gadget number
range lo hi gadget =
    tools.attach "range" (Gadget.tuple gadget gadget) ( lo, hi ) gadget


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
listLength : Int -> Int -> IR.Gadget (List a) -> IR.Gadget (List a)
listLength lo hi gadget =
    tools.attach "listLength" (Gadget.tuple Gadget.int Gadget.int) ( lo, hi ) gadget


{-| Constrain a Gadget's `Random.Generator` to emit one of a set of options.
-}
choose : a -> List a -> IR.Gadget a -> IR.Gadget a
choose first rest gadget =
    tools.attach "choose" (Gadget.tuple gadget (Gadget.list gadget)) ( first, rest ) gadget


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
    IR.irType gadget
        |> randomAdapter
        |> Random.map (IR.toOutput gadget)
        |> Random.andThen
            (\res ->
                case res of
                    Ok b ->
                        Random.constant b

                    Err _ ->
                        -- let's hope this never happens...
                        generator gadget
            )


randomAdapter : IR.Type -> Random.Generator IR.Value
randomAdapter irType =
    case
        tools.extract irType
            |> tools.get "choose"
    of
        Just (IR.TupleValue choice (IR.ListValue choices)) ->
            Random.uniform choice choices

        _ ->
            case irType of
                IR.LazyType _ constructType ->
                    randomAdapter (constructType ())

                IR.UnitType _ ->
                    Random.constant IR.UnitValue

                IR.BoolType _ ->
                    Random.uniform False [ True ]
                        |> Random.map IR.BoolValue

                IR.CharType _ ->
                    Random.Char.basicLatin
                        |> Random.map IR.CharValue

                IR.StringType _ ->
                    Random.String.rangeLengthString 0 10 Random.Char.basicLatin
                        |> Random.map IR.StringValue

                IR.IntType metadata ->
                    Random.map IR.IntValue <|
                        case
                            tools.decode "range" (Gadget.tuple Gadget.int Gadget.int) metadata
                        of
                            Just ( lo, hi ) ->
                                Random.int lo hi

                            _ ->
                                Random.Int.anyInt

                IR.FloatType metadata ->
                    Random.map IR.FloatValue <|
                        case
                            tools.decode "range" (Gadget.tuple Gadget.float Gadget.float) metadata
                        of
                            Just ( lo, hi ) ->
                                Random.float lo hi

                            _ ->
                                Random.Float.anyFloat

                IR.CustomType _ firstVariant restVariants ->
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

                IR.RecordType _ fields ->
                    fields
                        |> Random.Extra.traverse
                            (\( name, fld ) ->
                                randomAdapter fld
                                    |> Random.map (Tuple.pair name)
                            )
                        |> Random.map IR.RecordValue

                IR.ListType metadata itemType ->
                    let
                        ( min, max ) =
                            tools.decode "listLength" (Gadget.tuple Gadget.int Gadget.int) metadata
                                |> Maybe.withDefault ( 0, 10 )
                    in
                    Random.int min max
                        |> Random.andThen (\int -> Random.list int (randomAdapter itemType))
                        |> Random.map IR.ListValue

                IR.TupleType _ aType bType ->
                    Random.map2 (\a b -> IR.TupleValue a b)
                        (randomAdapter aType)
                        (randomAdapter bType)

                IR.TripleType _ aType bType cType ->
                    Random.map3 (\a b c -> IR.TripleValue a b c)
                        (randomAdapter aType)
                        (randomAdapter bType)
                        (randomAdapter cType)
