module Gadget.Adapter.Random exposing (generator, generatorWithOverrides, Override, label, override, intRange)

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

@docs generator, generatorWithOverrides, Override, label, override, intRange

-}

import Dict
import Gadget.IR as IR
import Random
import Random.Char
import Random.Extra
import Random.Float
import Random.Int
import Random.String


meta : IR.MetadataTools a
meta =
    IR.makeMetadataTools "random"


type alias IRValue =
    IR.IR IR.Value


type alias IRType =
    IR.IR IR.Type


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
            |> Gadget.field .name Gadget.string
            |> Gadget.field .age Gadget.int
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
    generatorWithOverrides [] gadget


{-| A type used to represent overrides.
-}
type Override
    = Override String (Random.Generator IRValue)


{-| Override the default implementation of a `Random.Generator`.
-}
override : String -> IR.Gadget a -> Random.Generator a -> Override
override label_ gadget inputGenerator =
    Override label_ (Random.map (IR.fromInput gadget) inputGenerator)


{-| Limit the output of a `Random.Generator Int` by setting minimum and maximum
values for the generator.
-}
intRange : Int -> Int -> IR.Gadget a -> IR.Gadget a
intRange lo hi g =
    g
        |> meta.attach "int_lo" (IR.Int lo)
        |> meta.attach "int_hi" (IR.Int hi)


{-| Add a label to a `Gadget` so that it can be overridden.
-}
label : String -> IR.Gadget a -> IR.Gadget a
label l =
    meta.attach l (IR.String "")


{-| Turn a Gadget into a `Random.Generator`, but override some of the default
implementations of generators that are defined by this module.

    import Gadget
    import Gadget.Adapter.Random
    import Random -- `elm/random`

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field .name nameGadget
            |> Gadget.field .age Gadget.int
            |> Gadget.endRecord

    nameGadget =
        Gadget.string
            |> Gadget.Adapter.Random.label "name"

    personGenerator =
        Gadget.Adapter.Random.generatorWithOverrides
            [ Gadget.Adapter.Random.override
                "name"
                Gadget.string
                (Random.constant "Ed")
            ]
            personGadget

    randomPerson =
        Random.step
            personGenerator
            (Random.initialSeed 2)
            |> Tuple.first

    randomPerson --> { age = -97690584, name = "Ed" }

-}
generatorWithOverrides : List Override -> IR.Gadget a -> Random.Generator a
generatorWithOverrides overrides gadget =
    let
        overridesDict =
            overrides
                |> List.map (\(Override key generator_) -> ( key, generator_ ))
                |> Dict.fromList
    in
    generatorWithOverridesHelp overridesDict gadget
        |> Random.andThen
            (\res ->
                case res of
                    Ok b ->
                        Random.constant b

                    Err _ ->
                        -- let's hope this never happens...
                        generatorWithOverrides overrides gadget
            )


generatorWithOverridesHelp : Dict.Dict String (Random.Generator IRValue) -> IR.Gadget a -> Random.Generator (Result IR.Error a)
generatorWithOverridesHelp overridesDict gadget =
    IR.irType gadget
        |> randomAdapter overridesDict
        |> Random.map (IR.toOutput gadget)


randomAdapter : Dict.Dict String (Random.Generator IRValue) -> IRType -> Random.Generator IRValue
randomAdapter overrides (IR.IR metadata irType) =
    case
        overrides
            |> Dict.foldl
                (\key thisOverride maybePrevOverride ->
                    case maybePrevOverride of
                        Just prevOverride ->
                            Just prevOverride

                        Nothing ->
                            if meta.member key metadata then
                                Just thisOverride

                            else
                                Nothing
                )
                Nothing
    of
        Just thisOverride ->
            thisOverride

        Nothing ->
            case irType of
                IR.LazyType constructType ->
                    randomAdapter overrides (constructType ())

                IR.BoolType ->
                    Random.uniform False [ True ]
                        |> Random.map IR.Bool
                        |> Random.map (IR.IR metadata)

                IR.CharType ->
                    Random.Char.basicLatin
                        |> Random.map IR.Char
                        |> Random.map (IR.IR metadata)

                IR.StringType ->
                    Random.String.rangeLengthString 0 10 Random.Char.basicLatin
                        |> Random.map IR.String
                        |> Random.map (IR.IR metadata)

                IR.IntType ->
                    Random.map (IR.IR metadata) <|
                        Random.map IR.Int <|
                            case ( meta.get "int_lo" metadata, meta.get "int_hi" metadata ) of
                                ( Just (IR.Int lo), Just (IR.Int hi) ) ->
                                    Random.int lo hi

                                ( Just (IR.Int lo), _ ) ->
                                    Random.int lo Random.maxInt

                                ( _, Just (IR.Int hi) ) ->
                                    Random.int Random.maxInt hi

                                _ ->
                                    Random.Int.anyInt

                IR.FloatType ->
                    Random.Float.anyFloat
                        |> Random.map IR.Float
                        |> Random.map (IR.IR metadata)

                IR.CustomType firstVariant restVariants ->
                    let
                        variantTypeToGenerator idx variant =
                            case variant of
                                IR.Variant0Type ->
                                    Random.constant
                                        (IR.Custom idx IR.Variant0)

                                IR.Variant1Type arg ->
                                    Random.map
                                        (\a -> IR.Custom idx (IR.Variant1 a))
                                        (randomAdapter overrides arg)

                                IR.Variant2Type arg1 arg2 ->
                                    Random.map2
                                        (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                        (randomAdapter overrides arg1)
                                        (randomAdapter overrides arg2)

                                IR.Variant3Type arg1 arg2 arg3 ->
                                    Random.map3
                                        (\a1 a2 a3 -> IR.Custom idx (IR.Variant3 a1 a2 a3))
                                        (randomAdapter overrides arg1)
                                        (randomAdapter overrides arg2)
                                        (randomAdapter overrides arg3)

                                IR.Variant4Type arg1 arg2 arg3 arg4 ->
                                    Random.map4
                                        (\a1 a2 a3 a4 -> IR.Custom idx (IR.Variant4 a1 a2 a3 a4))
                                        (randomAdapter overrides arg1)
                                        (randomAdapter overrides arg2)
                                        (randomAdapter overrides arg3)
                                        (randomAdapter overrides arg4)

                                IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                    Random.map5
                                        (\a1 a2 a3 a4 a5 -> IR.Custom idx (IR.Variant5 a1 a2 a3 a4 a5))
                                        (randomAdapter overrides arg1)
                                        (randomAdapter overrides arg2)
                                        (randomAdapter overrides arg3)
                                        (randomAdapter overrides arg4)
                                        (randomAdapter overrides arg5)
                    in
                    Random.Extra.choices
                        (variantTypeToGenerator 0 firstVariant)
                        (List.indexedMap (\idx v -> variantTypeToGenerator (idx + 1) v) restVariants)
                        |> Random.map (IR.IR metadata)

                IR.ProductType fields ->
                    fields
                        |> Random.Extra.traverse (randomAdapter overrides)
                        |> Random.map IR.Product
                        |> Random.map (IR.IR metadata)

                IR.ListType itemType ->
                    Random.int 0 10
                        |> Random.andThen (\int -> Random.list int (randomAdapter overrides itemType))
                        |> Random.map IR.List
                        |> Random.map (IR.IR metadata)
