module Gadget.Adapter.Fuzz exposing
    ( fuzzer
    , fuzzerWithOverrides, Override, label, override
    )

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to create a `Fuzz.Fuzzer` for use with functions from the
`elm-explorations/test` package.


## API

@docs fuzzer

@docs fuzzerWithOverrides, Override, label, override

-}

import Dict
import Fuzz
import Gadget.IR as IR


type alias IRValue =
    IR.IR IR.Value


type alias IRType =
    IR.IR IR.Type


tools : IR.MetadataTools a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Fuzz"


{-| Turn a Gadget into a `Fuzz.Fuzzer`.

    import Gadget
    import Gadget.Adapter.Fuzzer
    import Fuzz -- `elm-explorations/test`

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field "name" .name Gadget.string
            |> Gadget.field "age" .age Gadget.int
            |> Gadget.endRecord

    personFuzzer =
        Gadget.Adapter.Fuzz.fuzzer personGadget

    fuzzedPerson =
        Fuzz.examples 1 personFuzzer

    fuzzedPerson --> [ { age = 92, name = "o \n\\" } ]

-}
fuzzer : IR.Gadget a -> Fuzz.Fuzzer a
fuzzer gadget =
    fuzzerWithOverrides [] gadget


{-| Turn a Gadget into a `Fuzz.Fuzzer`, but override some of the default
implementations of fuzzers that are defined by this module.

    import Gadget
    import Gadget.Adapter.Fuzzer
    import Fuzz -- `elm-explorations/test`

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field "name" .name nameGadget
            |> Gadget.field "age" .age Gadget.int
            |> Gadget.endRecord

    nameGadget =
        Gadget.string
            |> Gadget.Adapter.Fuzz.label "name"

    personFuzzer =
        Gadget.Adapter.Fuzz.fuzzerWithOverrides
            [ Gadget.Adapter.Fuzz.override
                "name"
                Gadget.string
                (Fuzz.constant "Ed")
            ]
            personGadget

    fuzzedPerson =
        Fuzz.examples 1 personFuzzer

    fuzzedPerson --> [ { age = 105, name = "Ed" } ]

-}
fuzzerWithOverrides : List Override -> IR.Gadget a -> Fuzz.Fuzzer a
fuzzerWithOverrides overrides gadget =
    let
        overridesDict =
            overrides
                |> List.map (\(Override label_ overrideFuzzer) -> ( label_, overrideFuzzer ))
                |> Dict.fromList
    in
    IR.irType gadget
        |> fuzzAdapter overridesDict
        |> Fuzz.andThen
            (\fuzzedIR ->
                case IR.toOutput gadget fuzzedIR of
                    Ok fuzzedOutput ->
                        Fuzz.constant fuzzedOutput

                    Err e ->
                        Fuzz.invalid e
            )


{-| A type used to represent overrides.
-}
type Override
    = Override String (Fuzz.Fuzzer IRValue)


{-| Add a label to a `Gadget` so that it can be overridden.
-}
label : String -> IR.Gadget a -> IR.Gadget a
label l =
    tools.attach l (IR.StringValue "")


{-| Override the default implementation of a `Fuzz.Fuzzer`.
-}
override : String -> IR.Gadget a -> Fuzz.Fuzzer a -> Override
override label_ gadget inputFuzzer =
    Override label_ (Fuzz.map (IR.fromInput gadget) inputFuzzer)


fuzzAdapter : Dict.Dict String (Fuzz.Fuzzer IRValue) -> IRType -> Fuzz.Fuzzer IRValue
fuzzAdapter overrides (IR.IR metadata irType) =
    case
        overrides
            |> Dict.foldl
                (\key thisOverride maybePrevOverride ->
                    case maybePrevOverride of
                        Just prevOverride ->
                            Just prevOverride

                        Nothing ->
                            if tools.member key metadata then
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
                IR.LazyType construct ->
                    Fuzz.lazy (\() -> fuzzAdapter overrides (construct ()))

                IR.BoolType ->
                    Fuzz.bool |> Fuzz.map (IR.BoolValue >> IR.IR metadata)

                IR.CharType ->
                    Fuzz.char |> Fuzz.map (IR.CharValue >> IR.IR metadata)

                IR.StringType ->
                    Fuzz.string |> Fuzz.map (IR.StringValue >> IR.IR metadata)

                IR.IntType ->
                    Fuzz.int |> Fuzz.map (IR.IntValue >> IR.IR metadata)

                IR.FloatType ->
                    Fuzz.niceFloat |> Fuzz.map (IR.FloatValue >> IR.IR metadata)

                IR.CustomType firstVariant restVariants ->
                    Fuzz.map (IR.IR metadata) <|
                        Fuzz.oneOf
                            (List.indexedMap
                                (\idx ( name, variant ) ->
                                    case variant of
                                        IR.Variant0Type ->
                                            Fuzz.constant
                                                (IR.CustomValue idx ( name, IR.Variant0Value ))

                                        IR.Variant1Type arg ->
                                            Fuzz.map
                                                (\a -> IR.CustomValue idx ( name, IR.Variant1Value a ))
                                                (fuzzAdapter overrides arg)

                                        IR.Variant2Type arg1 arg2 ->
                                            Fuzz.map2
                                                (\a1 a2 -> IR.CustomValue idx ( name, IR.Variant2Value a1 a2 ))
                                                (fuzzAdapter overrides arg1)
                                                (fuzzAdapter overrides arg2)

                                        IR.Variant3Type arg1 arg2 arg3 ->
                                            Fuzz.map3
                                                (\a1 a2 a3 -> IR.CustomValue idx ( name, IR.Variant3Value a1 a2 a3 ))
                                                (fuzzAdapter overrides arg1)
                                                (fuzzAdapter overrides arg2)
                                                (fuzzAdapter overrides arg3)

                                        IR.Variant4Type arg1 arg2 arg3 arg4 ->
                                            Fuzz.map4
                                                (\a1 a2 a3 a4 -> IR.CustomValue idx ( name, IR.Variant4Value a1 a2 a3 a4 ))
                                                (fuzzAdapter overrides arg1)
                                                (fuzzAdapter overrides arg2)
                                                (fuzzAdapter overrides arg3)
                                                (fuzzAdapter overrides arg4)

                                        IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                            Fuzz.map5
                                                (\a1 a2 a3 a4 a5 -> IR.CustomValue idx ( name, IR.Variant5Value a1 a2 a3 a4 a5 ))
                                                (fuzzAdapter overrides arg1)
                                                (fuzzAdapter overrides arg2)
                                                (fuzzAdapter overrides arg3)
                                                (fuzzAdapter overrides arg4)
                                                (fuzzAdapter overrides arg5)
                                )
                                (firstVariant :: restVariants)
                            )

                IR.ProductType fields ->
                    fields
                        |> Fuzz.traverse
                            (\( name, field ) ->
                                fuzzAdapter overrides field
                                    |> Fuzz.map (Tuple.pair name)
                            )
                        |> Fuzz.map IR.ProductValue
                        |> Fuzz.map (IR.IR metadata)

                IR.ListType itemType ->
                    Fuzz.list (fuzzAdapter overrides itemType)
                        |> Fuzz.map IR.ListValue
                        |> Fuzz.map (IR.IR metadata)
