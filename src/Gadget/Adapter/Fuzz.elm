module Gadget.Adapter.Fuzz exposing
    ( fuzzer
    , fuzzerWithOverrides, Override, override, useOverride
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

@docs fuzzerWithOverrides, Override, override, useOverride

-}

import Dict
import Fuzz
import Gadget
import Gadget.IR as IR exposing (Gadget, Type(..), Value(..), VariantType(..), VariantValue(..))


tools : IR.MetadataTools meta a
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
fuzzer : Gadget a -> Fuzz.Fuzzer a
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
            |> Gadget.Adapter.Fuzz.useOverride "name"

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
fuzzerWithOverrides : List Override -> Gadget a -> Fuzz.Fuzzer a
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

                    Err errors ->
                        errors
                            |> List.map .error
                            |> String.join "\n"
                            |> Fuzz.invalid
            )


{-| A type used to represent overrides.
-}
type Override
    = Override String (Fuzz.Fuzzer Value)


{-| Specify which override a `Gadget` should use.
-}
useOverride : String -> Gadget a -> Gadget a
useOverride label_ =
    tools.attach "useOverride" Gadget.string label_


{-| Override the default implementation of a `Fuzz.Fuzzer`.
-}
override : String -> Gadget a -> Fuzz.Fuzzer a -> Override
override label_ gadget inputFuzzer =
    Override label_ (Fuzz.map (IR.fromInput gadget) inputFuzzer)


fuzzAdapter : Dict.Dict String (Fuzz.Fuzzer Value) -> Type -> Fuzz.Fuzzer Value
fuzzAdapter overrides irType =
    case
        overrides
            |> Dict.foldl
                (\key thisOverride maybePrevOverride ->
                    case maybePrevOverride of
                        Just prevOverride ->
                            Just prevOverride

                        Nothing ->
                            tools.extract irType
                                |> tools.decode "useOverride" Gadget.string
                                |> Maybe.andThen
                                    (\lbl ->
                                        if key == lbl then
                                            Just thisOverride

                                        else
                                            Nothing
                                    )
                )
                Nothing
    of
        Just thisOverride ->
            thisOverride

        Nothing ->
            case irType of
                LazyType _ construct ->
                    Fuzz.lazy (\() -> fuzzAdapter overrides (construct ()))

                UnitType _ ->
                    Fuzz.constant UnitValue

                BoolType _ ->
                    Fuzz.bool
                        |> Fuzz.map BoolValue

                CharType _ ->
                    Fuzz.char
                        |> Fuzz.map CharValue

                StringType _ ->
                    Fuzz.string
                        |> Fuzz.map StringValue

                IntType _ ->
                    Fuzz.int
                        |> Fuzz.map IntValue

                FloatType _ ->
                    Fuzz.niceFloat
                        |> Fuzz.map FloatValue

                CustomType _ firstVariant restVariants ->
                    Fuzz.oneOf
                        (List.indexedMap
                            (\idx ( name, variant ) ->
                                case variant of
                                    Variant0Type ->
                                        Fuzz.constant
                                            (CustomValue idx ( name, Variant0Value ))

                                    Variant1Type arg ->
                                        Fuzz.map
                                            (\a -> CustomValue idx ( name, Variant1Value a ))
                                            (fuzzAdapter overrides arg)

                                    Variant2Type arg1 arg2 ->
                                        Fuzz.map2
                                            (\a1 a2 -> CustomValue idx ( name, Variant2Value a1 a2 ))
                                            (fuzzAdapter overrides arg1)
                                            (fuzzAdapter overrides arg2)

                                    Variant3Type arg1 arg2 arg3 ->
                                        Fuzz.map3
                                            (\a1 a2 a3 -> CustomValue idx ( name, Variant3Value a1 a2 a3 ))
                                            (fuzzAdapter overrides arg1)
                                            (fuzzAdapter overrides arg2)
                                            (fuzzAdapter overrides arg3)

                                    Variant4Type arg1 arg2 arg3 arg4 ->
                                        Fuzz.map4
                                            (\a1 a2 a3 a4 -> CustomValue idx ( name, Variant4Value a1 a2 a3 a4 ))
                                            (fuzzAdapter overrides arg1)
                                            (fuzzAdapter overrides arg2)
                                            (fuzzAdapter overrides arg3)
                                            (fuzzAdapter overrides arg4)

                                    Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                        Fuzz.map5
                                            (\a1 a2 a3 a4 a5 -> CustomValue idx ( name, Variant5Value a1 a2 a3 a4 a5 ))
                                            (fuzzAdapter overrides arg1)
                                            (fuzzAdapter overrides arg2)
                                            (fuzzAdapter overrides arg3)
                                            (fuzzAdapter overrides arg4)
                                            (fuzzAdapter overrides arg5)
                            )
                            (firstVariant :: restVariants)
                        )

                RecordType _ fields ->
                    fields
                        |> Fuzz.traverse
                            (\( name, field ) ->
                                fuzzAdapter overrides field
                                    |> Fuzz.map (Tuple.pair name)
                            )
                        |> Fuzz.map RecordValue

                ListType _ itemType ->
                    Fuzz.list (fuzzAdapter overrides itemType)
                        |> Fuzz.map ListValue

                TupleType _ aType bType ->
                    Fuzz.map2 (\a b -> TupleValue a b)
                        (fuzzAdapter overrides aType)
                        (fuzzAdapter overrides bType)

                TripleType _ aType bType cType ->
                    Fuzz.map3 (\a b c -> TripleValue a b c)
                        (fuzzAdapter overrides aType)
                        (fuzzAdapter overrides bType)
                        (fuzzAdapter overrides cType)
