module Gadget.Adapter.Fuzz exposing
    ( fuzzer
    , fuzzerWithOverrides, Override, override
    )

{-| Use a Gadget to create a `Fuzz.Fuzzer` for use with functions from the
`elm-explorations/test` package.

@docs fuzzer

@docs fuzzerWithOverrides, Override, override

-}

import Dict
import Fuzz
import Gadget.IR as IR
import Set


{-| Turn a Gadget into a `Fuzz.Fuzzer`.

    import Gadget
    import Gadget.Adapter.Random
    import Fuzz -- `elm-explorations/test`

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field .name Gadget.string
            |> Gadget.field .age Gadget.int
            |> Gadget.endRecord

    personFuzzer =
        Gadget.Adapter.Fuzz.fuzzer personGadget

    fuzzedPerson =
        Fuzz.examples 1 personFuzzer

    fuzzedPerson --> [ { age = 92, name = "o \n\\" } ]

-}
fuzzer : IR.Gadget a -> Fuzz.Fuzzer a
fuzzer codec =
    fuzzerWithOverrides [] codec


{-| Turn a Gadget into a `Fuzz.Fuzzer`, but override some of the default
implementations of fuzzers that are defined by this module.

    import Gadget
    import Gadget.Adapter.Random
    import Fuzz -- `elm-explorations/test`

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
            |> Gadget.label "name"

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
fuzzerWithOverrides overrides codec =
    let
        overridesDict =
            overrides
                |> List.map (\(Override label overrideFuzzer) -> ( label, overrideFuzzer ))
                |> Dict.fromList
    in
    IR.irType codec
        |> fuzzAdapter overridesDict
        |> Fuzz.andThen
            (\fuzzedIR ->
                case IR.toOutput codec fuzzedIR of
                    Ok fuzzedOutput ->
                        Fuzz.constant fuzzedOutput

                    Err e ->
                        Fuzz.invalid e
            )


{-| A type used to represent overrides.
-}
type Override
    = Override String (Fuzz.Fuzzer IR.IR)


{-| Override the default implementation of a `Fuzz.Fuzzer`.
-}
override : String -> IR.Gadget a -> Fuzz.Fuzzer a -> Override
override label codec inputFuzzer =
    Override label (Fuzz.map (IR.fromInput codec) inputFuzzer)


fuzzAdapter : Dict.Dict String (Fuzz.Fuzzer IR.IR) -> IR.IRType -> Fuzz.Fuzzer IR.IR
fuzzAdapter overrides irType =
    case irType of
        IR.LazyType construct ->
            Fuzz.lazy (\() -> fuzzAdapter overrides (construct ()))

        IR.LabelledType labels innerType ->
            Set.foldl
                (\label maybe ->
                    case maybe of
                        Nothing ->
                            Dict.get label overrides

                        _ ->
                            maybe
                )
                Nothing
                labels
                |> Maybe.withDefault (fuzzAdapter overrides innerType)
                |> Fuzz.map (IR.Labelled labels)

        IR.BoolType ->
            Fuzz.bool |> Fuzz.map IR.Bool

        IR.CharType ->
            Fuzz.char |> Fuzz.map IR.Char

        IR.StringType ->
            Fuzz.string |> Fuzz.map IR.String

        IR.IntType ->
            Fuzz.int |> Fuzz.map IR.Int

        IR.FloatType ->
            Fuzz.niceFloat |> Fuzz.map IR.Float

        IR.CustomType firstVariant restVariants ->
            Fuzz.oneOf
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            IR.Variant0Type ->
                                Fuzz.constant
                                    (IR.Custom idx IR.Variant0)

                            IR.Variant1Type arg ->
                                Fuzz.map
                                    (\a -> IR.Custom idx (IR.Variant1 a))
                                    (fuzzAdapter overrides arg)

                            IR.Variant2Type arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)

                            IR.Variant3Type arg1 arg2 arg3 ->
                                Fuzz.map3
                                    (\a1 a2 a3 -> IR.Custom idx (IR.Variant3 a1 a2 a3))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)
                                    (fuzzAdapter overrides arg3)

                            IR.Variant4Type arg1 arg2 arg3 arg4 ->
                                Fuzz.map4
                                    (\a1 a2 a3 a4 -> IR.Custom idx (IR.Variant4 a1 a2 a3 a4))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)
                                    (fuzzAdapter overrides arg3)
                                    (fuzzAdapter overrides arg4)

                            IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                Fuzz.map5
                                    (\a1 a2 a3 a4 a5 -> IR.Custom idx (IR.Variant5 a1 a2 a3 a4 a5))
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
                |> Fuzz.traverse (fuzzAdapter overrides)
                |> Fuzz.map IR.Product

        IR.ListType itemType ->
            Fuzz.list (fuzzAdapter overrides itemType)
                |> Fuzz.map IR.List
