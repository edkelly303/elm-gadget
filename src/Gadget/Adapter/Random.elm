module Gadget.Adapter.Random exposing (generator, generatorWithOverrides, Override, override)

{-| Use a Gadget to create a `Random.Generator` for use with functions from the
`elm/random` package.

@docs generator, generatorWithOverrides, Override, override

-}

import Dict
import Gadget.IR as IR
import Random
import Random.Char
import Random.Extra
import Random.Float
import Random.Int
import Random.String
import Set


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
generator codec =
    generatorWithOverrides [] codec


{-| A type used to represent overrides.
-}
type Override
    = Override String (Random.Generator IR.IR)


{-| Override the default implementation of a `Random.Generator`.
-}
override : String -> IR.Gadget a -> Random.Generator a -> Override
override label codec inputGenerator =
    Override label (Random.map (IR.fromInput codec) inputGenerator)


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
            |> Gadget.label "name"

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
generatorWithOverrides overrides codec =
    let
        overridesDict =
            overrides
                |> List.map (\(Override label generator_) -> ( label, generator_ ))
                |> Dict.fromList
    in
    generatorWithOverridesHelp overridesDict codec
        |> Random.andThen
            (\res ->
                case res of
                    Ok b ->
                        Random.constant b

                    Err _ ->
                        -- let's hope this never happens...
                        generatorWithOverrides overrides codec
            )


generatorWithOverridesHelp : Dict.Dict String (Random.Generator IR.IR) -> IR.Gadget a -> Random.Generator (Result IR.Error a)
generatorWithOverridesHelp overridesDict codec =
    IR.irType codec
        |> randomAdapter overridesDict
        |> Random.map (IR.toOutput codec)


randomAdapter : Dict.Dict String (Random.Generator IR.IR) -> IR.IRType -> Random.Generator IR.IR
randomAdapter overrides irType =
    case irType of
        IR.LazyType constructType ->
            randomAdapter overrides (constructType ())

        IR.LabelledType labels innerType ->
            Set.foldl
                (\label maybe ->
                    case maybe of
                        Nothing ->
                            Dict.get label overrides

                        Just override_ ->
                            Just override_
                )
                Nothing
                labels
                |> Maybe.withDefault (randomAdapter overrides innerType)
                |> Random.map (IR.Labelled labels)

        IR.BoolType ->
            Random.uniform False [ True ] |> Random.map IR.Bool

        IR.CharType ->
            Random.Char.basicLatin |> Random.map IR.Char

        IR.StringType ->
            Random.String.rangeLengthString 0 10 Random.Char.basicLatin |> Random.map IR.String

        IR.IntType ->
            Random.Int.anyInt |> Random.map IR.Int

        IR.FloatType ->
            Random.Float.anyFloat |> Random.map IR.Float

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

        IR.ProductType fields ->
            fields
                |> Random.Extra.traverse (randomAdapter overrides)
                |> Random.map IR.Product

        IR.ListType itemType ->
            Random.int 0 10
                |> Random.andThen (\int -> Random.list int (randomAdapter overrides itemType))
                |> Random.map IR.List
