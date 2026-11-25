module Gadget.Adapter.Fuzz exposing
    ( fuzzer
    , fuzzerWithOverrides, Override, override, useOverride, isPrimaryKey, hasForeignKey
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

@docs fuzzerWithOverrides, Override, override, useOverride, isPrimaryKey, hasForeignKey

-}

import Dict
import Fuzz
import Gadget
import Gadget.IR as IR exposing (Gadget, Type(..), Value(..), VariantType(..), VariantValue(..))
import List.Extra
import Maybe.Extra


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

        irType =
            IR.irType gadget
    in
    makePrimaryKeysFuzzer overridesDict irType
        |> Fuzz.andThen
            (\primaryKeys ->
                fuzzAdapter overridesDict primaryKeys irType
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


type alias PrimaryKeysFuzzer =
    Fuzz.Fuzzer PrimaryKeys


type alias PrimaryKeys =
    Dict.Dict String (List Value)


type alias OverridesDict =
    Dict.Dict String (Fuzz.Fuzzer Value)


fuzzAdapter : OverridesDict -> PrimaryKeys -> Type -> Fuzz.Fuzzer Value
fuzzAdapter overrides primaryKeys irType =
    let
        metadata =
            tools.extract irType
    in
    case tools.decode "foreignKey" Gadget.string metadata of
        Just key ->
            Dict.get key primaryKeys
                |> Maybe.map Fuzz.oneOfValues
                |> Maybe.withDefault (Fuzz.invalid ("primary key `" ++ key ++ "` not found"))

        Nothing ->
            case
                overrides
                    |> Dict.foldl
                        (\key thisOverride maybePrevOverride ->
                            case maybePrevOverride of
                                Just prevOverride ->
                                    Just prevOverride

                                Nothing ->
                                    metadata
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
                            Fuzz.lazy (\() -> fuzzAdapter overrides primaryKeys (construct ()))

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
                                                    (fuzzAdapter overrides primaryKeys arg)

                                            Variant2Type arg1 arg2 ->
                                                Fuzz.map2
                                                    (\a1 a2 -> CustomValue idx ( name, Variant2Value a1 a2 ))
                                                    (fuzzAdapter overrides primaryKeys arg1)
                                                    (fuzzAdapter overrides primaryKeys arg2)

                                            Variant3Type arg1 arg2 arg3 ->
                                                Fuzz.map3
                                                    (\a1 a2 a3 -> CustomValue idx ( name, Variant3Value a1 a2 a3 ))
                                                    (fuzzAdapter overrides primaryKeys arg1)
                                                    (fuzzAdapter overrides primaryKeys arg2)
                                                    (fuzzAdapter overrides primaryKeys arg3)

                                            Variant4Type arg1 arg2 arg3 arg4 ->
                                                Fuzz.map4
                                                    (\a1 a2 a3 a4 -> CustomValue idx ( name, Variant4Value a1 a2 a3 a4 ))
                                                    (fuzzAdapter overrides primaryKeys arg1)
                                                    (fuzzAdapter overrides primaryKeys arg2)
                                                    (fuzzAdapter overrides primaryKeys arg3)
                                                    (fuzzAdapter overrides primaryKeys arg4)

                                            Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                                Fuzz.map5
                                                    (\a1 a2 a3 a4 a5 -> CustomValue idx ( name, Variant5Value a1 a2 a3 a4 a5 ))
                                                    (fuzzAdapter overrides primaryKeys arg1)
                                                    (fuzzAdapter overrides primaryKeys arg2)
                                                    (fuzzAdapter overrides primaryKeys arg3)
                                                    (fuzzAdapter overrides primaryKeys arg4)
                                                    (fuzzAdapter overrides primaryKeys arg5)
                                    )
                                    (firstVariant :: restVariants)
                                )

                        RecordType _ fields ->
                            fields
                                |> Fuzz.traverse
                                    (\( name, field ) ->
                                        fuzzAdapter overrides primaryKeys field
                                            |> Fuzz.map (Tuple.pair name)
                                    )
                                |> Fuzz.map RecordValue

                        ListType _ itemType ->
                            case getPrimaryKey itemType of
                                Nothing ->
                                    Fuzz.list (fuzzAdapter overrides primaryKeys itemType)
                                        |> Fuzz.map ListValue

                                Just key ->
                                    case Dict.get key primaryKeys of
                                        Just vals ->
                                            Fuzz.traverse (\val -> insertValue overrides primaryKeys key (Fuzz.constant val) itemType) vals
                                                |> Fuzz.map ListValue

                                        Nothing ->
                                            Fuzz.invalid "primary key not found"

                        PairType _ aType bType ->
                            Fuzz.map2 (\a b -> PairValue a b)
                                (fuzzAdapter overrides primaryKeys aType)
                                (fuzzAdapter overrides primaryKeys bType)

                        TripleType _ aType bType cType ->
                            Fuzz.map3 (\a b c -> TripleValue a b c)
                                (fuzzAdapter overrides primaryKeys aType)
                                (fuzzAdapter overrides primaryKeys bType)
                                (fuzzAdapter overrides primaryKeys cType)


insertValue overrides primaryKeys pKey valueFuzzer irType =
    let
        continue =
            case irType of
                LazyType _ construct ->
                    Fuzz.lazy (\() -> insertValue overrides primaryKeys pKey valueFuzzer (construct ()))

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
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg)

                                    Variant2Type arg1 arg2 ->
                                        Fuzz.map2
                                            (\a1 a2 -> CustomValue idx ( name, Variant2Value a1 a2 ))
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg1)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg2)

                                    Variant3Type arg1 arg2 arg3 ->
                                        Fuzz.map3
                                            (\a1 a2 a3 -> CustomValue idx ( name, Variant3Value a1 a2 a3 ))
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg1)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg2)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg3)

                                    Variant4Type arg1 arg2 arg3 arg4 ->
                                        Fuzz.map4
                                            (\a1 a2 a3 a4 -> CustomValue idx ( name, Variant4Value a1 a2 a3 a4 ))
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg1)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg2)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg3)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg4)

                                    Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                        Fuzz.map5
                                            (\a1 a2 a3 a4 a5 -> CustomValue idx ( name, Variant5Value a1 a2 a3 a4 a5 ))
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg1)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg2)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg3)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg4)
                                            (insertValue overrides primaryKeys pKey valueFuzzer arg5)
                            )
                            (firstVariant :: restVariants)
                        )

                RecordType _ fields ->
                    fields
                        |> Fuzz.traverse
                            (\( name, field ) ->
                                insertValue overrides primaryKeys pKey valueFuzzer field
                                    |> Fuzz.map (Tuple.pair name)
                            )
                        |> Fuzz.map RecordValue

                ListType _ itemType ->
                    case getPrimaryKey itemType of
                        Nothing ->
                            Fuzz.list (insertValue overrides primaryKeys pKey valueFuzzer itemType)
                                |> Fuzz.map ListValue

                        Just key ->
                            case Dict.get key primaryKeys of
                                Just vals ->
                                    Fuzz.traverse (\val -> insertValue overrides primaryKeys key (Fuzz.constant val) itemType) vals
                                        |> Fuzz.map ListValue

                                Nothing ->
                                    Fuzz.invalid "primary key not found"

                PairType _ aType bType ->
                    Fuzz.map2 (\a b -> PairValue a b)
                        (insertValue overrides primaryKeys pKey valueFuzzer aType)
                        (insertValue overrides primaryKeys pKey valueFuzzer bType)

                TripleType _ aType bType cType ->
                    Fuzz.map3 (\a b c -> TripleValue a b c)
                        (insertValue overrides primaryKeys pKey valueFuzzer aType)
                        (insertValue overrides primaryKeys pKey valueFuzzer bType)
                        (insertValue overrides primaryKeys pKey valueFuzzer cType)

                _ ->
                    fuzzAdapter overrides primaryKeys irType
    in
    case tools.extract irType |> tools.decode "primaryKey" Gadget.string of
        Just thisPKey ->
            if thisPKey == pKey then
                valueFuzzer

            else
                continue

        Nothing ->
            continue


getPrimaryKey : Type -> Maybe String
getPrimaryKey irType =
    case tools.extract irType |> tools.decode "primaryKey" Gadget.string of
        Nothing ->
            case irType of
                CustomType _ firstNamedVariantType restNamedVariantTypes ->
                    (firstNamedVariantType :: restNamedVariantTypes)
                        |> List.concatMap
                            (\( _, variantType ) ->
                                variantType
                                    |> variantTypeToList
                                    |> List.map getPrimaryKey
                            )
                        |> Maybe.Extra.orList

                RecordType _ namedFieldTypes ->
                    namedFieldTypes
                        |> List.map (\( _, fieldType ) -> getPrimaryKey fieldType)
                        |> Maybe.Extra.orList

                LazyType _ thunk ->
                    getPrimaryKey (thunk ())

                PairType _ aType bType ->
                    getPrimaryKey aType
                        |> Maybe.Extra.or (getPrimaryKey bType)

                TripleType _ aType bType cType ->
                    getPrimaryKey aType
                        |> Maybe.Extra.or (getPrimaryKey bType)
                        |> Maybe.Extra.or (getPrimaryKey cType)

                ListType _ _ ->
                    -- I think we don't want to look deeper for primary keys
                    -- that are nested in lists of lists...
                    Nothing

                _ ->
                    Nothing

        Just key ->
            Just key


isPrimaryKey : String -> Gadget a -> Gadget a
isPrimaryKey key gadget =
    tools.attach "primaryKey" Gadget.string key gadget


hasForeignKey : String -> Gadget a -> Gadget a
hasForeignKey key gadget =
    tools.attach "foreignKey" Gadget.string key gadget


makePrimaryKeysFuzzer :
    OverridesDict
    -> Type
    -> PrimaryKeysFuzzer
makePrimaryKeysFuzzer overrides irType =
    makePrimaryKeysFuzzerHelp overrides irType (Fuzz.constant Dict.empty)


makePrimaryKeysFuzzerHelp :
    Dict.Dict String (Fuzz.Fuzzer Value)
    -> Type
    -> PrimaryKeysFuzzer
    -> PrimaryKeysFuzzer
makePrimaryKeysFuzzerHelp overrides irType oldPrimaryKeysFuzzer =
    let
        primaryKeysFuzzer =
            case tools.extract irType |> tools.decode "primaryKey" Gadget.string of
                Nothing ->
                    oldPrimaryKeysFuzzer

                Just key ->
                    Fuzz.map2
                        (Dict.insert key)
                        (oldPrimaryKeysFuzzer
                            |> Fuzz.andThen (\oldPrimaryKeys -> Fuzz.list (fuzzAdapter overrides oldPrimaryKeys irType))
                            |> Fuzz.map List.Extra.unique
                        )
                        oldPrimaryKeysFuzzer
    in
    case irType of
        CustomType _ firstNamedVariant restNamedVariants ->
            List.foldl
                (\( _, variantType ) variantDict ->
                    List.foldl
                        (\argType argDict ->
                            makePrimaryKeysFuzzerHelp overrides argType argDict
                        )
                        variantDict
                        (variantTypeToList variantType)
                )
                primaryKeysFuzzer
                (firstNamedVariant :: restNamedVariants)

        RecordType _ namedFieldTypes ->
            List.foldl
                (\( _, fieldType ) fieldDict ->
                    makePrimaryKeysFuzzerHelp overrides fieldType fieldDict
                )
                primaryKeysFuzzer
                namedFieldTypes

        ListType _ itemType ->
            makePrimaryKeysFuzzerHelp overrides itemType primaryKeysFuzzer

        LazyType _ thunk ->
            makePrimaryKeysFuzzerHelp overrides (thunk ()) primaryKeysFuzzer

        PairType _ aType bType ->
            List.foldl
                (\elemType elemDict ->
                    makePrimaryKeysFuzzerHelp overrides elemType elemDict
                )
                primaryKeysFuzzer
                [ aType, bType ]

        TripleType _ aType bType cType ->
            List.foldl
                (\elemType elemDict ->
                    makePrimaryKeysFuzzerHelp overrides elemType elemDict
                )
                primaryKeysFuzzer
                [ aType, bType, cType ]

        _ ->
            primaryKeysFuzzer


variantTypeToList : VariantType -> List Type
variantTypeToList variantType =
    case variantType of
        Variant0Type ->
            []

        Variant1Type arg ->
            [ arg ]

        Variant2Type arg1 arg2 ->
            [ arg1
            , arg2
            ]

        Variant3Type arg1 arg2 arg3 ->
            [ arg1
            , arg2
            , arg3
            ]

        Variant4Type arg1 arg2 arg3 arg4 ->
            [ arg1
            , arg2
            , arg3
            , arg4
            ]

        Variant5Type arg1 arg2 arg3 arg4 arg5 ->
            [ arg1
            , arg2
            , arg3
            , arg4
            , arg5
            ]
