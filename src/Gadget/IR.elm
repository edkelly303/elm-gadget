module Gadget.IR exposing
    ( Gadget(..)
    , fromInput, irType, toOutput, Error
    , Value(..), VariantValue(..), Type(..), VariantType(..)
    , Metadata, MetadataTools, makeMetadataTools, emptyMetadata
    )

{-| Tools for creating adapters for Gadgets.

To see some examples of how to use this module, look at the source code for the
various `Gadget.Adapter` modules in this package:

  - The simplest one is probably [`Gadget.Adapter.Html`](Gadget-Adapter-Html).
  - For bidirectional examples, try [`Gadget.Adapter.Json`](Gadget-Adapter-Json)
    or [`Gadget.Adapter.String`](Gadget-Adapter-String).
  - For an example of how to use [`Metadata`](#Metadata) to configure Gadgets,
    see [`Gadget.Adapter.Random`](Gadget-Adapter-Random).
  - To see how you can use [`Metadata`](#Metadata) to build an override
    system for Gadgets, look at [`Gadget.Adapter.Fuzz`](Gadget-Adapter-Fuzz).
  - [`Gadget.Adapter.Diff`](Gadget-Adapter-Diff) isn't a very good example of
    anything, but I guess it shows how you can use Gadgets to make something
    quite complex.

@docs Gadget

@docs fromInput, irType, toOutput, Error

@docs Value, VariantValue, Type, VariantType

@docs Metadata, MetadataTools, makeMetadataTools, emptyMetadata

-}

import Dict exposing (Dict)


{-| The core type of this package. Use the functions in this module together
with an appropriate Gadget to convert values to and from the `IR` type.
-}
type Gadget a
    = Gadget
        { fromInput : a -> Value
        , toOutput : Value -> Result Error a
        , irType : Type
        }


{-| An error that may be generated if `toOutput` fails.
-}
type alias Error =
    String


{-| When an Elm value is translated into IR, its structure and contents are
represented using variants of `Value`.
-}
type Value
    = UnitValue
    | BoolValue Bool
    | CharValue Char
    | StringValue String
    | IntValue Int
    | FloatValue Float
    | CustomValue Int ( String, VariantValue )
    | RecordValue (List ( String, Value ))
    | ListValue (List Value)
    | TupleValue Value Value
    | TripleValue Value Value Value


{-| A type used by the `Custom` constructor of the `Value` type.
-}
type VariantValue
    = Variant0Value
    | Variant1Value Value
    | Variant2Value Value Value
    | Variant3Value Value Value Value
    | Variant4Value Value Value Value Value
    | Variant5Value Value Value Value Value Value


{-| When translated into IR, any Elm value will have a "type" that is a variant of `Type`.
-}
type Type
    = UnitType Metadata
    | BoolType Metadata
    | CharType Metadata
    | StringType Metadata
    | IntType Metadata
    | FloatType Metadata
    | CustomType Metadata ( String, VariantType ) (List ( String, VariantType ))
    | RecordType Metadata (List ( String, Type ))
    | ListType Metadata Type
    | LazyType Metadata (() -> Type)
    | TupleType Metadata Type Type
    | TripleType Metadata Type Type Type


{-| A type used by the `Custom` constructor of the `Type` type.
-}
type VariantType
    = Variant0Type
    | Variant1Type Type
    | Variant2Type Type Type
    | Variant3Type Type Type Type
    | Variant4Type Type Type Type Type
    | Variant5Type Type Type Type Type Type


{-| Use an appropriate Gadget to convert an Elm value into an `Value`.
-}
fromInput : Gadget a -> a -> Value
fromInput (Gadget c) input =
    c.fromInput input


{-| Use an appropriate Gadget to extract the `Type` of an Elm value.
-}
irType : Gadget a -> Type
irType (Gadget c) =
    c.irType


{-| Use an appropriate Gadget to attempt to convert an `Value` into an Elm
value.
-}
toOutput : Gadget a -> Value -> Result Error a
toOutput (Gadget c) a =
    c.toOutput a


{-| A type used to carry metadata for `Value` or `Type` nodes.
-}
type Metadata
    = Metadata (Dict String (Dict String Value))


{-| TODO - can we avoid exposing this?
-}
emptyMetadata : Metadata
emptyMetadata =
    Metadata Dict.empty


{-| Tools for working with `Metadata` attached to the `Value` or `Type`
produced by a Gadget.

For example, here's how [`Gadget.Adapter.Random`](Gadget-Adapter-Random) defines
[`intRange`](Gadget-Adapter-Random#range):

    import Gadget
    import Gadget.IR

    tools =
        Gadget.IR.makeMetadataTools "MyAdapter"

    -- we can use `tools.attach` to put some values in a
    -- Gadget's metadata store:

    range : number -> number -> Gadget.IR.Gadget number -> Gadget.IR.Gadget number
    range lo hi gadget =
        tools.attach "range" (Gadget.tuple gadget gadget) ( lo, hi ) gadget

    -- and we can look up those values using the other tools:

    rangedIntGadget =
        Gadget.int
            |> range 0 10

    metadata =
        rangedIntGadget
            |> Gadget.IR.irType
            |> tools.extract

    irValue =
        tools.get "range" metadata

    irValue --> Just (Gadget.IR.TupleValue (Gadget.IR.IntValue 0) (Gadget.IR.IntValue 10))

    elmValue =
        tools.decode "range" (Gadget.tuple Gadget.int Gadget.int) metadata

    elmValue --> Just ( 0, 10 )

    allValues =
        tools.debug metadata

    allValues --: List (String, List (String, String))

-}
type alias MetadataTools meta a =
    { attach : String -> Gadget meta -> meta -> Gadget a -> Gadget a
    , extract : Type -> Metadata
    , decode : String -> Gadget meta -> Metadata -> Maybe meta
    , get : String -> Metadata -> Maybe Value
    , debug : Metadata -> List ( String, List ( String, String ) )
    }


{-| Make tools for working with `Metadata` for a specific adapter.

`Metadata` is effectively a key-value store that allows adapters to attach extra
information to the `Value` and `Type` nodes produced by Gadgets.

To avoid spooky action at a distance and weird bugs, it's important that each
adapter can only access the metadata values that it has inserted - it can't read
values inserted by other adapters, or write values that other adapters can see.

For this reason, each adapter should create its own individual instance of
`MetadataTools` by passing a unique ID to the `makeMetadataTools` function. All
metadata inserted into `Metadata` will then be namespaced under that unique ID,
avoiding any risk of adapters interfering with each other.

The unique ID can be any `String`, but as a convention, you could use the
package name and/or module name of the Elm file where the adapter is defined.
This might make it easier to debug if you have lots of different adapters.

    -- module MyAdapter

    import Gadget.IR

    tools =
        Gadget.IR.makeMetadataTools "MyAdapter"

    tools --: Gadget.IR.MetadataTools meta a

-}
makeMetadataTools : String -> MetadataTools meta a
makeMetadataTools adapterId =
    let
        insert key metaGadget metaValue type_ =
            mapMetadata
                (\(Metadata metadata) ->
                    Dict.update adapterId
                        (\maybe ->
                            case maybe of
                                Just adapterDict ->
                                    Just (Dict.insert key (fromInput metaGadget metaValue) adapterDict)

                                Nothing ->
                                    Just (Dict.singleton key (fromInput metaGadget metaValue))
                        )
                        metadata
                        |> Metadata
                )
                type_

        attach key metaGadget value (Gadget gadget) =
            Gadget
                { gadget
                    | irType =
                        insert key metaGadget value gadget.irType
                }

        get key (Metadata metadata) =
            metadata
                |> Dict.get adapterId
                |> Maybe.andThen (Dict.get key)

        decode key metaGadget metadata =
            get key metadata
                |> Maybe.andThen (toOutput metaGadget >> Result.toMaybe)

        debug (Metadata metadata) =
            Dict.map
                (\_ dict ->
                    dict
                        |> Dict.map (\_ value -> debugValue value)
                        |> Dict.toList
                )
                metadata
                |> Dict.toList

        debugValue value =
            case value of
                UnitValue ->
                    "()"

                BoolValue b ->
                    if b then
                        "True"

                    else
                        "False"

                CharValue c ->
                    "'" ++ String.fromChar c ++ "'"

                StringValue s ->
                    "\"" ++ escape s ++ "\""

                IntValue i ->
                    String.fromInt i

                FloatValue f ->
                    String.fromFloat f

                CustomValue _ ( name, variant ) ->
                    name ++ String.join " " (List.map debugValue (argsToList variant))

                RecordValue namedFields ->
                    "{ " ++ (List.map (\( name, field ) -> name ++ " = " ++ debugValue field) namedFields |> String.join ", ") ++ " }"

                ListValue items ->
                    "[ " ++ (List.map debugValue items |> String.join ", ") ++ " ]"

                TupleValue a b ->
                    "( " ++ debugValue a ++ ", " ++ debugValue b ++ " )"

                TripleValue a b c ->
                    "( " ++ debugValue a ++ ", " ++ debugValue b ++ debugValue c ++ " )"

        escape s =
            s
                |> String.replace "\"" "\\\""
                |> String.replace "\t" "\\t"
                |> String.replace "\u{000D}" "\\r"
                |> String.replace "\n" "\\n"

        argsToList variant =
            case variant of
                Variant0Value ->
                    []

                Variant1Value arg ->
                    [ arg ]

                Variant2Value arg1 arg2 ->
                    [ arg1
                    , arg2
                    ]

                Variant3Value arg1 arg2 arg3 ->
                    [ arg1
                    , arg2
                    , arg3
                    ]

                Variant4Value arg1 arg2 arg3 arg4 ->
                    [ arg1
                    , arg2
                    , arg3
                    , arg4
                    ]

                Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                    [ arg1
                    , arg2
                    , arg3
                    , arg4
                    , arg5
                    ]

        mapMetadata f type_ =
            case type_ of
                UnitType m ->
                    UnitType (f m)

                BoolType m ->
                    BoolType (f m)

                CharType m ->
                    CharType (f m)

                StringType m ->
                    StringType (f m)

                IntType m ->
                    IntType (f m)

                FloatType m ->
                    FloatType (f m)

                CustomType m fst rst ->
                    CustomType (f m) fst rst

                RecordType m flds ->
                    RecordType (f m) flds

                ListType m items ->
                    ListType (f m) items

                LazyType m inner ->
                    LazyType (f m) inner

                TupleType m a b ->
                    TupleType (f m) a b

                TripleType m a b c ->
                    TripleType (f m) a b c

        extract type_ =
            case type_ of
                UnitType m ->
                    m

                BoolType m ->
                    m

                CharType m ->
                    m

                StringType m ->
                    m

                IntType m ->
                    m

                FloatType m ->
                    m

                CustomType m _ _ ->
                    m

                RecordType m _ ->
                    m

                ListType m _ ->
                    m

                LazyType m _ ->
                    m

                TupleType m _ _ ->
                    m

                TripleType m _ _ _ ->
                    m
    in
    { attach = attach
    , decode = decode
    , extract = extract
    , get = get
    , debug = debug
    }
