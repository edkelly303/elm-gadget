module Gadget.IR exposing
    ( Gadget(..)
    , fromInput, irType, toOutput, Error
    , IR(..), ir, Value(..), VariantValue(..), Type(..), VariantType(..)
    , Metadata, MetadataTools, makeMetadataTools
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

@docs IR, ir, Value, VariantValue, Type, VariantType

@docs Metadata, MetadataTools, makeMetadataTools

-}

import Dict exposing (Dict)


{-| The core type of this package. Use the functions in this module together
with an appropriate Gadget to convert values to and from the `IR` type.
-}
type Gadget a
    = Gadget
        { fromInput : a -> IR Value
        , toOutput : IR Value -> Result Error a
        , irType : IR Type
        }


{-| An error that may be generated if `toOutput` fails.
-}
type alias Error =
    String


{-| A wrapper that attaches `Metadata` to each `Value` and `Type` node.
-}
type IR valueOrType
    = IR Metadata valueOrType


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
    | RecordValue (List ( String, IR Value ))
    | ListValue (List (IR Value))
    | TupleValue (IR Value) (IR Value)
    | TripleValue (IR Value) (IR Value) (IR Value)


{-| A type used by the `Custom` constructor of the `Value` type.
-}
type VariantValue
    = Variant0Value
    | Variant1Value (IR Value)
    | Variant2Value (IR Value) (IR Value)
    | Variant3Value (IR Value) (IR Value) (IR Value)
    | Variant4Value (IR Value) (IR Value) (IR Value) (IR Value)
    | Variant5Value (IR Value) (IR Value) (IR Value) (IR Value) (IR Value)


{-| When translated into IR, any Elm value will have a "type" that is a variant of `Type`.
-}
type Type
    = UnitType
    | BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType ( String, VariantType ) (List ( String, VariantType ))
    | RecordType (List ( String, IR Type ))
    | ListType (IR Type)
    | LazyType (() -> IR Type)
    | TupleType (IR Type) (IR Type)
    | TripleType (IR Type) (IR Type) (IR Type)


{-| A type used by the `Custom` constructor of the `Type` type.
-}
type VariantType
    = Variant0Type
    | Variant1Type (IR Type)
    | Variant2Type (IR Type) (IR Type)
    | Variant3Type (IR Type) (IR Type) (IR Type)
    | Variant4Type (IR Type) (IR Type) (IR Type) (IR Type)
    | Variant5Type (IR Type) (IR Type) (IR Type) (IR Type) (IR Type)


{-| Use an appropriate Gadget to convert an Elm value into an `IR Value`.
-}
fromInput : Gadget a -> a -> IR Value
fromInput (Gadget c) input =
    c.fromInput input


{-| Use an appropriate Gadget to extract the `IR Type` of an Elm value.
-}
irType : Gadget a -> IR Type
irType (Gadget c) =
    c.irType


{-| Use an appropriate Gadget to attempt to convert an `IR Value` into an Elm
value.
-}
toOutput : Gadget a -> IR Value -> Result Error a
toOutput (Gadget c) a =
    c.toOutput a


{-| A type used to carry metadata for `IR Value` or `IR Type` nodes.
-}
type Metadata
    = Metadata (Dict String (Dict String (IR Value)))


{-| A helper for making new `IR` nodes
-}
ir : valueOrType -> IR valueOrType
ir =
    IR (Metadata Dict.empty)


{-| Tools for working with `Metadata` attached to the `IR Value` or `IR Type`
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

    metadata =
        Gadget.int
            |> range 0 10
            |> Gadget.IR.irType
            |> (\(Gadget.IR.IR metadata_ _) ->
                metadata_
               )

    value =
        tools.decode "range" (Gadget.tuple Gadget.int Gadget.int) metadata

    value --> Just ( 0, 10 )

    allValues =
        tools.debug metadata

    allValues --: List (String, List (String, String))

-}
type alias MetadataTools meta a =
    { attach : String -> Gadget meta -> meta -> Gadget a -> Gadget a
    , decode : String -> Gadget meta -> Metadata -> Maybe meta
    , get : String -> Metadata -> Maybe (IR Value)
    , debug : Metadata -> List ( String, List ( String, String ) )
    }


{-| Make tools for working with `Metadata` for a specific adapter.

`Metadata` is effectively a key-value store that allows adapters to attach extra
information to the `IR Value` and `IR Type` nodes produced by Gadgets.

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
        insert key metaGadget value (Metadata m) =
            Dict.update adapterId
                (\maybe ->
                    case maybe of
                        Just adapterDict ->
                            Just (Dict.insert key (fromInput metaGadget value) adapterDict)

                        Nothing ->
                            Just (Dict.singleton key (fromInput metaGadget value))
                )
                m
                |> Metadata

        attach key metaGadget value (Gadget c) =
            Gadget
                { fromInput =
                    \input ->
                        let
                            (IR metadata inner) =
                                c.fromInput input
                        in
                        IR (insert key metaGadget value metadata) inner
                , toOutput =
                    \ir_ ->
                        c.toOutput ir_
                , irType =
                    let
                        (IR metadata inner) =
                            c.irType
                    in
                    IR (insert key metaGadget value metadata) inner
                }

        get key (Metadata m) =
            m
                |> Dict.get adapterId
                |> Maybe.andThen (Dict.get key)

        decode key metaGadget metadata =
            get key metadata
                |> Maybe.andThen (toOutput metaGadget >> Result.toMaybe)

        debug (Metadata m) =
            Dict.map
                (\_ dict ->
                    dict
                        |> Dict.map (\_ value -> debugValue value)
                        |> Dict.toList
                )
                m
                |> Dict.toList

        debugValue (IR _ value) =
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
    in
    { attach = attach
    , decode = decode
    , get = get
    , debug = debug
    }
