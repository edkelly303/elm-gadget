module Gadget.IR exposing
    ( Gadget(..)
    , fromInput, irType, toOutput, Error
    , IR(..), Variant(..), IRType(..), VariantType(..)
    , withMetadata
    )

{-| Tools for creating adapters for Gadgets.

To see some examples of how to use this module, look at the source code for the
various `Gadget.Adapter` modules in this package:

  - The simplest one is probably [`Gadget.Adapter.Html`](Gadget-Adapter-Html).
  - For a bidirectional example, try [`Gadget.Adapter.Json`](Gadget-Adapter-Json).
  - For an example of how to override Gadgets, see [`Gadget.Adapter.Fuzz`](Gadget-Adapter-Fuzz).

@docs Gadget

@docs fromInput, irType, toOutput, Error

@docs IR, Variant, IRType, VariantType

@docs withMetadata

-}

import Dict exposing (Dict)


{-| The core type of this package. Use the functions in this module together
with an appropriate Gadget to convert values to and from the `IR` type.
-}
type Gadget a
    = Gadget
        { fromInput : a -> IR
        , toOutput : IR -> Result Error a
        , irType : IRType
        }


{-| An error that may be generated if `toOutput` fails.
-}
type alias Error =
    String


{-| `IR` values are variants of this type. All Elm values (as long as they don't
contain functions) should be able to be encoded as a value of this type.
-}
type IR
    = Bool Bool
    | Char Char
    | String String
    | Int Int
    | Float Float
    | Custom Int Variant
    | Product (List IR)
    | List (List IR)
    | WithMetadata (Dict String IR) IR


{-| A type used by the `Custom` constructor of the `IR` type.
-}
type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR
    | Variant3 IR IR IR
    | Variant4 IR IR IR IR
    | Variant5 IR IR IR IR IR


{-| Any IR value will have a "type" that is a variant of `IRType`.
-}
type IRType
    = BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType VariantType (List VariantType)
    | ProductType (List IRType)
    | ListType IRType
    | WithMetadataType (Dict String IR) IRType
    | LazyType (() -> IRType)


{-| A type used by the `Custom` constructor of the `IRType` type.
-}
type VariantType
    = Variant0Type
    | Variant1Type IRType
    | Variant2Type IRType IRType
    | Variant3Type IRType IRType IRType
    | Variant4Type IRType IRType IRType IRType
    | Variant5Type IRType IRType IRType IRType IRType


{-| Use an appropriate Gadget to convert an Elm value into an `IR` value.
-}
fromInput : Gadget a -> a -> IR
fromInput (Gadget c) input =
    c.fromInput input


{-| Use an appropriate Gadget to extract the `IRType` of an Elm value.
-}
irType : Gadget a -> IRType
irType (Gadget c) =
    c.irType


{-| Use an appropriate Gadget to attempt to convert an `IR` value into an Elm
value.
-}
toOutput : Gadget a -> IR -> Result Error a
toOutput (Gadget c) a =
    c.toOutput a


{-| Attach metadata to the `IR` and `IRType` produced by a Gadget.

The metadata is stored as `Dict String IR`, and each time `withMetadata` is
called, it will add a new key and value to the dictionary.

You can use this to pass additional information to an adapter. Why? Perhaps you
want to tweak the output of a fuzzer or generator, or even override the
adapter's default fuzzer with a completely different one.

See for example the implementation of
[`Gadget.Adapter.Random.limit`](Gadget-Adapter-Random#limit):

    import Gadget
    import Gadget.IR

    limit : Int -> Int -> IR.Gadget Int -> IR.Gadget Int
    limit lo hi intGadget =
        intGadget
            |> Gadget.IR.withMetadata
                "random_int_lo"
                (Gadget.IR.Int lo)
            |> Gadget.IR.withMetadata
                "random_int_hi"
                (Gadget.IR.Int hi)

    intBetween1And5 =
        Gadget.int
            |> limit 1 5

Then we can write an random generator adapter that interprets the `IRType`
produced by `intBetween1And5` and checks whether its metadata dictionary
contains the keys "random\_int\_lo" or "random\_int\_hi". If it does, then the
adapter can set the specified lower and upper bounds on the `Int` that it
generates.

-}
withMetadata : String -> IR -> Gadget a -> Gadget a
withMetadata label_ meta (Gadget c) =
    Gadget
        { fromInput =
            \input ->
                case c.fromInput input of
                    WithMetadata labels inner ->
                        WithMetadata (Dict.insert label_ meta labels) inner

                    other ->
                        WithMetadata (Dict.singleton label_ meta) other
        , toOutput =
            \ir ->
                case ir of
                    WithMetadata _ innerIR ->
                        c.toOutput innerIR

                    _ ->
                        c.toOutput ir
        , irType =
            case c.irType of
                WithMetadataType labels inner ->
                    WithMetadataType (Dict.insert label_ meta labels) inner

                other ->
                    WithMetadataType (Dict.singleton label_ meta) other
        }
