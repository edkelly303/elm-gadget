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
    | Labelled (Dict String IR) IR


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
    | LabelledType (Dict String IR) IRType
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


{-| Add metadata to a Gadget.

For an example of why this might be helpful, see the docs for
[`Gadget.Adapter.Fuzz.fuzzerWithOverrides`](Gadget-Adapter-Fuzz#fuzzerWithOverrides).

    import Gadget
    import Gadget.IR

    labelled =
        Gadget.int
            |> Gadget.IR.withMetadata "max" (Gadget.IR.Int 1)

    labelled --: Gadget.Gadget Int

-}
withMetadata : String -> IR -> Gadget a -> Gadget a
withMetadata label_ meta (Gadget c) =
    Gadget
        { fromInput =
            \input ->
                case c.fromInput input of
                    Labelled labels inner ->
                        Labelled (Dict.insert label_ meta labels) inner

                    other ->
                        Labelled (Dict.singleton label_ meta) other
        , toOutput =
            \ir ->
                case ir of
                    Labelled _ innerIR ->
                        c.toOutput innerIR

                    _ ->
                        c.toOutput ir
        , irType =
            case c.irType of
                LabelledType labels inner ->
                    LabelledType (Dict.insert label_ meta labels) inner

                other ->
                    LabelledType (Dict.singleton label_ meta) other
        }
