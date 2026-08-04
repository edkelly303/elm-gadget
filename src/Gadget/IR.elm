module Gadget.IR exposing
    ( Gadget(..)
    , fromInput, irType, toOutput, Error
    , IR(..), Variant(..), Type(..), VariantType(..)
    , Metadata, MetadataTools, makeMetadataTools
    , Value(..), ir
    )

{-| Tools for creating adapters for Gadgets.

To see some examples of how to use this module, look at the source code for the
various `Gadget.Adapter` modules in this package:

  - The simplest one is probably [`Gadget.Adapter.Html`](Gadget-Adapter-Html).
  - For a bidirectional example, try [`Gadget.Adapter.Json`](Gadget-Adapter-Json).
  - For an example of how to override Gadgets, see [`Gadget.Adapter.Fuzz`](Gadget-Adapter-Fuzz).

@docs Gadget

@docs fromInput, irType, toOutput, Error

@docs IR, Data, Variant, Type, VariantType

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


{-| `IR` values are variants of this type. All Elm values (as long as they don't
contain functions) should be able to be encoded as a value of this type.
-}
type IR a
    = IR Metadata a


type Value
    = Bool Bool
    | Char Char
    | String String
    | Int Int
    | Float Float
    | Custom Int Variant
    | Product (List (IR Value))
    | List (List (IR Value))


{-| A type used by the `Custom` constructor of the `IR` type.
-}
type Variant
    = Variant0
    | Variant1 (IR Value)
    | Variant2 (IR Value) (IR Value)
    | Variant3 (IR Value) (IR Value) (IR Value)
    | Variant4 (IR Value) (IR Value) (IR Value) (IR Value)
    | Variant5 (IR Value) (IR Value) (IR Value) (IR Value) (IR Value)


{-| Any IR value will have a "type" that is a variant of `IR Type`.
-}
type Type
    = BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType VariantType (List VariantType)
    | ProductType (List (IR Type))
    | ListType (IR Type)
    | LazyType (() -> IR Type)


{-| A type used by the `Custom` constructor of the `IR Type` type.
-}
type VariantType
    = Variant0Type
    | Variant1Type (IR Type)
    | Variant2Type (IR Type) (IR Type)
    | Variant3Type (IR Type) (IR Type) (IR Type)
    | Variant4Type (IR Type) (IR Type) (IR Type) (IR Type)
    | Variant5Type (IR Type) (IR Type) (IR Type) (IR Type) (IR Type)


{-| Use an appropriate Gadget to convert an Elm value into an `IR` value.
-}
fromInput : Gadget a -> a -> IR Value
fromInput (Gadget c) input =
    c.fromInput input


{-| Use an appropriate Gadget to extract the `IR Type` of an Elm value.
-}
irType : Gadget a -> IR Type
irType (Gadget c) =
    c.irType


{-| Use an appropriate Gadget to attempt to convert an `IR` value into an Elm
value.
-}
toOutput : Gadget a -> IR Value -> Result Error a
toOutput (Gadget c) a =
    c.toOutput a


{-| A type used to carry metadata for `IR` nodes
-}
type Metadata
    = Metadata (Dict String (Dict String Value))


{-| A helper for making new `IR` nodes
-}
ir : a -> IR a
ir =
    IR (Metadata Dict.empty)


{-| Tools for working with `Metadata` attached to the `IR` produced by a Gadget.
-}
type alias MetadataTools a =
    { attach : String -> Value -> Gadget a -> Gadget a
    , get : String -> Metadata -> Maybe Value
    , member : String -> Metadata -> Bool
    , export : Metadata -> List ( String, List ( String, Value ) )
    }


{-| Make tools for working with `Metadata` for a specific adapter.
-}
makeMetadataTools : String -> MetadataTools a
makeMetadataTools adapterId =
    let
        new key value =
            Dict.singleton adapterId (Dict.singleton key value)
                |> Metadata

        insert key value (Metadata m) =
            Dict.update adapterId
                (\maybe ->
                    case maybe of
                        Just adapterDict ->
                            Just (Dict.insert key value adapterDict)

                        Nothing ->
                            Just (Dict.singleton key value)
                )
                m
                |> Metadata

        attach key value (Gadget c) =
            Gadget
                { fromInput =
                    \input ->
                        let
                            (IR metadata inner) =
                                c.fromInput input
                        in
                        IR (insert key value metadata) inner
                , toOutput =
                    \ir_ ->
                        c.toOutput ir_
                , irType =
                    let
                        (IR metadata inner) =
                            c.irType
                    in
                    IR (insert key value metadata) inner
                }

        get key (Metadata m) =
            m
                |> Dict.get adapterId
                |> Maybe.andThen (Dict.get key)

        member key metadata =
            get key metadata /= Nothing

        export (Metadata m) =
            Dict.map (\_ v -> Dict.toList v) m
                |> Dict.toList
    in
    { attach = attach
    , get = get
    , member = member
    , export = export
    }
