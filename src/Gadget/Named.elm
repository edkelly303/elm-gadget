module Gadget.Named exposing
    ( NamedRecordGadgetBuilder, record, field, endRecord
    , NamedCustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
    , variant4, variant5, endCustom
    )

{-| The functions in this module are drop-in replacements for the `record`,
`field`, `endRecord`, `custom`, `variant0`, `variant1`, `variant2`, `variant3`,
`variant4`, `variant5`, and `endCustom` functions in the `Gadget` module.

The difference is that with these functions, you can track the names of record
fields and custom type variants, which may be useful when you're writing certain
types of adapters.

For example, if you wanted to write an adapter that would pretty-print Elm
values, you need to be able to print out the record field names and custom type
variant names.

@docs NamedRecordGadgetBuilder, record, field, endRecord

@docs NamedCustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
@docs variant4, variant5, endCustom

-}

import Gadget
import Gadget.IR as IR exposing (IR(..), Type(..), Value(..))
import List.Extra


tools : IR.MetadataTools a
tools =
    IR.makeMetadataTools "Gadget.Named"


{-| A type used to build Gadgets for records. Like `RecordGadgetBuilder`, but it
tracks the names of each record field.
-}
type NamedRecordGadgetBuilder input output
    = NamedRecordGadgetBuilder
        { names : List String
        , builder : Gadget.RecordGadgetBuilder input output
        }


{-| Start the definition of a record that will track the names of fields.
-}
record : output -> NamedRecordGadgetBuilder input output
record ctor =
    NamedRecordGadgetBuilder
        { names = []
        , builder = Gadget.record ctor
        }


{-| Add a field to the definition of a record, tracking its name.
-}
field :
    String
    -> (input -> field)
    -> Gadget.Gadget field
    -> NamedRecordGadgetBuilder input (field -> output)
    -> NamedRecordGadgetBuilder input output
field name getter this (NamedRecordGadgetBuilder prev) =
    NamedRecordGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.field getter this prev.builder
        }


{-| Complete the definition of a named record.
-}
endRecord : NamedRecordGadgetBuilder a a -> IR.Gadget a
endRecord (NamedRecordGadgetBuilder prev) =
    let
        (IR.Gadget gadget) =
            Gadget.endRecord prev.builder

        (IR metadata type_) =
            gadget.irType

        newMetadata =
            prev.names
                |> List.reverse
                |> List.Extra.indexedFoldl
                    (\idx name out ->
                        tools.insert
                            (String.fromInt idx)
                            (StringValue name)
                            out
                    )
                    metadata
    in
    IR.Gadget
        { fromInput =
            \input ->
                let
                    (IR _ value) =
                        gadget.fromInput input
                in
                IR newMetadata value
        , toOutput = gadget.toOutput
        , irType = IR newMetadata type_
        }


{-| A type used to build Gadgets for custom types, where we track the names of
each variant.
-}
type NamedCustomGadgetBuilder input hasAtLeastOneVariant output
    = NamedCustomGadgetBuilder
        { names : List String
        , builder : Gadget.CustomGadgetBuilder input hasAtLeastOneVariant output
        }


{-| Start the definition of a custom type that will track the names of each
variant.
-}
custom : input -> NamedCustomGadgetBuilder input Never output
custom match =
    NamedCustomGadgetBuilder
        { names = []
        , builder = Gadget.custom match
        }


{-| Add a variant with no arguments to the definition of a custom type and track
its name.
-}
variant0 :
    String
    -> output
    -> NamedCustomGadgetBuilder (IR Value -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant0 name ctor (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant0 ctor prev.builder
        }


{-| Add a variant with one argument to the definition of a custom type and track
its name.
-}
variant1 :
    String
    -> (arg1 -> output)
    -> IR.Gadget arg1
    -> NamedCustomGadgetBuilder ((arg1 -> IR Value) -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant1 name ctor arg1 (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant1 ctor arg1 prev.builder
        }


{-| Add a variant with two arguments to the definition of a custom type and
track its name.
-}
variant2 :
    String
    -> (arg1 -> arg2 -> output)
    -> IR.Gadget arg1
    -> IR.Gadget arg2
    -> NamedCustomGadgetBuilder ((arg1 -> arg2 -> IR Value) -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant2 name ctor arg1 arg2 (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant2 ctor arg1 arg2 prev.builder
        }


{-| Add a variant with three arguments to the definition of a custom type and
track its name.
-}
variant3 :
    String
    -> (arg1 -> arg2 -> arg3 -> output)
    -> IR.Gadget arg1
    -> IR.Gadget arg2
    -> IR.Gadget arg3
    -> NamedCustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> IR Value) -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant3 name ctor arg1 arg2 arg3 (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant3 ctor arg1 arg2 arg3 prev.builder
        }


{-| Add a variant with four arguments to the definition of a custom type and
track its name.
-}
variant4 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> output)
    -> IR.Gadget arg1
    -> IR.Gadget arg2
    -> IR.Gadget arg3
    -> IR.Gadget arg4
    -> NamedCustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> IR Value) -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant4 name ctor arg1 arg2 arg3 arg4 (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant4 ctor arg1 arg2 arg3 arg4 prev.builder
        }


{-| Add a variant with five arguments to the definition of a custom type and
track its name.
-}
variant5 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> output)
    -> IR.Gadget arg1
    -> IR.Gadget arg2
    -> IR.Gadget arg3
    -> IR.Gadget arg4
    -> IR.Gadget arg5
    -> NamedCustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> IR Value) -> input) variantType output
    -> NamedCustomGadgetBuilder input () output
variant5 name ctor arg1 arg2 arg3 arg4 arg5 (NamedCustomGadgetBuilder prev) =
    NamedCustomGadgetBuilder
        { names = name :: prev.names
        , builder = Gadget.variant5 ctor arg1 arg2 arg3 arg4 arg5 prev.builder
        }


{-| Complete the definition of a custom type with tracked names.
-}
endCustom : NamedCustomGadgetBuilder (a -> IR Value) () a -> IR.Gadget a
endCustom (NamedCustomGadgetBuilder prev) =
    let
        (IR.Gadget gadget) =
            Gadget.endCustom prev.builder

        (IR metadata type_) =
            gadget.irType

        newMetadata =
            prev.names
                |> List.reverse
                |> List.Extra.indexedFoldl
                    (\idx name out ->
                        tools.insert
                            (String.fromInt idx)
                            (StringValue name)
                            out
                    )
                    metadata
    in
    IR.Gadget
        { fromInput =
            \input ->
                let
                    (IR _ value) =
                        gadget.fromInput input
                in
                IR newMetadata value
        , toOutput = gadget.toOutput
        , irType = IR newMetadata type_
        }
