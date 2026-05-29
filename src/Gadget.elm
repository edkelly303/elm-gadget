module Gadget exposing
    ( Gadget
    , bool, char, string, int, float
    , list, array, dict, set
    , tuple, triple
    , maybe, result
    , RecordGadgetBuilder, record, field, endRecord
    , CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
    , variant4, variant5, endCustom
    , map, filterMap, lazy
    , label
    )

{-| This module is for application developers who want to create Gadgets and use
them with pre-existing adapters.

The main idea is that you define a Gadget for each of the Elm types you declare
in your application, and then you can use various adapters to get different
functionality.

There's a good example in the documentation for
[`Gadget.Adapter.Fuzz.fuzzer`](Gadget-Adapter-Fuzz#fuzzer) and
[`Gadget.Adapter.Random.generator`](Gadget-Adapter-Random#generator). You'll see
the definition of `personGadget` is identical in both examples, and it can be
used to create both a fuzzer and a random generator simply by passing it to the
appropriate adapter functions.

If you want to make your own adapters, see the [`Gadget.IR`](Gadget-IR) module.


# Gadgets

@docs Gadget


# Primitives

@docs bool, char, string, int, float


# Combinators

@docs list, array, dict, set

@docs tuple, triple

@docs maybe, result


# Records

    import Gadget

    type alias Person =
        { name : String
        , age : Int
        }

    personGadget =
        Gadget.record Person
            |> Gadget.field .name Gadget.string
            |> Gadget.field .age Gadget.int
            |> Gadget.endRecord

    personGadget --: Gadget.Gadget Person

@docs RecordGadgetBuilder, record, field, endRecord


# Custom types

    import Gadget

    type Shape
        = Rectangle Int Int
        | Circle Int

    shapeGadget =
        Gadget.custom
            (\rectangle circle variant ->
                case variant of
                    Rectangle width height ->
                        rectangle width height
                    Circle radius ->
                        circle radius
            )
            |> Gadget.variant2 Rectangle Gadget.int Gadget.int
            |> Gadget.variant1 Circle Gadget.int
            |> Gadget.endCustom

    shapeGadget --: Gadget.Gadget Shape

@docs CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
@docs variant4, variant5, endCustom


# Transforming Gadgets

@docs map, filterMap, lazy


# Labelling Gadgets

@docs label

-}

import Array
import Dict
import Gadget.IR
    exposing
        ( Error
        , Gadget(..)
        , IR(..)
        , IRType(..)
        , Variant(..)
        , VariantType(..)
        )
import Result.Extra
import Set


{-| The core type of this package. Use the primitives and combinators in this
module to define Gadgets for the types in your application.
-}
type alias Gadget a =
    Gadget.IR.Gadget a


{-| A type used to build Gadgets for records.
-}
type RecordGadgetBuilder input output
    = RecordGadgetBuilder
        { fromInput : input -> List IR
        , toOutput : List IR -> Result Error output
        , irType : List IRType
        }


{-| A type used to build Gadgets for custom types.
-}
type CustomGadgetBuilder input hasAtLeastOneVariant output
    = CustomGadgetBuilder
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


{-| A Gadget for the `Bool` primitive type.
-}
bool : Gadget Bool
bool =
    Gadget
        { fromInput = Bool
        , toOutput =
            \ir ->
                case ir of
                    Bool b ->
                        Ok b

                    _ ->
                        Err "bool toOutput failed"
        , irType = BoolType
        }


{-| A Gadget for the `Char` primitive type.
-}
char : Gadget Char
char =
    Gadget
        { fromInput = Char
        , toOutput =
            \ir ->
                case ir of
                    Char c ->
                        Ok c

                    _ ->
                        Err "char toOutput failed"
        , irType = CharType
        }


{-| A Gadget for the `String` primitive type.
-}
string : Gadget String
string =
    Gadget
        { fromInput = String
        , toOutput =
            \ir ->
                case ir of
                    String s ->
                        Ok s

                    _ ->
                        Err "string toOutput failed"
        , irType = StringType
        }


{-| A Gadget for the `Int` primitive type.
-}
int : Gadget Int
int =
    Gadget
        { fromInput = Int
        , toOutput =
            \ir ->
                case ir of
                    Int i ->
                        Ok i

                    _ ->
                        Err "int toOutput failed"
        , irType = IntType
        }


{-| A Gadget for the `Float` primitive type.
-}
float : Gadget Float
float =
    Gadget
        { fromInput = Float
        , toOutput =
            \ir ->
                case ir of
                    Float s ->
                        Ok s

                    _ ->
                        Err "float toOutput failed"
        , irType = FloatType
        }


{-| A combinator used to define Gadgets for lists.

    import Gadget

    listGadget =
        Gadget.list Gadget.int

    listGadget --: Gadget.Gadget (List Int)

-}
list : Gadget a -> Gadget (List a)
list (Gadget item) =
    Gadget
        { fromInput = \items -> List (List.map item.fromInput items)
        , toOutput =
            \ir ->
                case ir of
                    List items ->
                        List.map item.toOutput items
                            |> Result.Extra.combine

                    _ ->
                        Err "list toOutput failed"
        , irType = ListType item.irType
        }


{-| A combinator used to define Gadgets for dictionaries.

    import Gadget
    import Dict

    dictGadget =
        Gadget.dict Gadget.int Gadget.string

    dictGadget --: Gadget.Gadget (Dict.Dict Int String)

-}
dict :
    Gadget comparable
    -> Gadget v
    -> Gadget (Dict.Dict comparable v)
dict key value =
    list (tuple key value)
        |> map Dict.fromList Dict.toList


{-| A combinator used to define Gadgets for sets.

    import Gadget
    import Set

    dictGadget =
        Gadget.set Gadget.int

    dictGadget --: Gadget.Gadget (Set.Set Int)

-}
set :
    Gadget comparable
    -> Gadget (Set.Set comparable)
set value =
    list value
        |> map Set.fromList Set.toList


{-| A combinator used to define Gadgets for arrays.

    import Gadget
    import Array

    arrayGadget =
        Gadget.array Gadget.int

    arrayGadget --: Gadget.Gadget (Array.Array Int)

-}
array : Gadget a -> Gadget (Array.Array a)
array item =
    list item
        |> map Array.fromList Array.toList


{-| A combinator used to define Gadgets for the `Maybe` type.

    import Gadget

    maybeGadget =
        Gadget.maybe Gadget.int

    maybeGadget --: Gadget.Gadget (Maybe Int)

-}
maybe : Gadget a -> Gadget (Maybe a)
maybe item =
    custom
        (\just nothing variant ->
            case variant of
                Just a ->
                    just a

                Nothing ->
                    nothing
        )
        |> variant1 Just item
        |> variant0 Nothing
        |> endCustom


{-| A combinator used to define Gadgets for the `Result` type.

    import Gadget

    resultGadget =
        Gadget.result Gadget.string Gadget.int

    resultGadget --: Gadget.Gadget (Result String Int)

-}
result : Gadget x -> Gadget a -> Gadget (Result x a)
result x a =
    custom
        (\err ok variant ->
            case variant of
                Err x_ ->
                    err x_

                Ok a_ ->
                    ok a_
        )
        |> variant1 Err x
        |> variant1 Ok a
        |> endCustom


{-| A combinator used to define Gadgets for tuples.

    import Gadget

    tupleGadget =
        Gadget.tuple Gadget.string Gadget.int

    tupleGadget --: Gadget.Gadget ( String, Int )

-}
tuple : Gadget a -> Gadget b -> Gadget ( a, b )
tuple a b =
    record Tuple.pair
        |> field Tuple.first a
        |> field Tuple.second b
        |> endRecord


{-| A combinator used to define Gadgets for triples.

    import Gadget

    tripleGadget =
        Gadget.triple Gadget.string Gadget.int Gadget.float

    tripleGadget --: Gadget.Gadget ( String, Int, Float )

-}
triple : Gadget a -> Gadget b -> Gadget c -> Gadget ( a, b, c )
triple a b c =
    record (\a_ b_ c_ -> ( a_, b_, c_ ))
        |> field (\( a_, _, _ ) -> a_) a
        |> field (\( _, b_, _ ) -> b_) b
        |> field (\( _, _, c_ ) -> c_) c
        |> endRecord


{-| Start the definition of a custom type.
-}
custom : input -> CustomGadgetBuilder input Never output
custom match =
    CustomGadgetBuilder
        { match = match
        , index = 0
        , fromIR = \_ -> Err "custom toOutput failed"
        , variantTypes = []
        }


{-| Add a variant with zero arguments to the definition of a custom type.
-}
variant0 :
    output
    -> CustomGadgetBuilder (IR -> input) variantType output
    -> CustomGadgetBuilder input () output
variant0 ctor (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| Custom prev.index Variant0
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected Variant0 ->
                        if selected == prev.index then
                            Ok ctor

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant0Type
                :: prev.variantTypes
        }


{-| Add a variant with one argument to the definition of a custom type.
-}
variant1 :
    (arg1 -> output)
    -> Gadget arg1
    -> CustomGadgetBuilder ((arg1 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant1 ctor (Gadget argfns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.fromInput arg))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant1 arg) ->
                        if selected == prev.index then
                            Result.map ctor (argfns.toOutput arg)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant1Type argfns.irType
                :: prev.variantTypes
        }


{-| Add a variant with two arguments to the definition of a custom type.
-}
variant2 :
    (arg1 -> arg2 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> CustomGadgetBuilder ((arg1 -> arg2 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant2 ctor (Gadget arg1fns) (Gadget arg2fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.fromInput arg1) (arg2fns.fromInput arg2))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant2 arg1 arg2) ->
                        if selected == prev.index then
                            Result.map2 ctor (arg1fns.toOutput arg1) (arg2fns.toOutput arg2)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant2Type arg1fns.irType arg2fns.irType
                :: prev.variantTypes
        }


{-| Add a variant with three arguments to the definition of a custom type.
-}
variant3 :
    (arg1 -> arg2 -> arg3 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant3 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 ->
                    Custom prev.index
                        (Variant3
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant3 arg1 arg2 arg3) ->
                        if selected == prev.index then
                            Result.map3 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant3Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                :: prev.variantTypes
        }


{-| Add a variant with four arguments to the definition of a custom type.
-}
variant4 :
    (arg1 -> arg2 -> arg3 -> arg4 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant4 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 ->
                    Custom prev.index
                        (Variant4
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant4 arg1 arg2 arg3 arg4) ->
                        if selected == prev.index then
                            Result.map4 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)
                                (arg4fns.toOutput arg4)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant4Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
                :: prev.variantTypes
        }


{-| Add a variant with five arguments to the definition of a custom type.
-}
variant5 :
    (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> Gadget arg5
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant5 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (Gadget arg5fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 arg5 ->
                    Custom prev.index
                        (Variant5
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                            (arg5fns.fromInput arg5)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant5 arg1 arg2 arg3 arg4 arg5) ->
                        if selected == prev.index then
                            Result.map5 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)
                                (arg4fns.toOutput arg4)
                                (arg5fns.toOutput arg5)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant5Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
                arg5fns.irType
                :: prev.variantTypes
        }


{-| Complete the definition of a custom type.
-}
endCustom : CustomGadgetBuilder (a -> IR) () a -> Gadget a
endCustom (CustomGadgetBuilder prev) =
    Gadget
        { fromInput = prev.match
        , toOutput = prev.fromIR
        , irType =
            case List.reverse prev.variantTypes of
                [] ->
                    -- we know this can't happen, because if the second type
                    -- variable of CustomGadgetBuilder is `()`, then we know
                    -- that we've used at least one `variantX` function, so the
                    -- list of variants can't be empty. So it's ok to use a
                    -- spurious Variant0Type here, because this will never get
                    -- produced.
                    CustomType Variant0Type []

                firstVariantType :: restVariantTypes ->
                    CustomType firstVariantType restVariantTypes
        }


{-| Start the definition of a record.
-}
record : output -> RecordGadgetBuilder input output
record ctor =
    RecordGadgetBuilder
        { fromInput = \_ -> []
        , toOutput = \_ -> Ok ctor
        , irType = []
        }


{-| Add a field to the definition of a record.
-}
field :
    (input -> field)
    -> Gadget field
    -> RecordGadgetBuilder input (field -> output)
    -> RecordGadgetBuilder input output
field getter (Gadget gadget) (RecordGadgetBuilder builder) =
    RecordGadgetBuilder
        { fromInput =
            \input ->
                let
                    thisField =
                        gadget.fromInput (getter input)

                    prevFields =
                        builder.fromInput input
                in
                thisField :: prevFields
        , toOutput =
            \fields ->
                case fields of
                    thisField :: prevFields ->
                        Result.map2 (\ctor val -> ctor val)
                            (builder.toOutput prevFields)
                            (gadget.toOutput thisField)

                    [] ->
                        Err "expecting a Product field"
        , irType =
            gadget.irType :: builder.irType
        }


{-| Complete the definition of a record.
-}
endRecord : RecordGadgetBuilder a a -> Gadget a
endRecord (RecordGadgetBuilder builder) =
    Gadget
        { fromInput =
            \input ->
                Product (List.reverse (builder.fromInput input))
        , toOutput =
            \ir ->
                case ir of
                    Product fields ->
                        builder.toOutput (List.reverse fields)

                    _ ->
                        Err "expecting a Product"
        , irType = ProductType (List.reverse builder.irType)
        }


{-| Convert a Gadget of one type to a Gadget of another type.

    import Gadget

    charListGadget =
        Gadget.string
            |> Gadget.map
                String.toList
                String.fromList

    charListGadget --: Gadget.Gadget (List Char)

-}
map :
    (a -> b)
    -> (b -> a)
    -> Gadget a
    -> Gadget b
map aToB bToA (Gadget prev) =
    Gadget
        { fromInput = bToA >> prev.fromInput
        , toOutput = prev.toOutput >> Result.map aToB
        , irType = prev.irType
        }


{-| Convert a Gadget of one type to a Gadget of another type,
possibly failing the conversion to the more narrow result type.

    import Gadget

    namesGadget =
        Gadget.list Gadget.string
            |> Gadget.filterMap
                (\list ->
                    case list of
                        [] ->
                            Err "must contain at least one name"

                        h :: t ->
                            Ok ( h, t )
                )
                (\( h, t ) -> h :: t)

    namesGadget --: Gadget.Gadget (List String)

Do not use this for overly fine-grained checks (like checking for a fixed set of valid values),
as this might leave generators and fuzzers scrambling to find any valid values.
Instead, prefer the constructive approach.

-}
filterMap :
    (a -> Result String b)
    -> (b -> a)
    -> Gadget a
    -> Gadget b
filterMap aToB bToA (Gadget prev) =
    Gadget
        { fromInput = bToA >> prev.fromInput
        , toOutput = prev.toOutput >> Result.andThen aToB
        , irType = prev.irType
        }


{-| Construct this step lazily to avoid elm complaining about an infinitely recursive value.

    type Tree
        = Leaf String
        | Branch Tree Tree

    treeGadget =
        Gadget.custom
            (\leaf branch tree ->
                case tree of
                    Leaf value ->
                        leaf value

                    Branch branch0 branch1 ->
                        branch branch0 branch1
            )
            |> Gadget.variant1 Leaf Gadget.string
            |> Gadget.variant2 Branch
                (Gadget.lazy (\() -> treeGadget))
                (Gadget.lazy (\() -> treeGadget))
            |> Gadget.endCustom

    treeGadget --: Gadget.Gadget Tree

-}
lazy : (() -> Gadget a) -> Gadget a
lazy step =
    Gadget
        { fromInput =
            \value ->
                let
                    (Gadget gadget) =
                        step ()
                in
                value |> gadget.fromInput
        , toOutput =
            \ir ->
                let
                    (Gadget gadget) =
                        step ()
                in
                ir |> gadget.toOutput
        , irType =
            Gadget.IR.LazyType
                (\() ->
                    let
                        (Gadget gadget) =
                            step ()
                    in
                    gadget.irType
                )
        }


{-| Add a label to a Gadget.

For an example of why this might be helpful, see the docs for
[`Gadget.Adapter.Fuzz.fuzzerWithOverrides`](Gadget-Adapter-Fuzz#fuzzerWithOverrides).

    import Gadget

    labelled =
        Gadget.int
            |> Gadget.label "age"

    labelled --: Gadget.Gadget Int

-}
label : String -> Gadget a -> Gadget a
label label_ (Gadget c) =
    Gadget
        { fromInput =
            \input ->
                case c.fromInput input of
                    Labelled labels inner ->
                        Labelled (Set.insert label_ labels) inner

                    other ->
                        Labelled (Set.singleton label_) other
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
                    LabelledType (Set.insert label_ labels) inner

                other ->
                    LabelledType (Set.singleton label_) other
        }
