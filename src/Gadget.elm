module Gadget exposing
    ( Gadget
    , unit, bool, char, string, int, float
    , list, array, dict, set
    , tuple, triple
    , maybe, result
    , RecordGadgetBuilder, record, field, endRecord
    , CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
    , variant4, variant5, endCustom
    , map, filterMap, lazy
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

@docs unit, bool, char, string, int, float


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
            |> Gadget.field "name" .name Gadget.string
            |> Gadget.field "age" .age Gadget.int
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
            |> Gadget.variant2 "Rectangle" Rectangle Gadget.int Gadget.int
            |> Gadget.variant1 "Circle" Circle Gadget.int
            |> Gadget.endCustom

    shapeGadget --: Gadget.Gadget Shape

@docs CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3
@docs variant4, variant5, endCustom


# Transforming Gadgets

@docs map, filterMap, lazy

-}

import Array
import Dict
import Gadget.IR as IR
    exposing
        ( Error
        , Gadget(..)
        , Path
        , Type(..)
        , Value(..)
        , VariantType(..)
        , VariantValue(..)
        )
import Result.Extra
import Set


{-| The core type of this package. Use the primitives and combinators in this
module to define Gadgets for the types in your application.
-}
type alias Gadget a =
    IR.Gadget a


{-| A type used to build Gadgets for records.
-}
type RecordGadgetBuilder input output
    = RecordGadgetBuilder
        { fromInput : input -> List ( String, Value )
        , toOutput : Path -> List ( String, Value ) -> Result (List Error) output
        , irType : List ( String, Type )
        }


{-| A type used to build Gadgets for custom types.
-}
type CustomGadgetBuilder input hasAtLeastOneVariant output
    = CustomGadgetBuilder
        { match : input
        , fromIR : Path -> Value -> Result (List Error) output
        , variantTypes : List ( String, VariantType )
        , index : Int
        }


{-| A Gadget for the unit primitive type.
-}
unit : Gadget ()
unit =
    Gadget
        { fromInput = \() -> UnitValue
        , toOutput =
            \path value ->
                case value of
                    UnitValue ->
                        Ok ()

                    _ ->
                        Err [ { error = "unit toOutput failed", path = path } ]
        , irType = UnitType IR.emptyMetadata
        }


{-| A Gadget for the `Bool` primitive type.
-}
bool : Gadget Bool
bool =
    Gadget
        { fromInput = BoolValue
        , toOutput =
            \path value ->
                case value of
                    BoolValue b ->
                        Ok b

                    _ ->
                        Err [ { error = "bool toOutput failed", path = path } ]
        , irType = BoolType IR.emptyMetadata
        }


{-| A Gadget for the `Char` primitive type.
-}
char : Gadget Char
char =
    Gadget
        { fromInput = CharValue
        , toOutput =
            \path value ->
                case value of
                    CharValue c ->
                        Ok c

                    _ ->
                        Err [ { error = "char toOutput failed", path = path } ]
        , irType = CharType IR.emptyMetadata
        }


{-| A Gadget for the `String` primitive type.
-}
string : Gadget String
string =
    Gadget
        { fromInput = StringValue
        , toOutput =
            \path value ->
                case value of
                    StringValue s ->
                        Ok s

                    _ ->
                        Err [ { error = "string toOutput failed", path = path } ]
        , irType = StringType IR.emptyMetadata
        }


{-| A Gadget for the `Int` primitive type.
-}
int : Gadget Int
int =
    Gadget
        { fromInput = IntValue
        , toOutput =
            \path value ->
                case value of
                    IntValue i ->
                        Ok i

                    _ ->
                        Err [ { error = "int toOutput failed", path = path } ]
        , irType = IntType IR.emptyMetadata
        }


{-| A Gadget for the `Float` primitive type.
-}
float : Gadget Float
float =
    Gadget
        { fromInput = FloatValue
        , toOutput =
            \path value ->
                case value of
                    FloatValue s ->
                        Ok s

                    _ ->
                        Err [ { error = "float toOutput failed", path = path } ]
        , irType = FloatType IR.emptyMetadata
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
        { fromInput = \items -> ListValue (List.map item.fromInput items)
        , toOutput =
            \path value ->
                case value of
                    ListValue items ->
                        List.indexedMap (\idx i -> item.toOutput (String.fromInt idx :: path) i) items
                            |> Result.Extra.combine

                    _ ->
                        Err [ { error = "list toOutput failed", path = path } ]
        , irType = ListType IR.emptyMetadata item.irType
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
        |> variant1 "Just" Just item
        |> variant0 "Nothing" Nothing
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
        |> variant1 "Err" Err x
        |> variant1 "Ok" Ok a
        |> endCustom


{-| A combinator used to define Gadgets for tuples.

    import Gadget

    tupleGadget =
        Gadget.tuple Gadget.string Gadget.int

    tupleGadget --: Gadget.Gadget ( String, Int )

-}
tuple : Gadget a -> Gadget b -> Gadget ( a, b )
tuple (Gadget a) (Gadget b) =
    Gadget
        { fromInput =
            \input ->
                TupleValue
                    (a.fromInput (Tuple.first input))
                    (b.fromInput (Tuple.second input))
        , toOutput =
            \path value ->
                case value of
                    TupleValue fst snd ->
                        Result.map2 Tuple.pair
                            (a.toOutput ("0" :: path) fst)
                            (b.toOutput ("1" :: path) snd)

                    _ ->
                        Err [ { error = "Not a tuple", path = path } ]
        , irType =
            TupleType IR.emptyMetadata a.irType b.irType
        }


{-| A combinator used to define Gadgets for triples.

    import Gadget

    tripleGadget =
        Gadget.triple Gadget.string Gadget.int Gadget.float

    tripleGadget --: Gadget.Gadget ( String, Int, Float )

-}
triple : Gadget a -> Gadget b -> Gadget c -> Gadget ( a, b, c )
triple (Gadget a) (Gadget b) (Gadget c) =
    Gadget
        { fromInput =
            \( fst, snd, thd ) ->
                TripleValue
                    (a.fromInput fst)
                    (b.fromInput snd)
                    (c.fromInput thd)
        , toOutput =
            \path value ->
                case value of
                    TripleValue fst snd thd ->
                        Result.map3
                            (\fstOutput sndOutput thdOutput ->
                                ( fstOutput, sndOutput, thdOutput )
                            )
                            (a.toOutput ("0" :: path) fst)
                            (b.toOutput ("1" :: path) snd)
                            (c.toOutput ("2" :: path) thd)

                    _ ->
                        Err [ { error = "Not a triple", path = path } ]
        , irType =
            TripleType IR.emptyMetadata a.irType b.irType c.irType
        }


{-| Start the definition of a custom type.
-}
custom : input -> CustomGadgetBuilder input Never output
custom match =
    CustomGadgetBuilder
        { match = match
        , index = 0
        , fromIR = \path _ -> Err [ { error = "custom toOutput failed", path = path } ]
        , variantTypes = []
        }


{-| Add a variant with zero arguments to the definition of a custom type.
-}
variant0 :
    String
    -> output
    -> CustomGadgetBuilder (Value -> input) variantType output
    -> CustomGadgetBuilder input () output
variant0 name ctor (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| CustomValue prev.index ( name, Variant0Value )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant0Value ) ->
                        if selected == prev.index then
                            Ok ctor

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name, Variant0Type )
                :: prev.variantTypes
        }


{-| Add a variant with one argument to the definition of a custom type.
-}
variant1 :
    String
    -> (arg1 -> output)
    -> Gadget arg1
    -> CustomGadgetBuilder ((arg1 -> Value) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant1 name ctor (Gadget argfns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| \arg -> CustomValue prev.index ( name, Variant1Value (argfns.fromInput arg) )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant1Value arg ) ->
                        if selected == prev.index then
                            Result.map ctor (argfns.toOutput ("0" :: name :: path) arg)

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name, Variant1Type argfns.irType )
                :: prev.variantTypes
        }


{-| Add a variant with two arguments to the definition of a custom type.
-}
variant2 :
    String
    -> (arg1 -> arg2 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> CustomGadgetBuilder ((arg1 -> arg2 -> Value) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant2 name ctor (Gadget arg1fns) (Gadget arg2fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match = prev.match <| \arg1 arg2 -> CustomValue prev.index ( name, Variant2Value (arg1fns.fromInput arg1) (arg2fns.fromInput arg2) )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant2Value arg1 arg2 ) ->
                        if selected == prev.index then
                            Result.map2 ctor
                                (arg1fns.toOutput ("0" :: name :: path) arg1)
                                (arg2fns.toOutput ("1" :: name :: path) arg2)

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name, Variant2Type arg1fns.irType arg2fns.irType )
                :: prev.variantTypes
        }


{-| Add a variant with three arguments to the definition of a custom type.
-}
variant3 :
    String
    -> (arg1 -> arg2 -> arg3 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> Value) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant3 name ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 ->
                    CustomValue prev.index
                        ( name
                        , Variant3Value
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                        )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant3Value arg1 arg2 arg3 ) ->
                        if selected == prev.index then
                            Result.map3 ctor
                                (arg1fns.toOutput ("0" :: name :: path) arg1)
                                (arg2fns.toOutput ("1" :: name :: path) arg2)
                                (arg3fns.toOutput ("2" :: name :: path) arg3)

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name
            , Variant3Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
            )
                :: prev.variantTypes
        }


{-| Add a variant with four arguments to the definition of a custom type.
-}
variant4 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> Value) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant4 name ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 ->
                    CustomValue prev.index
                        ( name
                        , Variant4Value
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                        )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant4Value arg1 arg2 arg3 arg4 ) ->
                        if selected == prev.index then
                            Result.map4 ctor
                                (arg1fns.toOutput ("0" :: name :: path) arg1)
                                (arg2fns.toOutput ("1" :: name :: path) arg2)
                                (arg3fns.toOutput ("2" :: name :: path) arg3)
                                (arg4fns.toOutput ("3" :: name :: path) arg4)

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name
            , Variant4Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
            )
                :: prev.variantTypes
        }


{-| Add a variant with five arguments to the definition of a custom type.
-}
variant5 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> output)
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> Gadget arg5
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> Value) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant5 name ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (Gadget arg5fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 arg5 ->
                    CustomValue prev.index
                        ( name
                        , Variant5Value
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                            (arg5fns.fromInput arg5)
                        )
        , index = prev.index + 1
        , fromIR =
            \path value ->
                case value of
                    CustomValue selected ( _, Variant5Value arg1 arg2 arg3 arg4 arg5 ) ->
                        if selected == prev.index then
                            Result.map5 ctor
                                (arg1fns.toOutput ("0" :: name :: path) arg1)
                                (arg2fns.toOutput ("1" :: name :: path) arg2)
                                (arg3fns.toOutput ("2" :: name :: path) arg3)
                                (arg4fns.toOutput ("3" :: name :: path) arg4)
                                (arg5fns.toOutput ("4" :: name :: path) arg5)

                        else
                            prev.fromIR path value

                    _ ->
                        prev.fromIR path value
        , variantTypes =
            ( name
            , Variant5Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
                arg5fns.irType
            )
                :: prev.variantTypes
        }


{-| Complete the definition of a custom type.
-}
endCustom : CustomGadgetBuilder (a -> Value) () a -> Gadget a
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
                    CustomType IR.emptyMetadata ( "", Variant0Type ) []

                firstVariantType :: restVariantTypes ->
                    CustomType IR.emptyMetadata firstVariantType restVariantTypes
        }


{-| Start the definition of a record.
-}
record : output -> RecordGadgetBuilder input output
record ctor =
    RecordGadgetBuilder
        { fromInput = \_ -> []
        , toOutput = \_ _ -> Ok ctor
        , irType = []
        }


{-| Add a field to the definition of a record.
-}
field :
    String
    -> (input -> field)
    -> Gadget field
    -> RecordGadgetBuilder input (field -> output)
    -> RecordGadgetBuilder input output
field name getter (Gadget gadget) (RecordGadgetBuilder builder) =
    RecordGadgetBuilder
        { fromInput =
            \input ->
                let
                    thisField =
                        gadget.fromInput (getter input)

                    prevFields =
                        builder.fromInput input
                in
                ( name, thisField ) :: prevFields
        , toOutput =
            \path fields ->
                case fields of
                    ( _, thisField ) :: prevFields ->
                        multiErrorResultMap2 (\ctor val -> ctor val)
                            (builder.toOutput path prevFields)
                            (gadget.toOutput (name :: path) thisField)

                    [] ->
                        Err [ { error = "expecting a Record field", path = path } ]
        , irType =
            ( name, gadget.irType ) :: builder.irType
        }


multiErrorResultMap2 : (value -> a -> b) -> Result appendable value -> Result appendable a -> Result appendable b
multiErrorResultMap2 f r1 r2 =
    case ( r1, r2 ) of
        ( Ok a, Ok b ) ->
            Ok (f a b)

        ( Err a, Ok _ ) ->
            Err a

        ( Ok _, Err b ) ->
            Err b

        ( Err a, Err b ) ->
            Err (a ++ b)


{-| Complete the definition of a record.
-}
endRecord : RecordGadgetBuilder a a -> Gadget a
endRecord (RecordGadgetBuilder builder) =
    Gadget
        { fromInput =
            \input ->
                RecordValue (List.reverse (builder.fromInput input))
        , toOutput =
            \path value ->
                case value of
                    RecordValue fields ->
                        builder.toOutput path (List.reverse fields)

                    _ ->
                        Err [ { error = "expecting a Record", path = path } ]
        , irType = RecordType IR.emptyMetadata (List.reverse builder.irType)
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
        , toOutput = \path value -> prev.toOutput path value |> Result.map aToB
        , irType = prev.irType
        }


{-| Convert a Gadget of one type to a Gadget of another type, possibly failing
the conversion to the output type.

    import Gadget

    nonEmptyListGadget a =
        Gadget.list a
            |> Gadget.filterMap
                (\list ->
                    case list of
                        [] ->
                            Err "must contain at least one item"

                        h :: t ->
                            Ok ( h, t )
                )
                (\( h, t ) -> h :: t)

    nonEmptyListGadget --: Gadget.Gadget a -> Gadget.Gadget ( a, List a )

Do not use this for fine-grained checks (like checking for a fixed set of valid
values), as this might leave generators and fuzzers scrambling to find any valid
values. Instead, prefer a constructive approach, like this:

    import Gadget

    nonEmptyListGadget a =
        Gadget.tuple a (Gadget.list a)

    nonEmptyListGadget --: Gadget.Gadget a -> Gadget.Gadget ( a, List a )

-}
filterMap :
    (a -> Result String b)
    -> (b -> a)
    -> Gadget a
    -> Gadget b
filterMap aToB bToA (Gadget prev) =
    Gadget
        { fromInput = bToA >> prev.fromInput
        , toOutput = \path value -> prev.toOutput path value |> Result.andThen (aToB >> Result.mapError (\error -> [ { path = path, error = error } ]))
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
            |> Gadget.variant1 "Leaf" Leaf Gadget.string
            |> Gadget.variant2 "Branch" Branch
                (Gadget.lazy (\() -> treeGadget))
                (Gadget.lazy (\() -> treeGadget))
            |> Gadget.endCustom

    treeGadget --: Gadget.Gadget Tree

-}
lazy : (() -> Gadget a) -> Gadget a
lazy step =
    Gadget
        { fromInput =
            \input ->
                let
                    (Gadget gadget) =
                        step ()
                in
                gadget.fromInput input
        , toOutput =
            \path value ->
                let
                    (Gadget gadget) =
                        step ()
                in
                gadget.toOutput path value
        , irType =
            IR.LazyType IR.emptyMetadata
                (\() ->
                    let
                        (Gadget gadget) =
                            step ()
                    in
                    gadget.irType
                )
        }
