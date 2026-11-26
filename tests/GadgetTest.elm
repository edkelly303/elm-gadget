module GadgetTest exposing (..)

import Expect
import Fuzz
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.IR
import Test exposing (..)
import TestHelpers exposing (..)


gadgetTests : Test
gadgetTests =
    Test.describe "Gadget"
        [ roundTrip complexRecordGadget "Record"
        , roundTrip treeGadget "Tree (recursive custom type)"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.Adapter.Fuzz.useOverride "override") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        , filterMapTest ()
        , duplicateFieldNameTest ()
        , duplicateVariantNameTest ()
        , invalidFieldNameTest ()
        , invalidVariantNameTest ()
        ]


filterMapTest : () -> Test
filterMapTest () =
    test "filterMap works on records" <|
        \() ->
            let
                fmr =
                    recordGadget
                        |> Gadget.filterMap (\_ -> Err [ "filterMapping failed" ]) identity
            in
            { bool = True, int = 1 }
                |> Gadget.IR.fromInput fmr
                |> Gadget.IR.toOutput fmr
                |> Expect.err


duplicateFieldNameTest : () -> Test
duplicateFieldNameTest () =
    test "records can't have duplicate field names" <|
        \() ->
            let
                g =
                    Gadget.record (\a b -> { a = a, b = b })
                        |> Gadget.field "a" .a Gadget.unit
                        |> Gadget.field "a" .b Gadget.unit
                        |> Gadget.endRecord
            in
            { a = (), b = () }
                |> Gadget.IR.fromInput g
                |> Gadget.IR.toOutput g
                |> Expect.err


duplicateVariantNameTest : () -> Test
duplicateVariantNameTest () =
    test "custom types can't have duplicate variant names" <|
        \() ->
            let
                g =
                    Gadget.custom
                        (\a b v ->
                            case v of
                                A ->
                                    a

                                B ->
                                    b
                        )
                        |> Gadget.variant0 "A" A
                        |> Gadget.variant0 "A" B
                        |> Gadget.endCustom
            in
            A
                |> Gadget.IR.fromInput g
                |> Gadget.IR.toOutput g
                |> Expect.err


invalidFieldNameTest : () -> Test
invalidFieldNameTest () =
    let
        t testName fieldName =
            f testName (Fuzz.constant fieldName)

        f testName fieldFuzzer =
            fuzz fieldFuzzer testName <|
                \fieldName ->
                    let
                        g =
                            Gadget.record (\a -> { a = a })
                                |> Gadget.field fieldName .a Gadget.unit
                                |> Gadget.endRecord
                    in
                    { a = () }
                        |> Gadget.IR.fromInput g
                        |> Gadget.IR.toOutput g
                        |> Expect.err
    in
    describe "record field names"
        [ t "cannot be an empty string" ""
        , t "cannot start with a number" "1"
        , t "cannot start with an upper case letter" "A"
        , f "cannot contain non-alphanumeric characters, except '_'" (Fuzz.char |> Fuzz.filter (\c -> not (Char.isAlphaNum c) || c == '_') |> Fuzz.map String.fromChar)
        ]


type A
    = A
    | B


invalidVariantNameTest : () -> Test
invalidVariantNameTest () =
    let
        t testName fieldName =
            f testName (Fuzz.constant fieldName)

        f testName fieldFuzzer =
            fuzz fieldFuzzer testName <|
                \fieldName ->
                    let
                        g =
                            Gadget.custom
                                (\a b v ->
                                    case v of
                                        A ->
                                            a

                                        B ->
                                            b
                                )
                                |> Gadget.variant0 fieldName A
                                |> Gadget.variant0 "B" B
                                |> Gadget.endCustom
                    in
                    A
                        |> Gadget.IR.fromInput g
                        |> Gadget.IR.toOutput g
                        |> Expect.err
    in
    describe "custom type variant names"
        [ t "cannot be an empty string" ""
        , t "cannot start with a number" "1"
        , t "cannot start with a lower case letter" "a"
        , f "cannot contain non-alphanumeric characters, except '_'" (Fuzz.char |> Fuzz.filter (\c -> not (Char.isAlphaNum c) || c == '_') |> Fuzz.map String.fromChar)
        ]


roundTrip : Gadget.Gadget input -> String -> Test
roundTrip gadget name =
    fuzz
        (Gadget.Adapter.Fuzz.fuzzerWithOverrides
            [ Gadget.Adapter.Fuzz.override "override" Gadget.string (Fuzz.stringOfLengthBetween 0 6)
            ]
            gadget
        )
        (name ++ " fromInput -> toOutput roundtrip")
    <|
        \rec ->
            rec
                |> Gadget.IR.fromInput gadget
                |> Gadget.IR.toOutput gadget
                |> Expect.equal (Ok rec)
