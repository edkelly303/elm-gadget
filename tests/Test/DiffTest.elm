module Test.DiffTest exposing (..)

import Expect
import Gadget
import Gadget.Adapter.Diff
import Gadget.Adapter.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.Diff"
        [ roundTrip recordGadget "Record"
        , roundTrip treeGadget "Tree"
        , roundTrip (Gadget.int |> Gadget.Adapter.Fuzz.useOverride "int") "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip Gadget.string "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip gadget name =
    fuzz2
        (Gadget.Adapter.Fuzz.fuzzer gadget)
        (Gadget.Adapter.Fuzz.fuzzer gadget)
        (name ++ " diff -> patch roundtrip")
    <|
        \old new ->
            let
                diff =
                    Gadget.Adapter.Diff.diff gadget old new

                expectation =
                    Gadget.Adapter.Diff.patch gadget diff old
                        |> Expect.equal (Ok new)
            in
            expectation
