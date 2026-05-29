module Test.StringTest exposing (..)

import Expect
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.Adapter.String
import Parser
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.String"
        [ roundTrip recordGadget "Record"
        , roundTrip treeGadget "Tree"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.label "String") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip gadget name =
    fuzz
        (Gadget.Adapter.Fuzz.fuzzer gadget)
        (name ++ " print -> parse roundtrip")
    <|
        \val ->
            let
                printed =
                    Gadget.Adapter.String.print gadget val
            in
            Parser.run (Gadget.Adapter.String.parser gadget) printed
                |> Expect.equal (Ok val)
