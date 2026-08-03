module Test.JsonTest exposing (..)

import Expect
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Json
import Gadget.IR
import Json.Decode
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.Json"
        [ roundTrip recordGadget "Record"
        , roundTrip treeGadget "Tree"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.IR.withMetadata "String" (Gadget.IR.String "")) "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip gadget name =
    fuzz
        (Gadget.Adapter.Fuzz.fuzzer gadget)
        (name ++ " encode -> decode roundtrip")
    <|
        \value ->
            let
                encoded =
                    Gadget.Adapter.Json.encode gadget value
            in
            Json.Decode.decodeValue (Gadget.Adapter.Json.decoder gadget) encoded
                |> Expect.equal (Ok value)
