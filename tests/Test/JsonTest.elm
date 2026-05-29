module Test.JsonTest exposing (..)

import Expect
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Json
import Json.Decode
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.Json"
        [ roundTrip recordGadget "Record"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.label "string") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip codec name =
    fuzz
        (Gadget.Adapter.Fuzz.fuzzer codec)
        (name ++ " encode -> decode roundtrip")
    <|
        \value ->
            let
                encoded =
                    Gadget.Adapter.Json.encode codec value
            in
            Json.Decode.decodeValue (Gadget.Adapter.Json.decoder codec) encoded
                |> Expect.equal (Ok value)
