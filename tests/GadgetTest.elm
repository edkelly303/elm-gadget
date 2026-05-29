module GadgetTest exposing (..)

import Expect
import Fuzz
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.IR
import Test exposing (..)
import TestHelpers exposing (..)


irTests : Test
irTests =
    Test.describe "IR"
        [ roundTrip recordCodec "Record"
        , roundTrip treeGadget "Tree (recursive custom type)"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.label "fuzz-override") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget input -> String -> Test
roundTrip codec name =
    fuzz
        (Gadget.Adapter.Fuzz.fuzzerWithOverrides
            [ Gadget.Adapter.Fuzz.override "fuzz-override" Gadget.string (Fuzz.stringOfLengthBetween 0 6)
            ]
            codec
        )
        (name ++ " fromInput -> toOutput roundtrip")
    <|
        \rec ->
            rec
                |> Gadget.IR.fromInput codec
                |> Gadget.IR.toOutput codec
                |> Expect.equal (Ok rec)
