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
        , test "filterMap works on records" <|
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
