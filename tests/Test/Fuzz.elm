module Test.Fuzz exposing (..)

import Expect
import Gadget
import Gadget.Adapter.Fuzz
import Test exposing (..)


databaseFuzzer =
    Gadget.pair
        (Gadget.list (Gadget.int |> Gadget.Adapter.Fuzz.isPrimaryKey "x"))
        (Gadget.list (Gadget.int |> Gadget.Adapter.Fuzz.hasForeignKey "x"))
        |> Gadget.Adapter.Fuzz.fuzzer


fuzzTests : Test
fuzzTests =
    Test.describe "Gadget.Adapter.Fuzz"
        [ fuzz databaseFuzzer "something" <|
            \( fst, snd ) ->
                List.all (\x -> List.member x fst) snd |> Expect.equal True
        ]
