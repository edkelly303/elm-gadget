module Test.FormTest exposing (suite)

import Expect
import Gadget as G
import Gadget.Adapter.Form as F
import Gadget.Adapter.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


type alias Foo =
    { bar : Int, baz : String }


suite : Test
suite =
    describe "Form"
        [ test """When a gadget fails to submit due to a parsing failure, errors should still be 
  returned on gadgets that are not its direct ancestors""" <|
            \() ->
                let
                    g =
                        G.record Foo
                            |> G.field "bar" .bar G.int
                            -- `bar` will fail on submission, because its initial
                            -- stats is `""`, and that won't parse as an integer
                            |> G.field "baz" .baz (G.string |> F.validate (\_ -> Err "This error should show up"))
                            -- `baz` is a sibling, not a direct ancestor of `bar`
                            |> G.endRecord

                    form =
                        F.fromGadget identity g

                    errors =
                        form.init
                            |> form.submit
                in
                errors
                    |> Expect.equal
                        (Err
                            [ { error = "Not an integer", path = [ "bar" ] }
                            , { error = "This error should show up", path = [ "baz" ] }
                            ]
                        )
        , test """When a gadget fails to submit due to a parsing failure, errors on its direct 
  ancestors should not be returned""" <|
            \() ->
                let
                    g =
                        G.record Foo
                            |> G.field "bar" .bar G.int
                            -- `bar` will fail on submission, because its initial
                            -- stats is `""`, and that won't parse as an integer
                            |> G.field "baz"
                                .baz
                                G.string
                            |> G.endRecord
                            |> F.validate (\_ -> Err "This error shouldn't show up")

                    -- this error is on the root gadget, which is a direct ancestor of `bar`
                    form =
                        F.fromGadget identity g

                    errors =
                        form.init
                            |> form.submit
                in
                errors
                    |> Expect.equal
                        (Err
                            [ { error = "Not an integer", path = [ "bar" ] }
                            ]
                        )
        ]
