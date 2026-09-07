module Test.FormTest exposing (suite)

import Expect
import Gadget as G
import Gadget.Adapter.Form as F
import Gadget.Adapter.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


type alias Foo =
    { bar : Int, baz : String }


g =
    G.record Foo
        |> G.field "bar" .bar G.int
        |> G.field "baz"
            .baz
            (G.string |> F.validate (\_ -> Err "This error should show up"))
        |> G.endRecord
        |> F.validate (\_ -> Err "This error shouldn't show up")


form : F.Form Foo F.Msg
form =
    F.fromGadget identity g


suite : Test
suite =
    describe "Form"
        [ test "When a filterMapped gadget fails to submit, errors should still be returned on gadgets that are not its direct ancestors" <|
            \() ->
                let
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
        , test "When a filterMapped gadget fails to submit, errors on its direct ancestors should not be returned" <|
            \() ->
                let
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
        ]
