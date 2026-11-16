module Form2 exposing (main)

import Browser
import Gadget
import Gadget.Adapter.Form2 as Form
import Html as H


type alias Model =
    { form : Form.Model }


type Msg
    = FormUpdated Form.ControlMsg
    | Blah ()
    | OtherMsg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Foo =
    { a : Int, b : Int }


fooGadget =
    Gadget.record Foo
        |> Gadget.field "a" .a (Gadget.int |> Form.label "What is a?")
        |> Gadget.field "b" .b (Gadget.int |> Form.label "What is b?")
        |> Gadget.endRecord


form =
    Form.newForm FormUpdated Blah
        |> Form.endForm fooGadget


init flags =
    let
        ( formModel, formCmd ) =
            form.init
    in
    ( { form = formModel }
    , formCmd
    )


update msg model =
    case msg of
        FormUpdated formMsg ->
            let
                ( formModel, formCmd ) =
                    form.update formMsg model.form
            in
            ( { form = formModel }, formCmd )

        Blah () ->
            ( model, Cmd.none )

        OtherMsg ->
            ( model, Cmd.none )


view model =
    { title = "Form 2 demo"
    , body =
        [ H.main_
            []
            [ form.view model.form ]
        ]
    }
