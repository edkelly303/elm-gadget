module Form2Demo exposing (main)

import Browser
import Gadget
import Gadget.Adapter.Form2 as Form
import Html as H
import Task


type alias Model =
    { form : Form.Model }


type Msg
    = FormUpdated (Form.Msg SharedMsg)
    | OtherMsg


type SharedMsg
    = AUpdated String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Foo =
    { a : Int
    , b : Int
    }


fooGadget =
    Gadget.record Foo
        |> Gadget.field "a" .a aGadget
        |> Gadget.field "b" .b bGadget
        |> Gadget.endRecord


aGadget =
    Gadget.int
        |> Form.label "What is a?"
        |> Form.override "a"


bGadget =
    Gadget.int
        |> Form.label "What is b?"
        |> Form.override "b"


form =
    Form.newForm FormUpdated
        |> Form.withOverride "a"
            (Form.intControl
                |> Form.control_update
                    (\controlMsg model ->
                        ( controlMsg
                        , Task.perform (\() -> Form.ToForm (AUpdated controlMsg)) (Task.succeed ())
                        )
                    )
            )
        |> Form.withOverride "b"
            (Form.intControl
                |> Form.control_notify
                    (\formMsg model ->
                        case formMsg of
                            AUpdated s ->
                                s
                    )
            )
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
