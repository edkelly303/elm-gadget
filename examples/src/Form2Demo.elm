module Form2Demo exposing (main)

import Browser
import Gadget
import Gadget.Adapter.Form2 as Form
import Html as H
import Task


type alias Foo =
    { bar : Int
    , qux : Int
    }


fooGadget =
    Gadget.record Foo
        |> Gadget.field "bar" .bar barGadget
        |> Gadget.field "qux" .qux quxGadget
        |> Gadget.endRecord


barGadget =
    Gadget.int
        |> Form.label "What number is bar?"
        |> Form.useControl barControl


quxGadget =
    Gadget.int
        |> Form.label "What number is qux?"
        |> Form.useControl quxControl


type alias AppModel =
    { form : Form.Model }


type AppMsg
    = FormUpdatedItself (Form.Msg FormMsg)
    | QuxSentMsgToApp String


type FormMsg
    = BarSentMsgToForm String
    | AppSentMsgToForm String


main : Program () AppModel AppMsg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


form =
    Form.newForm FormUpdatedItself
        |> Form.withOverride barControl
        |> Form.withOverride quxControl
        |> Form.endForm fooGadget


barControl =
    Form.intControl
        |> Form.control_update Gadget.string
            Gadget.string
            (\controlMsg model ->
                ( controlMsg
                , send (Form.ToForm (BarSentMsgToForm controlMsg))
                )
            )
        |> Form.control_notify Gadget.string
            (\formMsg model ->
                case formMsg of
                    AppSentMsgToForm string ->
                        string

                    _ ->
                        model
            )


quxControl =
    Form.intControl
        |> Form.control_update Gadget.string
            Gadget.string
            (\controlMsg model ->
                ( controlMsg
                , send (Form.ToApp (QuxSentMsgToApp controlMsg))
                )
            )
        |> Form.control_notify Gadget.string
            (\formMsg model ->
                case formMsg of
                    BarSentMsgToForm string ->
                        string

                    _ ->
                        model
            )


send msg =
    Task.perform identity (Task.succeed msg)


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
        FormUpdatedItself formMsg ->
            let
                ( formModel, formCmd ) =
                    form.update formMsg model.form
            in
            ( { form = formModel }, formCmd )

        QuxSentMsgToApp string ->
            let
                formModel =
                    form.notify (AppSentMsgToForm string) model.form
            in
            ( { form = formModel }
            , Cmd.none
            )


view model =
    { title = "Form 2 demo"
    , body =
        [ H.main_ []
            [ form.view model.form ]
        ]
    }
