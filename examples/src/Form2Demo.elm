module Form2Demo exposing (main)

import Browser
import Gadget
import Gadget.Adapter.Form as Form
import Gadget.Adapter.Form.Control as Control
import Gadget.IR
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task
import Time



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'


-}


type alias AppModel =
    { form : Form.Model
    , output : Result (List Gadget.IR.Error) Foo
    }


type AppMsg
    = FormUpdatedItself (Form.Msg FormMsg)
    | FormSubmitted
    | QuxSentMsgToApp String


type FormMsg
    = BarSentMsgToForm String
    | AppSentMsgToForm String


type alias Foo =
    { qux : Int
    , zod : Maybe String
    }



{-
   d88888b  .d88b.  d8888b. .88b  d88.
   88'     .8P  Y8. 88  `8D 88'YbdP`88
   88ooo   88    88 88oobY' 88  88  88
   88~~~   88    88 88`8b   88  88  88
   88      `8b  d8' 88 `88. 88  88  88
   YP       `Y88P'  88   YD YP  YP  YP


-}


form : Form.Form FormMsg AppMsg Foo
form =
    Form.fromControl FormUpdatedItself fooControl



{-
    .o88b.  .d88b.  d8b   db d888888b d8888b.  .d88b.  db      .d8888.
   d8P  Y8 .8P  Y8. 888o  88 `~~88~~' 88  `8D .8P  Y8. 88      88'  YP
   8P      88    88 88V8o 88    88    88oobY' 88    88 88      `8bo.
   8b      88    88 88 V8o88    88    88`8b   88    88 88        `Y8b.
   Y8b  d8 `8b  d8' 88  V888    88    88 `88. `8b  d8' 88booo. db   8D
    `Y88P'  `Y88P'  VP   V8P    YP    88   YD  `Y88P'  Y88888P `8888Y'


-}


fooControl =
    Control.record (\qux zod _ -> Foo qux zod)
        |> Control.field "qux" .qux quxControl
        |> Control.field "zod" .zod zodControl
        |> Control.field "clock" (always 0) clockControl
        |> Control.endRecord
        |> Control.label "I am a record control. I produce records of type `Foo`"


zodControl =
    Control.custom
        (\just nothing variant ->
            case variant of
                Just s ->
                    just s

                Nothing ->
                    nothing
        )
        |> Control.variant1 "Just" Just barControl
        |> Control.variant0 "Nothing" Nothing
        |> Control.endCustom
        |> Control.label "I am a `Maybe Int` control"


clockControl =
    Control.element
        { modelGadget = Gadget.int
        , msgGadget = Gadget.unit
        , outputGadget = Gadget.int
        , id = "clock"
        , init = ( 0, Cmd.none )
        , load = identity
        , update = \() model -> ( model + 1, Cmd.none )
        , view = \id model -> H.text ("Time elapsed: " ++ String.fromInt model ++ " seconds")
        , subscriptions = \model -> Time.every 1000 (always ())
        , submit = Ok
        }
        |> Control.label "I am a `()` control. I subscribe to `Time.every`"


barControl : Control.Control FormMsg AppMsg String
barControl =
    Control.advanced
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ( "", Cmd.none )
        , load = identity
        , update =
            \msg model ->
                ( msg
                , sendMsg (Control.routeToForm (BarSentMsgToForm msg))
                )
        , notify =
            \formMsg model ->
                case formMsg of
                    AppSentMsgToForm string ->
                        string

                    _ ->
                        model
        , view = textInput
        , subscriptions = always Sub.none
        , submit = Ok
        , id = "bar"
        }
        |> Control.label "I am a `String` control, with special powers! I can send and receive `FormMsg` values"


quxControl : Control.Control FormMsg AppMsg Int
quxControl =
    Control.advanced
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.int
        , init = ( "", Cmd.none )
        , load = String.fromInt
        , update =
            \msg model ->
                ( msg
                , sendMsg (Control.routeToApp (QuxSentMsgToApp msg))
                )
        , notify =
            \formMsg model ->
                case formMsg of
                    BarSentMsgToForm string ->
                        string

                    _ ->
                        model
        , view = textInput
        , subscriptions = always Sub.none
        , submit = \model -> String.toInt model |> Result.fromMaybe "⚠️ This must be an integer"
        , id = "qux"
        }
        |> Control.label "I am an `Int` control, with special powers! I can send `AppMsg` values and receive `FormMsg` values"


textInput : String -> String -> H.Html (Control.Router String FormMsg AppMsg)
textInput id model =
    H.input
        [ HA.type_ "text"
        , HE.onInput Control.routeToSelf
        , HA.id id
        , HA.value model
        ]
        []


sendMsg : Control.Router String FormMsg AppMsg -> Cmd (Control.Router String FormMsg AppMsg)
sendMsg msg =
    Task.perform identity (Task.succeed msg)



{-
   .88b  d88.  .d8b.  d888888b d8b   db
   88'YbdP`88 d8' `8b   `88'   888o  88
   88  88  88 88ooo88    88    88V8o 88
   88  88  88 88~~~88    88    88 V8o88
   88  88  88 88   88   .88.   88  V888
   YP  YP  YP YP   YP Y888888P VP   V8P


-}


main : Program () AppModel AppMsg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( AppModel, Cmd AppMsg )
init flags =
    let
        ( formModel0, formCmd ) =
            Form.init form

        formModel =
            Form.load form
                { qux = 100
                , zod = Just "hello"
                }
                |> Result.withDefault formModel0
    in
    ( { form = formModel, output = Err [] }
    , formCmd
    )


update : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
update msg model =
    case msg of
        FormUpdatedItself formMsg ->
            let
                ( formModel, formCmd ) =
                    Form.update form formMsg model.form
            in
            ( { model | form = formModel }
            , formCmd
            )

        FormSubmitted ->
            ( { model | output = Form.submit form model.form }
            , Cmd.none
            )

        QuxSentMsgToApp string ->
            let
                formModel =
                    Form.notify form (AppSentMsgToForm string) model.form
            in
            ( { model | form = formModel }
            , Cmd.none
            )


view : AppModel -> Browser.Document AppMsg
view model =
    { title = "Form 2 demo"
    , body =
        [ H.main_ []
            [ Form.view form model.form
            , H.button [ HE.onClick FormSubmitted ] [ H.text "Submit!" ]
            , H.h2 [] [ H.text "Form output" ]
            , H.output [] [ H.text (Debug.toString model.output) ]
            ]
        ]
    }


subscriptions model =
    Form.subscriptions form model.form
