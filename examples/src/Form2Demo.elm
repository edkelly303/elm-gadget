module Form2Demo exposing (main)

import Browser
import Gadget
import Gadget.Adapter.Form2 as Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'


-}


type alias AppModel =
    { form : Form.Model }


type AppMsg
    = FormUpdatedItself (Form.Msg FormMsg)
    | QuxSentMsgToApp String


type FormMsg
    = BarSentMsgToForm String
    | AppSentMsgToForm String


type alias Foo =
    { bar : String
    , qux : String
    }



{-
    d888b   .d8b.  d8888b.  d888b  d88888b d888888b .d8888.
   88' Y8b d8' `8b 88  `8D 88' Y8b 88'     `~~88~~' 88'  YP
   88      88ooo88 88   88 88      88ooooo    88    `8bo.
   88  ooo 88~~~88 88   88 88  ooo 88~~~~~    88      `Y8b.
   88. ~8~ 88   88 88  .8D 88. ~8~ 88.        88    db   8D
    Y888P  YP   YP Y8888D'  Y888P  Y88888P    YP    `8888Y'


-}


fooGadget =
    Gadget.record Foo
        |> Gadget.field "bar" .bar barGadget
        |> Gadget.field "qux" .qux quxGadget
        |> Gadget.endRecord


barGadget =
    Gadget.string
        |> Form.label "What is a 'bar'?"
        |> Form.useControl barControl


quxGadget =
    Gadget.string
        |> Form.label "What is a 'qux'?"
        |> Form.useControl quxControl



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
    fooGadget
        |> Form.newForm FormUpdatedItself
        |> Form.registerControl barControl
        |> Form.registerControl quxControl
        |> Form.endForm



{-
    .o88b.  .d88b.  d8b   db d888888b d8888b.  .d88b.  db      .d8888.
   d8P  Y8 .8P  Y8. 888o  88 `~~88~~' 88  `8D .8P  Y8. 88      88'  YP
   8P      88    88 88V8o 88    88    88oobY' 88    88 88      `8bo.
   8b      88    88 88 V8o88    88    88`8b   88    88 88        `Y8b.
   Y8b  d8 `8b  d8' 88  V888    88    88 `88. `8b  d8' 88booo. db   8D
    `Y88P'  `Y88P'  VP   V8P    YP    88   YD  `Y88P'  Y88888P `8888Y'


-}


barControl : Form.Control FormMsg AppMsg String
barControl =
    Form.new
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ( "", Cmd.none )
        , placeholder = ""
        , load = identity
        , update =
            \controlMsg model ->
                ( controlMsg
                , send (Form.ToForm (BarSentMsgToForm controlMsg))
                )
        , notify =
            \formMsg model ->
                case formMsg of
                    AppSentMsgToForm string ->
                        string

                    _ ->
                        model
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput Form.ToSelf
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \model -> Sub.none
        , submit = Ok
        , id = "bar"
        }


quxControl : Form.Control FormMsg AppMsg String
quxControl =
    Form.new
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ( "", Cmd.none )
        , placeholder = ""
        , load = identity
        , update =
            \controlMsg model ->
                ( controlMsg
                , send (Form.ToApp (QuxSentMsgToApp controlMsg))
                )
        , notify =
            \formMsg model ->
                case formMsg of
                    BarSentMsgToForm string ->
                        string

                    _ ->
                        model
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput Form.ToSelf
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \model -> Sub.none
        , submit = Ok
        , id = "qux"
        }


send msg =
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
        , subscriptions = always Sub.none
        }


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
            ( { form = formModel }
            , formCmd
            )

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
