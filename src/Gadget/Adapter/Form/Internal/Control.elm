module Gadget.Adapter.Form.Internal.Control exposing
    ( Send
    , bool
    , char
    , element
    , float
    , int
    , new
    , sandbox
    , string
    , toApp
    , toForm
    , toSelf
    , withLayout
    )

import Gadget
import Gadget.Adapter.Default as Default
import Gadget.Adapter.Form.Internal.Types as Internal exposing (InternalControl(..), UI(..))
import Gadget.IR as IR
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra


type alias Send controlMsg formMsg appMsg =
    Internal.Router controlMsg formMsg appMsg


toSelf : controlMsg -> Send controlMsg formMsg appMsg
toSelf =
    Internal.RouteToSelf


toForm : formMsg -> Send controlMsg formMsg appMsg
toForm =
    Internal.RouteToForm


toApp : appMsg -> Send controlMsg formMsg appMsg
toApp =
    Internal.RouteToApp


sandbox :
    { init : model
    , load : output -> model
    , update : controlMsg -> model -> model
    , view : String -> model -> H.Html controlMsg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , id : String
    }
    -> InternalControl formMsg appMsg output
sandbox args =
    new
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , init = ( args.init, Cmd.none )
        , load = args.load
        , update = \msg model -> ( args.update msg model, Cmd.none )
        , notify = \_ model -> model
        , view = \id model -> args.view id model |> H.map Internal.RouteToSelf
        , subscriptions = \_ -> Sub.none
        , submit = args.submit
        , id = args.id
        }


element :
    { init : ( model, Cmd msg )
    , load : output -> model
    , update : msg -> model -> ( model, Cmd msg )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , id : String
    }
    -> InternalControl formMsg appMsg output
element args =
    new
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , init = args.init |> Tuple.mapSecond (Cmd.map Internal.RouteToSelf)
        , load = args.load
        , update = \msg model -> args.update msg model |> Tuple.mapSecond (Cmd.map Internal.RouteToSelf)
        , notify = \_ model -> model
        , view = \id model -> args.view id model |> H.map Internal.RouteToSelf
        , subscriptions = \model -> args.subscriptions model |> Sub.map Internal.RouteToSelf
        , submit = args.submit
        , id = args.id
        }


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , init : ( model, Cmd (Send controlMsg formMsg appMsg) )
    , load : output -> model
    , update : controlMsg -> model -> ( model, Cmd (Send controlMsg formMsg appMsg) )
    , notify : formMsg -> model -> model
    , view : String -> model -> H.Html (Send controlMsg formMsg appMsg)
    , subscriptions : model -> Sub (Send controlMsg formMsg appMsg)
    , submit : model -> Result String output
    , id : String
    }
    -> InternalControl formMsg appMsg output
new builder =
    let
        controlMsgToValue controlMsg =
            case controlMsg of
                Internal.RouteToSelf m ->
                    Internal.RouteToSelf (IR.fromInput builder.msgGadget m)

                Internal.RouteToForm m ->
                    Internal.RouteToForm m

                Internal.RouteToApp m ->
                    Internal.RouteToApp m
    in
    InternalControl builder.id
        { init =
            let
                ( model, command ) =
                    builder.init
            in
            ( IR.fromInput builder.modelGadget model
            , command |> Cmd.map controlMsgToValue
            )
        , load =
            \outputValue ->
                IR.toOutput builder.outputGadget outputValue
                    |> Result.map builder.load
                    |> Result.map (IR.fromInput builder.modelGadget)
        , dummyOutputValue = Default.forGadget builder.outputGadget
        , update =
            \controlMsg modelValue ->
                let
                    result =
                        Result.map2 builder.update
                            (IR.toOutput builder.msgGadget controlMsg)
                            (IR.toOutput builder.modelGadget modelValue)
                in
                case result of
                    Ok ( newModelValue, command ) ->
                        ( IR.fromInput builder.modelGadget newModelValue
                        , command |> Cmd.map controlMsgToValue
                        )

                    Err _ ->
                        ( modelValue
                        , Cmd.none
                        )
        , notify =
            \formMsg modelValue ->
                IR.toOutput builder.modelGadget modelValue
                    |> Result.map (builder.notify formMsg)
                    |> Result.map (IR.fromInput builder.modelGadget)
                    |> Result.withDefault modelValue
        , view =
            \id modelValue ->
                Result.map (builder.view id) (IR.toOutput builder.modelGadget modelValue)
                    |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                    |> H.map controlMsgToValue
        , layout =
            [ Label, Input, Feedback ]
        , subscriptions =
            \modelValue ->
                IR.toOutput builder.modelGadget modelValue
                    |> Result.map builder.subscriptions
                    |> Result.withDefault Sub.none
                    |> Sub.map controlMsgToValue
        , submit =
            \path modelValue ->
                IR.toOutput builder.modelGadget modelValue
                    |> Result.andThen
                        (\model ->
                            builder.submit model
                                |> Result.mapError (\error -> [ { error = error, path = path } ])
                                |> Result.map (IR.fromInput builder.outputGadget)
                        )
        }


withLayout : ({ label : UI, input : UI, feedback : UI } -> List UI) -> InternalControl formMsg appMsg output -> InternalControl formMsg appMsg a
withLayout f (InternalControl id builder) =
    InternalControl id
        { builder
            | layout =
                f { label = Label, input = Input, feedback = Feedback }
                    |> List.Extra.unique
        }


int : InternalControl formMsg appMsg Int
int =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.int
        , init = ""
        , load = String.fromInt
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "number"
                    , HA.attribute "inputmode" "numeric"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "This must be an integer"
        , id = "int"
        }


float : InternalControl formMsg appMsg Float
float =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.float
        , init = ""
        , load = String.fromFloat
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "number"
                    , HA.attribute "inputmode" "decimal"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.toFloat model
                    |> Result.fromMaybe "This must be a decimal number"
        , id = "float"
        }


string : InternalControl formMsg appMsg String
string =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ""
        , load = identity
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit = Ok
        , id = "string"
        }


bool : InternalControl formMsg appMsg output
bool =
    sandbox
        { modelGadget = Gadget.bool
        , msgGadget = Gadget.bool
        , outputGadget = Gadget.bool
        , init = False
        , load = identity
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "checkbox"
                    , HE.onCheck identity
                    , HA.checked model
                    , HA.id id
                    ]
                    []
        , submit = Ok
        , id = "bool"
        }
        |> withLayout (\ui -> [ ui.input, ui.label, ui.feedback ])


char : InternalControl formMsg appMsg Char
char =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.maybe Gadget.char
        , outputGadget = Gadget.char
        , init = ""
        , load = String.fromChar
        , update =
            \msg _ ->
                case msg of
                    Nothing ->
                        ""

                    Just c ->
                        String.fromChar c
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput (\str -> String.uncons str |> Maybe.map Tuple.first)
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.uncons model
                    |> Maybe.map Tuple.first
                    |> Result.fromMaybe "This must not be blank"
        , id = "char"
        }
