module Gadget.Adapter.Form.Control exposing
    ( Command
    , Control(..)
    , Definition
    , bool
    , char
    , define
    , float
    , int
    , noCommand
    , sendCommand
    , string
    , toCommand
    )

import Gadget
import Gadget.Adapter.Form.Internal as Internal
import Gadget.IR as IR
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra


type alias Command frontendMsg toBackend =
    Internal.Command frontendMsg toBackend


type alias UI =
    Internal.UI


noCommand : Command frontendMsg toBackend
noCommand =
    Internal.Command []


toCommand : Cmd frontendMsg -> Command frontendMsg toBackend
toCommand cmd =
    Internal.Command [ Internal.Cmd cmd ]


sendCommand : toBackend -> Command frontendMsg toBackend
sendCommand toBackend =
    Internal.Command [ Internal.ToBackend toBackend ]


{-| A custom form control.
-}
type Control backendModel output
    = Control (IR.Gadget backendModel -> Internal.InnerControl)


{-| A definition for a custom form control.
-}
type alias Definition toBackend toFrontend backendModel frontendMsg frontendModel output =
    { frontendMsgGadget : IR.Gadget frontendMsg
    , frontendModelGadget : IR.Gadget frontendModel
    , outputGadget : IR.Gadget output
    , toBackendGadget : IR.Gadget toBackend
    , toFrontendGadget : IR.Gadget toFrontend
    , init : ( frontendModel, Command frontendMsg toBackend )
    , placeholder : output
    , load : output -> frontendModel
    , update : frontendMsg -> frontendModel -> ( frontendModel, Command frontendMsg toBackend )
    , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Command frontendMsg toBackend )
    , view : String -> frontendModel -> H.Html frontendMsg
    , subscriptions : frontendModel -> Sub frontendMsg
    , submit : frontendModel -> Result String output
    , respond : toBackend -> backendModel -> toFrontend
    }


{-| Turn a `Definition` into a `Control`.
-}
define :
    Definition toBackend toFrontend backendModel frontendMsg frontendModel output
    -> Control backendModel output
define config =
    let
        placeholderValue =
            config.placeholder
                |> config.load
                |> IR.fromInput config.frontendModelGadget

        mapCmdType =
            Internal.mapCommand
                (IR.fromInput config.frontendMsgGadget)
                (IR.fromInput config.toBackendGadget)
    in
    Control <|
        \backendModelGadget ->
            { init =
                let
                    ( model, cmdType ) =
                        config.init
                in
                ( IR.fromInput config.frontendModelGadget model
                , mapCmdType cmdType
                )
            , load =
                \outputValue ->
                    IR.toOutput config.outputGadget outputValue
                        |> Result.map (\output -> config.load output)
                        |> Result.map (IR.fromInput config.frontendModelGadget)
                        |> Result.withDefault placeholderValue
            , placeholder = IR.fromInput config.outputGadget config.placeholder
            , update =
                \msg modelValue ->
                    let
                        result =
                            Result.map2 config.update
                                (IR.toOutput config.frontendMsgGadget msg)
                                (IR.toOutput config.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, either ) ->
                            ( IR.fromInput config.frontendModelGadget model
                            , mapCmdType either
                            )

                        Err _ ->
                            ( modelValue
                            , noCommand
                            )
            , updateFromBackend =
                \msg modelValue ->
                    let
                        result =
                            Result.map2 config.updateFromBackend
                                (IR.toOutput config.toFrontendGadget msg)
                                (IR.toOutput config.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, either ) ->
                            ( IR.fromInput config.frontendModelGadget model
                            , mapCmdType either
                            )

                        Err _ ->
                            ( modelValue
                            , noCommand
                            )
            , view =
                \id modelValue ->
                    Result.map (config.view id) (IR.toOutput config.frontendModelGadget modelValue)
                        |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                        |> H.map (\msg -> IR.fromInput config.frontendMsgGadget msg)
            , layout =
                [ Internal.Label, Internal.Input, Internal.Feedback ]
            , subscriptions = \_ -> Sub.none
            , submit =
                \path modelValue ->
                    IR.toOutput config.frontendModelGadget modelValue
                        |> Result.andThen
                            (\model ->
                                config.submit model
                                    |> Result.mapError (\error -> [ { error = error, path = path } ])
                                    |> Result.map (IR.fromInput config.outputGadget)
                            )
            , respond =
                \toBackend backendModel ->
                    Result.map2 config.respond
                        (IR.toOutput config.toBackendGadget toBackend)
                        (IR.toOutput backendModelGadget backendModel)
                        |> Result.map (IR.fromInput config.toFrontendGadget)
                        |> Result.withDefault IR.UnitValue
            }


int : Control backendModel Int
int =
    define
        { frontendModelGadget = Gadget.string
        , frontendMsgGadget = Gadget.string
        , outputGadget = Gadget.int
        , init = ( "", noCommand )
        , placeholder = 0
        , load = String.fromInt
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , update = \msg _ -> ( msg, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
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
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "This must be an integer"
        }


float : Control backendModel Float
float =
    define
        { frontendModelGadget = Gadget.string
        , frontendMsgGadget = Gadget.string
        , outputGadget = Gadget.float
        , init = ( "", noCommand )
        , placeholder = 0.0
        , load = String.fromFloat
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , update = \msg _ -> ( msg, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
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
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.toFloat model
                    |> Result.fromMaybe "This must be a decimal number"
        }


string : Control backendModel String
string =
    define
        { frontendModelGadget = Gadget.string
        , frontendMsgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ( "", noCommand )
        , placeholder = ""
        , load = identity
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , update = \msg _ -> ( msg, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit = Ok
        }


bool : Control backendModel Bool
bool =
    define
        { frontendModelGadget = Gadget.bool
        , frontendMsgGadget = Gadget.bool
        , outputGadget = Gadget.bool
        , init = ( False, noCommand )
        , placeholder = False
        , load = identity
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , update = \msg _ -> ( msg, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "checkbox"
                    , HE.onCheck identity
                    , HA.checked model
                    , HA.id id
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit = Ok
        }
        |> withLayout (\ui -> [ ui.input, ui.label, ui.feedback ])


char : Control backendModel Char
char =
    define
        { frontendModelGadget = Gadget.string
        , frontendMsgGadget = Gadget.maybe Gadget.char
        , outputGadget = Gadget.char
        , init = ( "", noCommand )
        , placeholder = 'a'
        , load = String.fromChar
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , update =
            \msg _ ->
                case msg of
                    Nothing ->
                        ( "", noCommand )

                    Just c ->
                        ( String.fromChar c, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput (\str -> String.uncons str |> Maybe.map Tuple.first)
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.uncons model
                    |> Maybe.map Tuple.first
                    |> Result.fromMaybe "This must not be blank"
        }


withLayout :
    ({ label : UI, input : UI, feedback : UI } -> List UI)
    -> Control backendModel output
    -> Control backendModel output
withLayout f (Control toControl) =
    Control <|
        \backendModelGadget ->
            let
                c =
                    toControl backendModelGadget
            in
            { c
                | layout =
                    f { label = Internal.Label, input = Internal.Input, feedback = Internal.Feedback }
                        |> List.Extra.unique
            }
