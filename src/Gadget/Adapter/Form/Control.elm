module Gadget.Adapter.Form.Control exposing
    ( Control(..), sandbox, element, define, withLayout
    , Command, noCommand, toCommand, sendCommand
    )

{-|

@docs Control, sandbox, element, define, withLayout
@docs Command, noCommand, toCommand, sendCommand

-}

import Gadget
import Gadget.Adapter.Form.Internal as Internal
import Gadget.IR as IR
import Html as H
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


sandbox :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : model
    , load : output -> model
    , update : msg -> model -> model
    , view : String -> model -> H.Html msg
    , submit : model -> Result String output
    }
    -> Control backendModel output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    define
        { placeholder = placeholder
        , init = ( init, noCommand )
        , load = load
        , update = \msg model -> ( update msg model, noCommand )
        , view = view
        , subscriptions = \_ -> Sub.none
        , submit = submit
        , frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , updateFromBackend = \_ model -> ( model, noCommand )
        }


element :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Cmd msg )
    , load : output -> model
    , update : msg -> model -> ( model, Cmd msg )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    }
    -> Control backendModel output
element { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    define
        { placeholder = placeholder
        , init = init |> Tuple.mapSecond toCommand
        , load = load
        , update = \msg model -> update msg model |> Tuple.mapSecond toCommand
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , respond = \_ _ -> ()
        , updateFromBackend = \_ model -> ( model, noCommand )
        }


{-| Turn a `Definition` into a `Control`.
-}
define :
    { frontendModelGadget : IR.Gadget frontendModel
    , frontendMsgGadget : IR.Gadget frontendMsg
    , outputGadget : IR.Gadget output
    , toBackendGadget : IR.Gadget toBackend
    , toFrontendGadget : IR.Gadget toFrontend
    , placeholder : output
    , init : ( frontendModel, Command frontendMsg toBackend )
    , load : output -> frontendModel
    , update : frontendMsg -> frontendModel -> ( frontendModel, Command frontendMsg toBackend )
    , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Command frontendMsg toBackend )
    , view : String -> frontendModel -> H.Html frontendMsg
    , subscriptions : frontendModel -> Sub frontendMsg
    , submit : frontendModel -> Result String output
    , respond : toBackend -> backendModel -> toFrontend
    }
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
