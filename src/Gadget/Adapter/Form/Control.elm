module Gadget.Adapter.Form.Control exposing
    ( Control(..), sandbox, element, transmitter, transceiver, define, withLayout
    , Command, noCommand, toCommand, request, broadcast
    )

{-|

@docs Control, sandbox, element, transmitter, transceiver, define, withLayout
@docs Command, noCommand, toCommand, request, broadcast

-}

import Gadget
import Gadget.Adapter.Form.Internal as Internal
import Gadget.IR as IR
import Html as H
import List.Extra
import Result.Extra


type alias Command frontendMsg broadcastMsg toBackend =
    Internal.Command frontendMsg broadcastMsg toBackend


type alias UI =
    Internal.UI


noCommand : Command frontendMsg broadcastMsg toBackend
noCommand =
    Internal.Command []


toCommand : Cmd frontendMsg -> Command frontendMsg broadcastMsg toBackend
toCommand cmd =
    Internal.Command [ Internal.Cmd cmd ]


request : toBackend -> Command frontendMsg broadcastMsg toBackend
request toBackend =
    Internal.Command [ Internal.ToBackend toBackend ]


broadcast : broadcastMsg -> Command frontendMsg broadcastMsg toBackend
broadcast broadcastMsg =
    Internal.Command [ Internal.Broadcast broadcastMsg ]


{-| A custom form control.
-}
type Control backendModel broadcastMsg output
    = Control (IR.Gadget backendModel -> Internal.InnerControl broadcastMsg)


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
    -> Control backendModel broadcastMsg output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    define
        { frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.unit
        , placeholder = placeholder
        , init = ( init, noCommand )
        , load = load
        , update = \msg model -> ( update msg model, noCommand )
        , updateFromBackend = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = \_ -> Sub.none
        , submit = submit
        , respond = \_ _ -> ()
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
    -> Control backendModel broadcastMsg output
element { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    define
        { frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Internal.fail
        , toFrontendGadget = Internal.fail
        , placeholder = placeholder
        , init = init |> Tuple.mapSecond toCommand
        , load = load
        , update = \msg model -> update msg model |> Tuple.mapSecond toCommand
        , updateFromBackend = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \_ _ -> ()
        }


transmitter :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Command msg broadcastMsg toBackend )
    , load : output -> model
    , update : msg -> model -> ( model, Command msg broadcastMsg toBackend )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    }
    -> Control backendModel broadcastMsg output
transmitter { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    define
        { frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Internal.fail
        , toFrontendGadget = Internal.fail
        , placeholder = placeholder
        , init = init
        , load = load
        , update = \msg model -> update msg model
        , updateFromBackend = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \_ _ -> ()
        }


transceiver :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Command msg broadcastMsg toBackend )
    , load : output -> model
    , update : msg -> model -> ( model, Command msg broadcastMsg toBackend )
    , updateFromBroadcast : broadcastMsg -> model -> ( model, Command msg broadcastMsg toBackend )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    }
    -> Control backendModel broadcastMsg output
transceiver { placeholder, init, load, update, updateFromBroadcast, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    define
        { frontendModelGadget = modelGadget
        , frontendMsgGadget = msgGadget
        , outputGadget = outputGadget
        , toBackendGadget = Internal.fail
        , toFrontendGadget = Internal.fail
        , placeholder = placeholder
        , init = init
        , load = load
        , update = update
        , updateFromBackend = \_ model -> ( model, noCommand )
        , updateFromBroadcast = updateFromBroadcast
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \_ _ -> ()
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
    , init : ( frontendModel, Command frontendMsg broadcastMsg toBackend )
    , load : output -> frontendModel
    , update : frontendMsg -> frontendModel -> ( frontendModel, Command frontendMsg broadcastMsg toBackend )
    , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Command frontendMsg broadcastMsg toBackend )
    , updateFromBroadcast : broadcastMsg -> frontendModel -> ( frontendModel, Command frontendMsg broadcastMsg toBackend )
    , view : String -> frontendModel -> H.Html frontendMsg
    , subscriptions : frontendModel -> Sub frontendMsg
    , submit : frontendModel -> Result String output
    , respond : toBackend -> backendModel -> toFrontend
    }
    -> Control backendModel broadcastMsg output
define config =
    let
        placeholderValue =
            config.placeholder
                |> config.load
                |> IR.fromInput config.frontendModelGadget

        mapCommand =
            Internal.mapCommand
                (IR.fromInput config.frontendMsgGadget)
                identity
                (IR.fromInput config.toBackendGadget)
    in
    Control <|
        \backendModelGadget ->
            { init =
                let
                    ( model, command ) =
                        config.init
                in
                ( IR.fromInput config.frontendModelGadget model
                , mapCommand command
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
                        Ok ( model, command ) ->
                            ( IR.fromInput config.frontendModelGadget model
                            , mapCommand command
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
                        Ok ( model, command ) ->
                            ( IR.fromInput config.frontendModelGadget model
                            , mapCommand command
                            )

                        Err _ ->
                            ( modelValue
                            , noCommand
                            )
            , updateFromBroadcast =
                \broadcastMsg modelValue ->
                    let
                        result =
                            Result.map
                                (config.updateFromBroadcast broadcastMsg)
                                (IR.toOutput config.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput config.frontendModelGadget model
                            , mapCommand command
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
    -> Control backendModel broadcastMsg output
    -> Control backendModel broadcastMsg output
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
