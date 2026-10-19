module Gadget.Adapter.Form.Control exposing
    ( Control(..)
    , Builder, sandbox, element, transmitter, new, build, withReceiver, withLayout
    , Command, noCommand, toCommand, request, broadcast
    )

{-|

@docs Control
@docs Builder, sandbox, element, transmitter, new, build, withReceiver, withLayout
@docs Command, noCommand, toCommand, request, broadcast

-}

import Gadget
import Gadget.Adapter.Form.Internal as Internal
import Gadget.IR as IR
import Html as H
import List.Extra
import Result.Extra


type alias Command msg broadcast request =
    Internal.Command msg broadcast request


type alias UI =
    Internal.UI


noCommand : Command msg broadcast request
noCommand =
    Internal.Command []


toCommand : Cmd msg -> Command msg broadcast request
toCommand cmd =
    Internal.Command [ Internal.Cmd cmd ]


request : request -> Command msg broadcast request
request toBackend =
    Internal.Command [ Internal.Request toBackend ]


broadcast : broadcast -> Command msg broadcast request
broadcast broadcastMsg =
    Internal.Command [ Internal.Broadcast broadcastMsg ]


{-| A custom form control.
-}
type Control backendModel broadcast output
    = Control (IR.Gadget backendModel -> Internal.InnerControl broadcast)


type Builder request response backendModel broadcast frontendModel frontendMsg output
    = Builder
        { frontendModelGadget : IR.Gadget frontendModel
        , frontendMsgGadget : IR.Gadget frontendMsg
        , outputGadget : IR.Gadget output
        , toBackendGadget : IR.Gadget request
        , toFrontendGadget : IR.Gadget response
        , placeholder : output
        , init : ( frontendModel, Command frontendMsg broadcast request )
        , load : output -> frontendModel
        , update : frontendMsg -> frontendModel -> ( frontendModel, Command frontendMsg broadcast request )
        , updateFromBackend : response -> frontendModel -> ( frontendModel, Command frontendMsg broadcast request )
        , updateFromBroadcast : broadcast -> frontendModel -> ( frontendModel, Command frontendMsg broadcast request )
        , view : String -> frontendModel -> H.Html frontendMsg
        , subscriptions : frontendModel -> Sub frontendMsg
        , submit : frontendModel -> Result String output
        , respond : request -> backendModel -> response
        , layout : List Internal.UI
        }


new :
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
    -> Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
new args =
    Builder
        { frontendModelGadget = args.frontendModelGadget
        , frontendMsgGadget = args.frontendMsgGadget
        , outputGadget = args.outputGadget
        , toBackendGadget = args.toBackendGadget
        , toFrontendGadget = args.toFrontendGadget
        , placeholder = args.placeholder
        , init = args.init
        , load = args.load
        , update = args.update
        , updateFromBackend = args.updateFromBackend
        , updateFromBroadcast = args.updateFromBroadcast
        , view = args.view
        , subscriptions = args.subscriptions
        , submit = args.submit
        , respond = args.respond
        , layout = [ Internal.Label, Internal.Input, Internal.Feedback ]
        }


sandbox :
    { placeholder : output
    , init : model
    , load : output -> model
    , update : msg -> model -> model
    , view : String -> model -> H.Html msg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    }
    -> Builder () () d broadcastMsg model msg output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    new
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
    { placeholder : output
    , init : ( model, Cmd msg )
    , load : output -> model
    , update : msg -> model -> ( model, Cmd msg )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    }
    -> Builder toBackend () backendModel broadcastMsg model msg output
element { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
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
    { placeholder : output
    , init : ( model, Command msg broadcast request )
    , load : output -> model
    , update : msg -> model -> ( model, Command msg broadcast request )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    }
    -> Builder request () backendModel broadcast model msg output
transmitter { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
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
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \_ _ -> ()
        }


{-| Turn a `Builder` into a `Control`.
-}
build :
    Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
    -> Control backendModel broadcastMsg output
build (Builder builder) =
    let
        placeholderValue =
            builder.placeholder
                |> builder.load
                |> IR.fromInput builder.frontendModelGadget

        mapCommand =
            Internal.mapCommand
                (IR.fromInput builder.frontendMsgGadget)
                identity
                (IR.fromInput builder.toBackendGadget)
    in
    Control <|
        \backendModelGadget ->
            { init =
                let
                    ( model, command ) =
                        builder.init
                in
                ( IR.fromInput builder.frontendModelGadget model
                , mapCommand command
                )
            , load =
                \outputValue ->
                    IR.toOutput builder.outputGadget outputValue
                        |> Result.map (\output -> builder.load output)
                        |> Result.map (IR.fromInput builder.frontendModelGadget)
                        |> Result.withDefault placeholderValue
            , placeholder = IR.fromInput builder.outputGadget builder.placeholder
            , update =
                \msg modelValue ->
                    let
                        result =
                            Result.map2 builder.update
                                (IR.toOutput builder.frontendMsgGadget msg)
                                (IR.toOutput builder.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.frontendModelGadget model
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
                            Result.map2 builder.updateFromBackend
                                (IR.toOutput builder.toFrontendGadget msg)
                                (IR.toOutput builder.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.frontendModelGadget model
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
                                (builder.updateFromBroadcast broadcastMsg)
                                (IR.toOutput builder.frontendModelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.frontendModelGadget model
                            , mapCommand command
                            )

                        Err _ ->
                            ( modelValue
                            , noCommand
                            )
            , view =
                \id modelValue ->
                    Result.map (builder.view id) (IR.toOutput builder.frontendModelGadget modelValue)
                        |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                        |> H.map (\msg -> IR.fromInput builder.frontendMsgGadget msg)
            , layout =
                builder.layout
            , subscriptions = \_ -> Sub.none
            , submit =
                \path modelValue ->
                    IR.toOutput builder.frontendModelGadget modelValue
                        |> Result.andThen
                            (\model ->
                                builder.submit model
                                    |> Result.mapError (\error -> [ { error = error, path = path } ])
                                    |> Result.map (IR.fromInput builder.outputGadget)
                            )
            , respond =
                \toBackend backendModel ->
                    Result.map2 builder.respond
                        (IR.toOutput builder.toBackendGadget toBackend)
                        (IR.toOutput backendModelGadget backendModel)
                        |> Result.map (IR.fromInput builder.toFrontendGadget)
                        |> Result.withDefault IR.UnitValue
            }


withReceiver :
    (broadcastMsg -> frontendModel -> ( frontendModel, Command frontendMsg broadcastMsg toBackend ))
    -> Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
    -> Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
withReceiver updateFromBroadcast (Builder builder) =
    Builder
        { frontendModelGadget = builder.frontendModelGadget
        , frontendMsgGadget = builder.frontendMsgGadget
        , outputGadget = builder.outputGadget
        , toBackendGadget = builder.toBackendGadget
        , toFrontendGadget = builder.toFrontendGadget
        , placeholder = builder.placeholder
        , init = builder.init
        , load = builder.load
        , update = builder.update
        , updateFromBackend = builder.updateFromBackend
        , updateFromBroadcast = updateFromBroadcast
        , view = builder.view
        , subscriptions = builder.subscriptions
        , submit = builder.submit
        , respond = builder.respond
        , layout = builder.layout
        }


withLayout :
    ({ label : UI, input : UI, feedback : UI } -> List UI)
    -> Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
    -> Builder toBackend toFrontend backendModel broadcastMsg frontendModel frontendMsg output
withLayout f (Builder builder) =
    Builder
        { builder
            | layout =
                f { label = Internal.Label, input = Internal.Input, feedback = Internal.Feedback }
                    |> List.Extra.unique
        }
