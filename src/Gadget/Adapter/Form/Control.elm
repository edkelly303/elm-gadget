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
type Control backend broadcast output
    = Control (IR.Gadget backend -> Internal.InnerControl broadcast)


type Builder request response backend broadcast model msg output
    = Builder
        { modelGadget : IR.Gadget model
        , msgGadget : IR.Gadget msg
        , outputGadget : IR.Gadget output
        , requestGadget : IR.Gadget request
        , responseGadget : IR.Gadget response
        , placeholder : output
        , init : ( model, Command msg broadcast request )
        , load : output -> model
        , update : msg -> model -> ( model, Command msg broadcast request )
        , updateFromBackend : response -> model -> ( model, Command msg broadcast request )
        , updateFromBroadcast : broadcast -> model -> ( model, Command msg broadcast request )
        , view : String -> model -> H.Html msg
        , subscriptions : model -> Sub msg
        , submit : model -> Result String output
        , respond : request -> backend -> response
        , layout : List Internal.UI
        }


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , requestGadget : IR.Gadget request
    , responseGadget : IR.Gadget response
    , placeholder : output
    , init : ( model, Command msg broadcast request )
    , load : output -> model
    , update : msg -> model -> ( model, Command msg broadcast request )
    , updateFromResponse : response -> model -> ( model, Command msg broadcast request )
    , updateFromBroadcast : broadcast -> model -> ( model, Command msg broadcast request )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , respond : request -> backend -> response
    }
    -> Builder request response backend broadcast model msg output
new args =
    Builder
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , requestGadget = args.requestGadget
        , responseGadget = args.responseGadget
        , placeholder = args.placeholder
        , init = args.init
        , load = args.load
        , update = args.update
        , updateFromBackend = args.updateFromResponse
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
    -> Builder () () backend broadcast model msg output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , requestGadget = Gadget.unit
        , responseGadget = Gadget.unit
        , placeholder = placeholder
        , init = ( init, noCommand )
        , load = load
        , update = \msg model -> ( update msg model, noCommand )
        , updateFromResponse = \_ model -> ( model, noCommand )
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
    -> Builder request () backend broadcast model msg output
element { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , requestGadget = Internal.fail
        , responseGadget = Internal.fail
        , placeholder = placeholder
        , init = init |> Tuple.mapSecond toCommand
        , load = load
        , update = \msg model -> update msg model |> Tuple.mapSecond toCommand
        , updateFromResponse = \_ model -> ( model, noCommand )
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
    -> Builder request () backend broadcast model msg output
transmitter { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , requestGadget = Internal.fail
        , responseGadget = Internal.fail
        , placeholder = placeholder
        , init = init
        , load = load
        , update = update
        , updateFromResponse = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \_ _ -> ()
        }


{-| Turn a `Builder` into a `Control`.
-}
build :
    Builder request response backend broadcast model msg output
    -> Control backend broadcast output
build (Builder builder) =
    let
        placeholderValue =
            builder.placeholder
                |> builder.load
                |> IR.fromInput builder.modelGadget

        mapCommand =
            Internal.mapCommand
                (IR.fromInput builder.msgGadget)
                identity
                (IR.fromInput builder.requestGadget)
    in
    Control <|
        \backendModelGadget ->
            { init =
                let
                    ( model, command ) =
                        builder.init
                in
                ( IR.fromInput builder.modelGadget model
                , mapCommand command
                )
            , load =
                \outputValue ->
                    IR.toOutput builder.outputGadget outputValue
                        |> Result.map (\output -> builder.load output)
                        |> Result.map (IR.fromInput builder.modelGadget)
                        |> Result.withDefault placeholderValue
            , placeholder = IR.fromInput builder.outputGadget builder.placeholder
            , update =
                \msg modelValue ->
                    let
                        result =
                            Result.map2 builder.update
                                (IR.toOutput builder.msgGadget msg)
                                (IR.toOutput builder.modelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.modelGadget model
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
                                (IR.toOutput builder.responseGadget msg)
                                (IR.toOutput builder.modelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.modelGadget model
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
                                (IR.toOutput builder.modelGadget modelValue)
                    in
                    case result of
                        Ok ( model, command ) ->
                            ( IR.fromInput builder.modelGadget model
                            , mapCommand command
                            )

                        Err _ ->
                            ( modelValue
                            , noCommand
                            )
            , view =
                \id modelValue ->
                    Result.map (builder.view id) (IR.toOutput builder.modelGadget modelValue)
                        |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                        |> H.map (\msg -> IR.fromInput builder.msgGadget msg)
            , layout =
                builder.layout
            , subscriptions = \_ -> Sub.none
            , submit =
                \path modelValue ->
                    IR.toOutput builder.modelGadget modelValue
                        |> Result.andThen
                            (\model ->
                                builder.submit model
                                    |> Result.mapError (\error -> [ { error = error, path = path } ])
                                    |> Result.map (IR.fromInput builder.outputGadget)
                            )
            , respond =
                \toBackend backend ->
                    Result.map2 builder.respond
                        (IR.toOutput builder.requestGadget toBackend)
                        (IR.toOutput backendModelGadget backend)
                        |> Result.map (IR.fromInput builder.responseGadget)
                        |> Result.withDefault IR.UnitValue
            }


withReceiver :
    (broadcast -> model -> ( model, Command msg broadcast request ))
    -> Builder request toFrontend backend broadcast model msg output
    -> Builder request toFrontend backend broadcast model msg output
withReceiver updateFromBroadcast (Builder builder) =
    Builder
        { modelGadget = builder.modelGadget
        , msgGadget = builder.msgGadget
        , outputGadget = builder.outputGadget
        , requestGadget = builder.requestGadget
        , responseGadget = builder.responseGadget
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
    -> Builder request response backend broadcast model msg output
    -> Builder request response backend broadcast model msg output
withLayout f (Builder builder) =
    Builder
        { builder
            | layout =
                f { label = Internal.Label, input = Internal.Input, feedback = Internal.Feedback }
                    |> List.Extra.unique
        }
