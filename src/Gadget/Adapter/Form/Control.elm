module Gadget.Adapter.Form.Control exposing
    ( Control, sandbox, element, transmitter, new, withReceiver, withLayout
    , Command, noCommand, toCommand, request, broadcast
    )

{-|

@docs Control, sandbox, element, transmitter, new, withReceiver, withLayout
@docs Command, noCommand, toCommand, request, broadcast

-}

import Gadget
import Gadget.Adapter.Form.Internal as Internal
import Gadget.IR as IR
import Html as H
import List.Extra


{-| A custom form control.
-}
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


type alias Control context request response broadcast model msg output =
    Internal.Control context request response broadcast model msg output


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
    , respond : request -> context -> response
    }
    -> Control context request response broadcast model msg output
new args =
    Internal.Control
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
    -> Control context Never Never broadcast model msg output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , placeholder = placeholder
        , init = ( init, noCommand )
        , load = load
        , update = \msg model -> ( update msg model, noCommand )
        , updateFromResponse = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = \_ -> Sub.none
        , submit = submit
        , requestGadget = Internal.never
        , responseGadget = Internal.never
        , respond = \ever _ -> never ever
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
    -> Control context Never Never broadcast model msg output
element { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , placeholder = placeholder
        , init = init |> Tuple.mapSecond toCommand
        , load = load
        , update = \msg model -> update msg model |> Tuple.mapSecond toCommand
        , updateFromResponse = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , requestGadget = Internal.never
        , responseGadget = Internal.never
        , respond = \ever _ -> never ever
        }


transmitter :
    { placeholder : output
    , init : ( model, Command msg broadcast Never )
    , load : output -> model
    , update : msg -> model -> ( model, Command msg broadcast Never )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    }
    -> Control context Never Never broadcast model msg output
transmitter { placeholder, init, load, update, view, subscriptions, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , requestGadget = Internal.never
        , responseGadget = Internal.never
        , placeholder = placeholder
        , init = init
        , load = load
        , update = update
        , updateFromResponse = \_ model -> ( model, noCommand )
        , updateFromBroadcast = \_ model -> ( model, noCommand )
        , view = view
        , subscriptions = subscriptions
        , submit = submit
        , respond = \ever _ -> never ever
        }


withReceiver :
    (broadcast -> model -> ( model, Command msg broadcast request ))
    -> Control context request response broadcast model msg output
    -> Control context request response broadcast model msg output
withReceiver updateFromBroadcast (Internal.Control builder) =
    Internal.Control
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
    -> Control context request response broadcast model msg output
    -> Control context request response broadcast model msg output
withLayout f (Internal.Control builder) =
    Internal.Control
        { builder
            | layout =
                f { label = Internal.Label, input = Internal.Input, feedback = Internal.Feedback }
                    |> List.Extra.unique
        }
