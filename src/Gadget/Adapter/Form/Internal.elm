module Gadget.Adapter.Form.Internal exposing (..)

import Gadget.IR as IR exposing (Error, Path, Value)
import Html as H


type Command msg broadcast request
    = Command (List (Effect msg broadcast request))


type Effect msg broadcast request
    = Cmd (Cmd msg)
    | Broadcast broadcast
    | Request request


type Control context request response broadcast model msg output
    = Control
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
        , respond : request -> context -> response
        , layout : List UI
        }


type alias IRControl context broadcast =
    { init : ( Value, Command Value broadcast Value )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Command Value broadcast Value )
    , updateFromResponse : Value -> Value -> ( Value, Command Value broadcast Value )
    , updateFromBroadcast : broadcast -> Value -> ( Value, Command Value broadcast Value )
    , view : String -> Value -> H.Html Value
    , subscriptions : Value -> Sub Value
    , layout : List UI
    , submit : Path -> Value -> Result (List Error) Value
    , respond : Value -> context -> Value
    }


batchCommands : List (Command msg broadcast request) -> Command msg broadcast request
batchCommands commands =
    List.foldl concatCommands (Command [ Cmd Cmd.none ]) commands


concatCommands : Command msg broadcast request -> Command msg broadcast request -> Command msg broadcast request
concatCommands (Command cmdType1) (Command cmdType2) =
    Command (cmdType1 ++ cmdType2)


mapCommand :
    (msgA -> msgB)
    -> (broadcastA -> broadcastB)
    -> (requestA -> requestB)
    -> Command msgA broadcastA requestA
    -> Command msgB broadcastB requestB
mapCommand cmdMapper broadcastMsgMapper toBackendMapper (Command commands) =
    List.map
        (\cmdType_ ->
            case cmdType_ of
                Cmd cmd ->
                    Cmd (Cmd.map cmdMapper cmd)

                Broadcast broadcastMsg ->
                    Broadcast (broadcastMsgMapper broadcastMsg)

                Request toBackend ->
                    Request (toBackendMapper toBackend)
        )
        commands
        |> Command


extractBroadcasts : Command msg broadcast request -> ( List broadcast, Command msg broadcast request )
extractBroadcasts (Command commands) =
    List.foldl
        (\effect ( broadcastMsgs, command ) ->
            case effect of
                Broadcast b ->
                    ( b :: broadcastMsgs, command )

                _ ->
                    ( broadcastMsgs, concatCommands (Command [ effect ]) command )
        )
        ( [], Command [] )
        commands


splitCommand : Command msg broadcast request -> ( Cmd msg, List request )
splitCommand (Command command) =
    List.foldl
        (\cmdType ( outCmds, outRequests ) ->
            case cmdType of
                Cmd cmd ->
                    ( cmd :: outCmds, outRequests )

                Broadcast _ ->
                    ( outCmds, outRequests )

                Request r ->
                    ( outCmds, r :: outRequests )
        )
        ( [], [] )
        command
        |> Tuple.mapFirst Cmd.batch


fail : IR.Gadget a
fail =
    IR.Gadget
        { fromInput = \_ -> IR.UnitValue
        , toOutput =
            \path _ ->
                Err [ { error = "fail failed", path = path } ]
        , irType = IR.UnitType IR.emptyMetadata
        }


never : IR.Gadget Never
never =
    IR.Gadget
        { fromInput = \ever -> Basics.never ever
        , toOutput =
            \path _ ->
                Err [ { error = "never say never", path = path } ]
        , irType = IR.UnitType IR.emptyMetadata
        }


type UI
    = Label
    | Input
    | Feedback
