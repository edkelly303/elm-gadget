module Gadget.Adapter.Form.Internal exposing (..)

import Gadget.IR as IR exposing (Error, Path, Value)
import Html as H


type Command msg broadcast request
    = Command (List (Effect msg broadcast request))


type Effect msg broadcast request
    = Cmd (Cmd msg)
    | Broadcast broadcast
    | Request request


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


toCmd : (msg -> msg2) -> (request -> Cmd msg2) -> Command msg broadcast request -> Cmd msg2
toCmd toMsg sendRequest (Command command) =
    List.map
        (\cmdType ->
            case cmdType of
                Cmd cmd ->
                    Cmd.map toMsg cmd

                Broadcast _ ->
                    Cmd.none

                Request r ->
                    sendRequest r
        )
        command
        |> Cmd.batch


fail : IR.Gadget a
fail =
    IR.Gadget
        { fromInput = \_ -> IR.UnitValue
        , toOutput =
            \path _ ->
                Err [ { error = "failed", path = path } ]
        , irType = IR.UnitType IR.emptyMetadata
        }


type alias InnerControl broadcast =
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
    , respond : Value -> Value -> Value
    }


type UI
    = Label
    | Input
    | Feedback
