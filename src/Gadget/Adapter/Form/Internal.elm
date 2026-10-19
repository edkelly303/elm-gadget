module Gadget.Adapter.Form.Internal exposing (..)

import Gadget.IR exposing (Error, Path, Value)
import Html as H


type Command frontendMsg broadcastMsg toBackend
    = Command (List (Effect frontendMsg broadcastMsg toBackend))


type Effect frontendMsg broadcastMsg toBackend
    = Cmd (Cmd frontendMsg)
    | Broadcast broadcastMsg
    | ToBackend toBackend


batchCommands : List (Command frontendMsg broadcastMsg toBackend) -> Command frontendMsg broadcastMsg toBackend
batchCommands commands =
    List.foldl concatCommands (Command [ Cmd Cmd.none ]) commands


concatCommands : Command frontendMsg broadcastMsg toBackend -> Command frontendMsg broadcastMsg toBackend -> Command frontendMsg broadcastMsg toBackend
concatCommands (Command cmdType1) (Command cmdType2) =
    Command (cmdType1 ++ cmdType2)


mapCommand : (frontendMsgA -> frontendMsgB) -> (broadcastMsgA -> broadcastMsgB) -> (toBackendA -> toBackendB) -> Command frontendMsgA broadcastMsgA toBackendA -> Command frontendMsgB broadcastMsgB toBackendB
mapCommand cmdMapper broadcastMsgMapper toBackendMapper (Command commands) =
    List.map
        (\cmdType_ ->
            case cmdType_ of
                Cmd cmd ->
                    Cmd (Cmd.map cmdMapper cmd)

                Broadcast broadcastMsg ->
                    Broadcast (broadcastMsgMapper broadcastMsg)

                ToBackend toBackend ->
                    ToBackend (toBackendMapper toBackend)
        )
        commands
        |> Command


extractBroadcasts : Command frontendMsg broadcastMsg toBackend -> ( List broadcastMsg, Command frontendMsg broadcastMsg toBackend )
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


toCmd : (frontendMsg -> msg) -> (toBackend -> Cmd msg) -> Command frontendMsg broadcastMsg toBackend -> Cmd msg
toCmd toFrontendMsg sendToBackend (Command command) =
    List.map
        (\cmdType ->
            case cmdType of
                Cmd cmd ->
                    Cmd.map toFrontendMsg cmd

                Broadcast broadcastMsg ->
                    Cmd.none

                ToBackend toBackend ->
                    sendToBackend toBackend
        )
        command
        |> Cmd.batch


type alias InnerControl broadcastMsg =
    { init : ( Value, Command Value broadcastMsg Value )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Command Value broadcastMsg Value )
    , updateFromBackend : Value -> Value -> ( Value, Command Value broadcastMsg Value )
    , updateFromBroadcast : broadcastMsg -> Value -> ( Value, Command Value broadcastMsg Value )
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
