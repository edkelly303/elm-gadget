module Gadget.Adapter.Form.Internal exposing (..)

import Gadget.IR exposing (Error, Path, Value)
import Html as H


type Command frontendMsg toBackend
    = Command (List (Effect frontendMsg toBackend))


type Effect frontendMsg toBackend
    = Cmd (Cmd frontendMsg)
    | ToBackend toBackend


batchCommands : List (Command frontendMsg toBackend) -> Command frontendMsg toBackend
batchCommands cmdTypes =
    List.foldl concatCommands (Command [ Cmd Cmd.none ]) cmdTypes


concatCommands : Command frontendMsg toBackend -> Command frontendMsg toBackend -> Command frontendMsg toBackend
concatCommands (Command cmdType1) (Command cmdType2) =
    Command (cmdType1 ++ cmdType2)


mapCommand : (frontendMsgA -> frontendMsgB) -> (toBackendA -> toBackendB) -> Command frontendMsgA toBackendA -> Command frontendMsgB toBackendB
mapCommand cmdMapper toBackendMapper (Command cmdTypes) =
    List.map
        (\cmdType_ ->
            case cmdType_ of
                Cmd cmd ->
                    Cmd (Cmd.map cmdMapper cmd)

                ToBackend toBackend ->
                    ToBackend (toBackendMapper toBackend)
        )
        cmdTypes
        |> Command


toCmd : (frontendMsg -> msg) -> (toBackend -> Cmd msg) -> Command frontendMsg toBackend -> Cmd msg
toCmd toFrontendMsg sendToBackend (Command cmdTypes) =
    List.map
        (\cmdType ->
            case cmdType of
                Cmd cmd ->
                    Cmd.map toFrontendMsg cmd

                ToBackend toBackend ->
                    sendToBackend toBackend
        )
        cmdTypes
        |> Cmd.batch


type alias InnerControl =
    { init : ( Value, Command Value Value )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Command Value Value )
    , updateFromBackend : Value -> Value -> ( Value, Command Value Value )
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
