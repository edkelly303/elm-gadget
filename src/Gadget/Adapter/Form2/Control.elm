module Gadget.Adapter.Form2.Control exposing
    ( Control, sandbox, element, new
    , Send, toSelf, toForm, toApp
    )

{-|

@docs Control, sandbox, element, new
@docs Send, toSelf, toForm, toApp

-}

import Gadget.Adapter.Form2.Internal as Internal exposing (..)
import Gadget.IR as IR
import Html as H
import Result.Extra


type alias Control formMsg appMsg output =
    Internal.Control formMsg appMsg output


type alias Send controlMsg formMsg appMsg =
    Internal.Send controlMsg formMsg appMsg


toSelf : controlMsg -> Send controlMsg formMsg appMsg
toSelf =
    Internal.ToSelf


toForm : formMsg -> Send controlMsg formMsg appMsg
toForm =
    Internal.ToForm


toApp : appMsg -> Send controlMsg formMsg appMsg
toApp =
    Internal.ToApp


sandbox :
    { placeholder : output
    , init : model
    , load : output -> model
    , update : controlMsg -> model -> model
    , view : String -> model -> H.Html controlMsg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , id : String
    }
    -> Control formMsg appMsg output
sandbox args =
    new
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , placeholder = args.placeholder
        , init = ( args.init, Cmd.none )
        , load = args.load
        , update = \msg model -> ( args.update msg model, Cmd.none )
        , notify = \_ model -> model
        , view = \id model -> args.view id model |> H.map Internal.ToSelf
        , subscriptions = \_ -> Sub.none
        , submit = args.submit
        , id = args.id
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
    , id : String
    }
    -> Control formMsg appMsg output
element args =
    new
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , placeholder = args.placeholder
        , init = args.init |> Tuple.mapSecond (Cmd.map Internal.ToSelf)
        , load = args.load
        , update = \msg model -> args.update msg model |> Tuple.mapSecond (Cmd.map Internal.ToSelf)
        , notify = \_ model -> model
        , view = \id model -> args.view id model |> H.map Internal.ToSelf
        , subscriptions = \model -> args.subscriptions model |> Sub.map Internal.ToSelf
        , submit = args.submit
        , id = args.id
        }


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Cmd (Send controlMsg formMsg appMsg) )
    , load : output -> model
    , update : controlMsg -> model -> ( model, Cmd (Send controlMsg formMsg appMsg) )
    , notify : formMsg -> model -> model
    , view : String -> model -> H.Html (Send controlMsg formMsg appMsg)
    , subscriptions : model -> Sub (Send controlMsg formMsg appMsg)
    , submit : model -> Result String output
    , id : String
    }
    -> Control formMsg appMsg output
new builder =
    let
        placeholderValue =
            builder.placeholder
                |> builder.load
                |> IR.fromInput builder.modelGadget

        controlMsgToValue controlMsg =
            case controlMsg of
                Internal.ToSelf m ->
                    Internal.ToSelf (IR.fromInput builder.msgGadget m)

                Internal.ToForm m ->
                    Internal.ToForm m

                Internal.ToApp m ->
                    Internal.ToApp m
    in
    Control builder.id
        { init =
            let
                ( model, command ) =
                    builder.init
            in
            ( IR.fromInput builder.modelGadget model
            , command |> Cmd.map controlMsgToValue
            )
        , load =
            \outputValue ->
                IR.toOutput builder.outputGadget outputValue
                    |> Result.map (\output -> builder.load output)
                    |> Result.map (IR.fromInput builder.modelGadget)
                    |> Result.withDefault placeholderValue
        , placeholder = IR.fromInput builder.outputGadget builder.placeholder
        , update =
            \controlMsg modelValue ->
                let
                    result =
                        Result.map2 builder.update
                            (IR.toOutput builder.msgGadget controlMsg)
                            (IR.toOutput builder.modelGadget modelValue)
                in
                case result of
                    Ok ( newModelValue, command ) ->
                        ( IR.fromInput builder.modelGadget newModelValue
                        , command |> Cmd.map controlMsgToValue
                        )

                    Err _ ->
                        ( modelValue
                        , Cmd.none
                        )
        , notify =
            \formMsg modelValue ->
                IR.toOutput builder.modelGadget modelValue
                    |> Result.map (builder.notify formMsg)
                    |> Result.map (IR.fromInput builder.modelGadget)
                    |> Result.withDefault modelValue
        , view =
            \id modelValue ->
                Result.map (builder.view id) (IR.toOutput builder.modelGadget modelValue)
                    |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                    |> H.map controlMsgToValue
        , layout =
            [ Label, Input, Feedback ]
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
        }
