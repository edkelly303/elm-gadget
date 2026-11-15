module Gadget.Adapter.Form2 exposing (..)

import Dict exposing (Dict)
import Gadget
import Gadget.IR as IR exposing (Error, Type(..), Value(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra
import Set exposing (Set)


type Msg
    = Msg Path Value


type alias Path =
    List String


type Model
    = Sum String (Dict String ( Int, Dict String Model ))
    | Collection Type (Dict String Model)
    | Record Int (Dict String ( Int, Model ))
    | Tuple Model Model
    | Triple Model Model Model
    | Unit
    | Primitive PrimitiveType Value


type PrimitiveType
    = StringType
    | CharType
    | IntType
    | FloatType
    | BoolType
    | OverrideType String


type alias Form inMsg appMsg output =
    { init : ( Model, Cmd appMsg )
    , load : output -> Model
    , update : Msg -> Model -> ( Model, Cmd appMsg )
    , inject : inMsg -> Model -> Model
    , view : Model -> H.Html appMsg
    , subscriptions : Model -> Sub appMsg
    , submit : Model -> Result (List Error) output
    }


type Control model inMsg appMsg output
    = Control
        { modelGadget : IR.Gadget model
        , msgGadget : IR.Gadget appMsg
        , outputGadget : IR.Gadget output
        , placeholder : output
        , init : ( model, Cmd appMsg )
        , load : output -> model
        , update : { msgToAppMsg : Msg -> appMsg, inMsgToAppMsg : inMsg -> appMsg } -> appMsg -> model -> ( model, Cmd appMsg )
        , inject : inMsg -> model -> model
        , view : String -> model -> H.Html appMsg
        , subscriptions : model -> Sub appMsg
        , submit : model -> Result String output
        , layout : List UI
        }


type alias InternalControl inMsg appMsg =
    { init : ( Value, Cmd appMsg )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Cmd appMsg )
    , inject : inMsg -> Value -> Value
    , view : String -> Value -> H.Html appMsg
    , subscriptions : Value -> Sub appMsg
    , layout : List UI
    , submit : Path -> Value -> Result (List Error) Value
    }


type alias Config inMsg appMsg output =
    { bool : InternalControl inMsg appMsg
    , int : InternalControl inMsg appMsg
    , float : InternalControl inMsg appMsg
    , char : InternalControl inMsg appMsg
    , string : InternalControl inMsg appMsg
    , overrides : Dict String (InternalControl inMsg appMsg)
    , viewFeedback : String -> H.Html appMsg
    , viewControl : Bool -> List (H.Html appMsg) -> List (H.Html appMsg)
    , viewTopLevel :
        { switchActiveField : Int -> appMsg
        , activeField : Int
        , fieldViews : List (H.Html appMsg)
        , result : Result (Set Int) output
        }
        -> List (H.Html appMsg)
    , msgToAppMsg : Msg -> appMsg
    , inMsgToAppMsg : inMsg -> appMsg
    }


type FormBuilder inMsg appMsg output
    = FormBuilder
        { bool : InternalControl inMsg appMsg
        , int : InternalControl inMsg appMsg
        , float : InternalControl inMsg appMsg
        , char : InternalControl inMsg appMsg
        , string : InternalControl inMsg appMsg
        , viewFeedback : String -> H.Html appMsg
        , viewControl : Bool -> List (H.Html appMsg) -> List (H.Html appMsg)
        , viewTopLevel :
            { switchActiveField : Int -> appMsg
            , activeField : Int
            , fieldViews : List (H.Html appMsg)
            , result : Result (Set.Set Int) output
            }
            -> List (H.Html appMsg)
        , overrides : Dict String (InternalControl inMsg appMsg)
        , msgToAppMsg : Msg -> appMsg
        , inMsgToAppMsg : inMsg -> appMsg
        }


type UI
    = Label
    | Input
    | Feedback



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'


-}


newForm : (Msg -> appMsg) -> (inMsg -> appMsg) -> FormBuilder inMsg appMsg output
newForm fromMsg fromInMsg =
    FormBuilder
        { bool = buildControl boolControl
        , int = buildControl intControl
        , float = buildControl floatControl
        , char = buildControl charControl
        , string = buildControl stringControl
        , viewFeedback = \error -> H.span [] [ H.text error ]
        , viewTopLevel = \{ fieldViews } -> fieldViews
        , viewControl =
            \validity inner ->
                [ H.node "form-control"
                    [ HA.class
                        (if validity then
                            "valid"

                         else
                            "invalid"
                        )
                    ]
                    inner
                ]
        , overrides = Dict.empty
        , msgToAppMsg = fromMsg
        , inMsgToAppMsg = fromInMsg
        }


endForm :
    IR.Gadget output
    -> FormBuilder inMsg appMsg output
    -> Form context appMsg output
endForm gadget (FormBuilder builder) =
    let
        config : Config inMsg appMsg output
        config =
            { bool = builder.bool
            , int = builder.int
            , float = builder.float
            , char = builder.char
            , string = builder.string
            , overrides = builder.overrides
            , viewFeedback = builder.viewFeedback
            , viewControl = builder.viewControl
            , viewTopLevel = builder.viewTopLevel
            , msgToAppMsg = builder.msgToAppMsg
            , inMsgToAppMsg = builder.inMsgToAppMsg
            }

        init_ =
            let
                ( model, cmd ) =
                    init config gadget
            in
            ( model
            , Cmd.map builder.toMsg cmd
            )

        update_ msg model =
            let
                ( newModel, cmd ) =
                    update config msg model
            in
            ( newModel
            , Cmd.map builder.toMsg cmd
            )

        load_ output =
            load config gadget output

        inject_ inMsg model =
            Debug.log "inject" model

        view_ model =
            view config gadget gadget
                |> H.map builder.toMsg

        subscriptions_ model =
            subscriptions config model
                |> Sub.map builder.toMsg

        submit_ model =
            submit config gadget model
    in
    { init = init_
    , load = load_
    , update = update_
    , inject = inject_
    , view = view_
    , subscriptions = subscriptions_
    , submit = submit_
    }


withOverride :
    String
    -> Control model inMsg controlMsg controlOutput
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withOverride id controlBuilder (FormBuilder builder) =
    let
        toControl =
            buildControl controlBuilder
    in
    FormBuilder { builder | overrides = Dict.insert id toControl builder.overrides }


withIntControl :
    Control model inMsg controlMsg Int
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withIntControl control (FormBuilder builder) =
    FormBuilder { builder | int = buildControl control }


withFloatControl :
    Control model inMsg controlMsg Float
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withFloatControl control (FormBuilder builder) =
    FormBuilder { builder | float = buildControl control }


withStringControl :
    Control model inMsg controlMsg String
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withStringControl control (FormBuilder builder) =
    FormBuilder { builder | string = buildControl control }


withCharControl :
    Control model inMsg controlMsg Char
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withCharControl control (FormBuilder builder) =
    FormBuilder { builder | char = buildControl control }


withBoolControl :
    Control model inMsg controlMsg Bool
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withBoolControl control (FormBuilder builder) =
    FormBuilder { builder | bool = buildControl control }


withControlView :
    (Bool -> List (H.Html appMsg) -> List (H.Html appMsg))
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withControlView f (FormBuilder builder) =
    FormBuilder { builder | viewControl = f }


withFeedbackView :
    (String -> H.Html appMsg)
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withFeedbackView f (FormBuilder builder) =
    FormBuilder { builder | viewFeedback = f }


withTopLevelView :
    ({ switchActiveField : Int -> appMsg
     , activeField : Int
     , fieldViews : List (H.Html appMsg)
     , result : Result (Set.Set Int) output
     }
     -> List (H.Html appMsg)
    )
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withTopLevelView f (FormBuilder builder) =
    FormBuilder { builder | viewTopLevel = f }


buildControl :
    Control model inMsg controlMsg output
    -> InternalControl inMsg appMsg
buildControl (Control builder) =
    let
        placeholderValue =
            builder.placeholder
                |> builder.load
                |> IR.fromInput builder.modelGadget
    in
    { init =
        let
            ( model, command ) =
                builder.init
        in
        ( IR.fromInput builder.modelGadget model
        , command
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
                    , command
                    )

                Err _ ->
                    ( modelValue
                    , Cmd.none
                    )
    , inject =
        \inMsg modelValue ->
            Debug.log "TODO" modelValue
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
    }


intControl : Control String inMsg appMsg Int
intControl =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.int
        , init = ""
        , placeholder = 0
        , load = String.fromInt
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "number"
                    , HA.attribute "inputmode" "numeric"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "This must be an integer"
        }


floatControl : Control String inMsg appMsg Float
floatControl =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.float
        , init = ""
        , placeholder = 0.0
        , load = String.fromFloat
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "number"
                    , HA.attribute "inputmode" "decimal"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.toFloat model
                    |> Result.fromMaybe "This must be a decimal number"
        }


stringControl : Control String inMsg appMsg String
stringControl =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.string
        , outputGadget = Gadget.string
        , init = ""
        , placeholder = ""
        , load = identity
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit = Ok
        }


boolControl : Control Bool inMsg appMsg Bool
boolControl =
    sandbox
        { modelGadget = Gadget.bool
        , msgGadget = Gadget.bool
        , outputGadget = Gadget.bool
        , init = False
        , placeholder = False
        , load = identity
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "checkbox"
                    , HE.onCheck identity
                    , HA.checked model
                    , HA.id id
                    ]
                    []
        , submit = Ok
        }
        |> withLayout (\ui -> [ ui.input, ui.label, ui.feedback ])


withLayout f (Control builder) =
    Control
        { builder
            | layout =
                f { label = Label, input = Input, feedback = Feedback }
                    |> List.Extra.unique
        }


charControl : Control String inMsg appMsg Char
charControl =
    sandbox
        { modelGadget = Gadget.string
        , msgGadget = Gadget.maybe Gadget.char
        , outputGadget = Gadget.char
        , init = ""
        , placeholder = 'a'
        , load = String.fromChar
        , update =
            \msg _ ->
                case msg of
                    Nothing ->
                        ""

                    Just c ->
                        String.fromChar c
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput (\str -> String.uncons str |> Maybe.map Tuple.first)
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.uncons model
                    |> Maybe.map Tuple.first
                    |> Result.fromMaybe "This must not be blank"
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
    -> Control model inMsg appMsg output
sandbox { placeholder, init, load, update, view, submit, modelGadget, msgGadget, outputGadget } =
    new
        { modelGadget = modelGadget
        , msgGadget = msgGadget
        , outputGadget = outputGadget
        , placeholder = placeholder
        , init = ( init, Cmd.none )
        , load = load
        , update = \_ msg model -> ( update msg model, Cmd.none )
        , inject = \_ model -> model
        , view = view
        , subscriptions = \_ -> Sub.none
        , submit = submit
        }


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget appMsg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Cmd appMsg )
    , load : output -> model
    , update : { msgToAppMsg : Msg -> appMsg, inMsgToAppMsg : inMsg -> appMsg } -> appMsg -> model -> ( model, Cmd appMsg )
    , inject : inMsg -> model -> model
    , view : String -> model -> H.Html appMsg
    , subscriptions : model -> Sub appMsg
    , submit : model -> Result String output
    }
    -> Control model inMsg appMsg output
new args =
    Control
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , placeholder = args.placeholder
        , init = args.init
        , load = args.load
        , update = args.update
        , inject = args.inject
        , view = args.view
        , subscriptions = args.subscriptions
        , submit = args.submit
        , layout = [ Label, Input, Feedback ]
        }
