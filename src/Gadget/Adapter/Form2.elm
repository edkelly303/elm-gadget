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


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Form"


label : String -> IR.Gadget a -> IR.Gadget a
label l gadget =
    tools.attach "label" Gadget.string l gadget


type ControlMsg
    = ControlMsg Path Value


type Msg controlMsg inMsg appMsg
    = ToSelf controlMsg
    | ToAll inMsg
    | ToApp appMsg


type alias Path =
    List String


type Model
    = Sum String (Dict String ( Int, Dict String Model ))
    | Collection Type (Dict String Model)
    | Record Int (List Model)
    | Tuple Model Model
    | Triple Model Model Model
    | Unit
    | Primitive PrimitiveType Value


type PrimitiveType
    = StringPrimitive
    | CharPrimitive
    | IntPrimitive
    | FloatPrimitive
    | BoolPrimitive
    | OverrideType String


type alias Form inMsg appMsg output =
    { init : ( Model, Cmd appMsg )
    , load : output -> Model
    , update : ControlMsg -> Model -> ( Model, Cmd appMsg )
    , inject : inMsg -> Model -> Model
    , view : Model -> H.Html appMsg
    , subscriptions : Model -> Sub appMsg
    , submit : Model -> Result (List Error) output
    }


type Control model msg inMsg appMsg output
    = Control
        { modelGadget : IR.Gadget model
        , msgGadget : IR.Gadget msg
        , outputGadget : IR.Gadget output
        , placeholder : output
        , init : ( model, Cmd (Msg msg inMsg appMsg) )
        , load : output -> model
        , update : msg -> model -> ( model, Cmd (Msg msg inMsg appMsg) )
        , inject : inMsg -> model -> model
        , view : String -> model -> H.Html (Msg msg inMsg appMsg)
        , subscriptions : model -> Sub (Msg msg inMsg appMsg)
        , submit : model -> Result String output
        , layout : List UI
        }


type alias InternalControl inMsg appMsg =
    { init : ( Value, Cmd (Msg Value inMsg appMsg) )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Cmd (Msg Value inMsg appMsg) )
    , inject : inMsg -> Value -> Value
    , view : String -> Value -> H.Html (Msg Value inMsg appMsg)
    , subscriptions : Value -> Sub (Msg Value inMsg appMsg)
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
    , msgToAppMsg : ControlMsg -> appMsg
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
        , msgToAppMsg : ControlMsg -> appMsg
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


newForm : (ControlMsg -> appMsg) -> (inMsg -> appMsg) -> FormBuilder inMsg appMsg output
newForm msgToAppMsg inMsgToAppMsg =
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
        , msgToAppMsg = msgToAppMsg
        , inMsgToAppMsg = inMsgToAppMsg
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

        type_ =
            IR.irType gadget

        path =
            []

        init_ =
            let
                ( model, cmd ) =
                    init config path type_
            in
            ( model
            , cmd
            )

        update_ msg model =
            let
                ( newModel, cmd ) =
                    update config path type_ msg model
            in
            ( newModel
            , cmd
            )

        load_ output =
            load config path type_ output

        inject_ inMsg model =
            inject config path inMsg model

        view_ model =
            view config path type_ model
                |> H.form []

        subscriptions_ model =
            subscriptions config path model

        submit_ model =
            submit config path type_ model
    in
    { init = init_
    , load = load_
    , update = update_
    , inject = inject_
    , view = view_
    , subscriptions = subscriptions_
    , submit = submit_
    }


init :
    Config inMsg appMsg output
    -> Path
    -> Type
    -> ( Model, Cmd appMsg )
init config path type_ =
    case type_ of
        UnitType metadata ->
            ( Unit, Cmd.none )

        IntType metadata ->
            let
                ( mdl, cmd ) =
                    config.int.init
            in
            ( Primitive IntPrimitive mdl
            , Cmd.map (msgMapper config path) cmd
            )

        RecordType metadata fields ->
            let
                ( out, cmds ) =
                    List.foldr
                        (\( thisFieldName, thisField ) ( prevFields, prevCmds ) ->
                            let
                                ( mdl, cmd ) =
                                    init config (thisFieldName :: path) thisField
                            in
                            ( mdl :: prevFields
                            , cmd :: prevCmds
                            )
                        )
                        ( [], [] )
                        fields
            in
            ( Record 0 out
            , Cmd.batch cmds
            )

        _ ->
            Debug.todo "init"


load config path type_ output =
    Debug.todo "load"


msgMapper config path msg =
    case msg of
        ToSelf toSelf ->
            config.msgToAppMsg (ControlMsg path toSelf)

        ToAll toAll ->
            config.inMsgToAppMsg toAll

        ToApp toApp ->
            toApp


update :
    Config inMsg appMsg output
    -> Path
    -> Type
    -> ControlMsg
    -> Model
    -> ( Model, Cmd appMsg )
update config modelPath type_ ((ControlMsg msgPath controlMsg) as msg) model =
    case ( model, type_ ) of
        ( Unit, _ ) ->
            ( Unit, Cmd.none )

        ( Primitive IntPrimitive controlModel, _ ) ->
            if msgPath == modelPath then
                let
                    ( newControlModel, controlCmd ) =
                        config.int.update controlMsg controlModel
                in
                ( Primitive IntPrimitive newControlModel
                , Cmd.map (msgMapper config modelPath) controlCmd
                )

            else
                ( model, Cmd.none )

        ( Record selected fields, RecordType metadata fieldTypes ) ->
            let
                ( newFields, cmds ) =
                    List.Extra.zip fieldTypes fields
                        |> List.foldr
                            (\( ( fieldName, fieldType ), fieldModel ) ( prevFields, prevCmds ) ->
                                let
                                    ( newFieldModel, fieldCmd ) =
                                        update config (fieldName :: modelPath) fieldType msg fieldModel
                                in
                                ( newFieldModel :: prevFields
                                , fieldCmd :: prevCmds
                                )
                            )
                            ( [], [] )
            in
            ( Record selected newFields
            , Cmd.batch cmds
            )

        _ ->
            Debug.todo "update"


inject config path inMsg model =
    Debug.todo "inject"


view :
    Config inMsg appMsg output
    -> Path
    -> Type
    -> Model
    -> List (H.Html appMsg)
view config path type_ model =
    let
        id =
            pathToString path
    in
    case ( model, type_ ) of
        ( Unit, _ ) ->
            []

        ( Primitive primitive primitiveModel, primitiveType ) ->
            let
                label_ metadata =
                    tools.decode "label" Gadget.string metadata
                        |> Maybe.withDefault id
            in
            case ( primitive, primitiveType ) of
                ( IntPrimitive, IntType metadata ) ->
                    config.viewControl False
                        [ H.label [ HA.for id ] [ H.text (label_ metadata) ]
                        , config.int.view id primitiveModel
                            |> H.map (msgMapper config path)
                        ]

                _ ->
                    Debug.todo "view primitive"

        ( Record selected fieldModels, RecordType metadata fieldTypes ) ->
            let
                label_ =
                    tools.decode "label" Gadget.string metadata
                        |> Maybe.map (\l -> H.label [ HA.for id ] [ H.text l ])
                        |> Maybe.withDefault (H.text "")

                fields =
                    List.Extra.zip fieldTypes fieldModels
                        |> List.concatMap
                            (\( ( fieldName, fieldType ), fieldModel ) ->
                                view config (fieldName :: path) fieldType fieldModel
                            )
            in
            label_ :: fields

        _ ->
            Debug.todo "view"


subscriptions config path model =
    Debug.todo "subscriptions"


submit config path type_ model =
    Debug.todo "submit"


withOverride :
    String
    -> Control model msg inMsg appMsg controlOutput
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withOverride id controlBuilder (FormBuilder builder) =
    let
        toControl =
            buildControl
                controlBuilder
    in
    FormBuilder { builder | overrides = Dict.insert id toControl builder.overrides }


withIntControl :
    Control model msg inMsg appMsg Int
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withIntControl control (FormBuilder builder) =
    FormBuilder
        { builder
            | int =
                buildControl
                    control
        }


withFloatControl :
    Control model msg inMsg appMsg Float
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withFloatControl control (FormBuilder builder) =
    FormBuilder
        { builder
            | float =
                buildControl
                    control
        }


withStringControl :
    Control model msg inMsg appMsg String
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withStringControl control (FormBuilder builder) =
    FormBuilder
        { builder
            | string =
                buildControl
                    control
        }


withCharControl :
    Control model msg inMsg appMsg Char
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withCharControl control (FormBuilder builder) =
    FormBuilder
        { builder
            | char =
                buildControl
                    control
        }


withBoolControl :
    Control model msg inMsg appMsg Bool
    -> FormBuilder inMsg appMsg output
    -> FormBuilder inMsg appMsg output
withBoolControl control (FormBuilder builder) =
    FormBuilder
        { builder
            | bool =
                buildControl
                    control
        }


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
    Control model msg inMsg appMsg output
    -> InternalControl inMsg appMsg
buildControl (Control builder) =
    let
        placeholderValue =
            builder.placeholder
                |> builder.load
                |> IR.fromInput builder.modelGadget

        controlMsgToValue msg =
            case msg of
                ToSelf m ->
                    ToSelf (IR.fromInput builder.msgGadget m)

                ToAll m ->
                    ToAll m

                ToApp m ->
                    ToApp m
    in
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
        \msg modelValue ->
            let
                result =
                    Result.map2 builder.update
                        (IR.toOutput builder.msgGadget msg)
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
    , inject =
        \inMsg modelValue ->
            Debug.log "TODO" modelValue
    , view =
        \id modelValue ->
            Result.map (builder.view id) (IR.toOutput builder.modelGadget modelValue)
                |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                |> H.map controlMsgToValue
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
    -> Control model msg inMsg appMsg output
sandbox args =
    new
        { modelGadget = args.modelGadget
        , msgGadget = args.msgGadget
        , outputGadget = args.outputGadget
        , placeholder = args.placeholder
        , init = ( args.init, Cmd.none )
        , load = args.load
        , update = \msg model -> ( args.update msg model, Cmd.none )
        , inject = \_ model -> model
        , view = \id model -> args.view id model |> H.map ToSelf
        , subscriptions = \_ -> Sub.none
        , submit = args.submit
        }


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget msg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Cmd (Msg msg inMsg appMsg) )
    , load : output -> model
    , update : msg -> model -> ( model, Cmd (Msg msg inMsg appMsg) )
    , inject : inMsg -> model -> model
    , view : String -> model -> H.Html (Msg msg inMsg appMsg)
    , subscriptions : model -> Sub (Msg msg inMsg appMsg)
    , submit : model -> Result String output
    }
    -> Control model msg inMsg appMsg output
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


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"
