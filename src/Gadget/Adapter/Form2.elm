module Gadget.Adapter.Form2 exposing
    ( label, useControl
    , Form, Model, Msg
    , Builder, new, registerControl, withControlView, withFeedbackView, withTopLevelView, end
    )

{-|

@docs label, useControl
@docs Form, Model, Msg
@docs Builder, new, registerControl, withControlView, withFeedbackView, withTopLevelView, end

-}

import Dict exposing (Dict)
import Gadget
import Gadget.Adapter.Form2.Control as Control
import Gadget.Adapter.Form2.Internal as Internal exposing (..)
import Gadget.IR as IR exposing (Error, Type(..), Value)
import Html as H
import Html.Attributes as HA
import List.Extra
import Set


type alias Form formMsg appMsg output =
    { init : ( Model, Cmd appMsg )
    , load : output -> Model
    , update : Msg formMsg -> Model -> ( Model, Cmd appMsg )
    , notify : formMsg -> Model -> Model
    , view : Model -> H.Html appMsg
    , subscriptions : Model -> Sub appMsg
    , submit : Model -> Result (List Error) output
    }


type alias Msg formMsg =
    Internal.Msg formMsg


type Model
    = Sum String (Dict String ( Int, Dict String Model ))
    | Collection Type (Dict String Model)
    | Record Int (List Model)
    | Tuple Model Model
    | Triple Model Model Model
    | Unit
    | Primitive Primitive Value


type Primitive
    = StringPrimitive
    | CharPrimitive
    | IntPrimitive
    | FloatPrimitive
    | BoolPrimitive
    | OtherPrimitive String


type Builder formMsg appMsg output
    = Builder (Internal.Config formMsg appMsg output)


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Form"


label : String -> IR.Gadget a -> IR.Gadget a
label labelText gadget =
    tools.attach "label" Gadget.string labelText gadget


useControl : Control formMsg appMsg a -> IR.Gadget a -> IR.Gadget a
useControl (Control id _) gadget =
    tools.attach "useControl" Gadget.string id gadget



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'


-}


new : (Msg formMsg -> appMsg) -> IR.Gadget output -> Builder formMsg appMsg output
new msgToAppMsg outputGadget =
    let
        unwrap (Control _ c) =
            c
    in
    Builder
        { bool = unwrap Control.bool
        , int = unwrap Control.int
        , float = unwrap Control.float
        , char = unwrap Control.char
        , string = unwrap Control.string
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
        , outputGadget = outputGadget
        , msgToAppMsg = msgToAppMsg
        }


end :
    Builder formMsg appMsg output
    -> Form formMsg appMsg output
end (Builder config) =
    let
        outputType =
            IR.irType config.outputGadget

        path =
            []

        init_ =
            let
                ( model, cmd ) =
                    init config path outputType
            in
            ( model
            , cmd
            )

        update_ msg model =
            let
                ( newModel, cmd ) =
                    update config path outputType msg model
            in
            ( newModel
            , cmd
            )

        load_ output =
            output
                |> IR.fromInput config.outputGadget
                |> load config outputType

        notify_ formMsg model =
            update_ (FormMsg formMsg) model
                |> Tuple.first

        view_ model =
            view config path outputType model
                |> H.form []

        subscriptions_ model =
            subscriptions config path model

        submit_ model =
            submit config path outputType model
    in
    { init = init_
    , load = load_
    , update = update_
    , notify = notify_
    , view = view_
    , subscriptions = subscriptions_
    , submit = submit_
    }


init :
    Config formMsg appMsg output
    -> Path
    -> Type
    -> ( Model, Cmd appMsg )
init config path type_ =
    let
        makePrimitive primitive control =
            let
                ( mdl, cmd ) =
                    control.init
            in
            ( Primitive primitive mdl
            , Cmd.map (msgMapper config path) cmd
            )
    in
    case
        tools.extract type_
            |> tools.decode "useControl" Gadget.string
            |> Maybe.andThen
                (\id ->
                    Dict.get id config.overrides
                        |> Maybe.map (Tuple.pair id)
                )
    of
        Just ( id, control ) ->
            makePrimitive (OtherPrimitive id) control

        Nothing ->
            case type_ of
                UnitType _ ->
                    ( Unit, Cmd.none )

                IntType _ ->
                    makePrimitive IntPrimitive config.int

                FloatType _ ->
                    makePrimitive FloatPrimitive config.float

                StringType _ ->
                    makePrimitive StringPrimitive config.string

                CharType _ ->
                    makePrimitive CharPrimitive config.char

                BoolType _ ->
                    makePrimitive BoolPrimitive config.bool

                RecordType _ fields ->
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


load : Config formMsg appMsg output -> Type -> Value -> Model
load config type_ output =
    let
        loadPrimitive primitive control =
            let
                mdl =
                    control.load output
            in
            Primitive primitive mdl
    in
    case
        tools.extract type_
            |> tools.decode "useControl" Gadget.string
            |> Maybe.andThen
                (\id ->
                    Dict.get id config.overrides
                        |> Maybe.map (Tuple.pair id)
                )
    of
        Just ( id, control ) ->
            loadPrimitive (OtherPrimitive id) control

        Nothing ->
            case type_ of
                UnitType _ ->
                    Unit

                IntType _ ->
                    loadPrimitive IntPrimitive config.int

                FloatType _ ->
                    loadPrimitive FloatPrimitive config.float

                StringType _ ->
                    loadPrimitive StringPrimitive config.string

                CharType _ ->
                    loadPrimitive CharPrimitive config.char

                BoolType _ ->
                    loadPrimitive BoolPrimitive config.bool

                RecordType _ fields ->
                    let
                        out =
                            List.foldr
                                (\( _, thisField ) prevFields ->
                                    load config thisField output :: prevFields
                                )
                                []
                                fields
                    in
                    Record 0 out

                _ ->
                    Debug.todo "init"


lookupControl : Primitive -> Config formMsg appMsg output -> Maybe (InternalControl formMsg appMsg)
lookupControl primitive config =
    case primitive of
        IntPrimitive ->
            Just config.int

        StringPrimitive ->
            Just config.string

        CharPrimitive ->
            Just config.char

        FloatPrimitive ->
            Just config.float

        BoolPrimitive ->
            Just config.bool

        OtherPrimitive id ->
            Dict.get id config.overrides


update :
    Config formMsg appMsg output
    -> Path
    -> Type
    -> Msg formMsg
    -> Model
    -> ( Model, Cmd appMsg )
update config modelPath type_ msg model =
    case ( model, type_ ) of
        ( Unit, _ ) ->
            ( Unit, Cmd.none )

        ( Primitive primitive controlModel, _ ) ->
            case lookupControl primitive config of
                Just control ->
                    case msg of
                        FormMsg formMsg ->
                            ( Primitive primitive (control.notify formMsg controlModel)
                            , Cmd.none
                            )

                        ControlMsg msgPath controlMsg ->
                            if msgPath == modelPath then
                                let
                                    ( newControlModel, controlCmd ) =
                                        control.update controlMsg controlModel
                                in
                                ( Primitive primitive newControlModel
                                , Cmd.map (msgMapper config modelPath) controlCmd
                                )

                            else
                                ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( Record selected fields, RecordType _ fieldTypes ) ->
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


view :
    Config formMsg appMsg output
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
                label_ =
                    tools.extract primitiveType
                        |> tools.decode "label" Gadget.string
                        |> Maybe.withDefault id
            in
            case lookupControl primitive config of
                Just control ->
                    config.viewControl False
                        [ H.label [ HA.for id ] [ H.text label_ ]
                        , control.view id primitiveModel
                            |> H.map (msgMapper config path)
                        ]

                Nothing ->
                    [ H.text "view error: control not found" ]

        ( Record _ fieldModels, RecordType metadata fieldTypes ) ->
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


subscriptions : a -> b -> c -> d
subscriptions config path model =
    Debug.todo "subscriptions"


submit : a -> b -> c -> d -> e
submit config path type_ model =
    Debug.todo "submit"


msgMapper : Config formMsg appMsg output -> Path -> Send Value formMsg appMsg -> appMsg
msgMapper config path msg =
    case msg of
        ToSelf toSelf ->
            config.msgToAppMsg (ControlMsg path toSelf)

        ToForm toAll ->
            config.msgToAppMsg (FormMsg toAll)

        ToApp toApp ->
            toApp


registerControl :
    Control formMsg appMsg controlOutput
    -> Builder formMsg appMsg output
    -> Builder formMsg appMsg output
registerControl controlBuilder (Builder builder) =
    let
        (Control id internalControl) =
            controlBuilder
    in
    Builder { builder | overrides = Dict.insert id internalControl builder.overrides }


withControlView :
    (Bool -> List (H.Html appMsg) -> List (H.Html appMsg))
    -> Builder formMsg appMsg output
    -> Builder formMsg appMsg output
withControlView f (Builder builder) =
    Builder { builder | viewControl = f }


withFeedbackView :
    (String -> H.Html appMsg)
    -> Builder formMsg appMsg output
    -> Builder formMsg appMsg output
withFeedbackView f (Builder builder) =
    Builder { builder | viewFeedback = f }


withTopLevelView :
    ({ switchActiveField : Int -> appMsg
     , activeField : Int
     , fieldViews : List (H.Html appMsg)
     , result : Result (Set.Set Int) output
     }
     -> List (H.Html appMsg)
    )
    -> Builder formMsg appMsg output
    -> Builder formMsg appMsg output
withTopLevelView f (Builder builder) =
    Builder { builder | viewTopLevel = f }


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"
