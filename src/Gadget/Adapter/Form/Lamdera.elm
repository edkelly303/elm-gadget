module Gadget.Adapter.Form.Lamdera exposing
    ( Config
    , Control
    , ControlDefinition
    , Either(..)
    , Form
    , Model
    , Msg
    , customLabels
    , defaultConfig
    , fromGadget
    , fromGadgetWithConfig
    , label
    , makeControl
    )

import Dict exposing (Dict)
import Gadget
import Gadget.IR as IR exposing (Error, Path, Type(..), Value(..), VariantType(..), VariantValue(..))
import Html as H
import Html.Attributes as HA exposing (value)
import Html.Events as HE
import List.Extra
import Result.Extra


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Form"


{-| A record of functions that you can plumb into a standard Elm application to
manage the lifecycle of a form.
-}
type alias Form backendModel backendMsg msg a =
    { init : ( Model, Cmd msg )
    , load : a -> Model
    , update : Msg -> Model -> ( Model, Cmd msg )
    , updateFromBackend : Msg -> Model -> ( Model, Cmd msg )
    , view : Model -> H.Html msg
    , subscriptions : Model -> Sub msg
    , submit : Model -> Result (List Error) a
    , respond : String -> Msg -> IR.Gadget backendModel -> backendModel -> Cmd backendMsg
    }


{-| The internal state of a form - as a user of this module, you don't need to
worry about the details, so this is an opaque type.
-}
type Model
    = Sum String IR.Metadata (Dict String ( Int, Dict String Model ))
    | Collection IR.Metadata Type (Dict String Model)
    | Record IR.Metadata (Dict String ( Int, Model ))
    | Tuple IR.Metadata Model Model
    | Triple IR.Metadata Model Model Model
    | Unit
    | Primitive PrimitiveType IR.Metadata Value


type PrimitiveType
    = PString
    | PChar
    | PInt
    | PFloat
    | PBool


{-| The internal messages used to update the form - as a user of this module,
you don't need to worry about the details, so this is an opaque type.
-}
type Msg
    = Msg Path Value


{-| Convert a `Gadget` into a `Form`.
-}
fromGadget :
    (Msg -> msg)
    -> (Msg -> Cmd msg)
    -> (String -> Msg -> Cmd backendMsg)
    -> IR.Gadget a
    -> Form backendModel backendMsg msg a
fromGadget mkMsg mkToBackend mkToFrontend gadget =
    fromGadgetWithConfig defaultConfig mkMsg mkToBackend mkToFrontend gadget


{-| Convert a `Gadget` into a `Form`, supplying a `Config`.
-}
fromGadgetWithConfig :
    Config
    -> (Msg -> msg)
    -> (Msg -> Cmd msg)
    -> (String -> Msg -> Cmd backendMsg)
    -> IR.Gadget a
    -> Form backendModel backendMsg msg a
fromGadgetWithConfig config mkMsg mkToBackend mkToFrontend gadget =
    { init = init config gadget |> Tuple.mapSecond (Cmd.map mkMsg)
    , load = \output -> load config gadget output
    , update =
        \msg model ->
            let
                ( newModel, either ) =
                    update .update config msg model
            in
            ( newModel
            , case either of
                Cmd cmd ->
                    Cmd.map mkMsg cmd

                ToBackend toBackend ->
                    mkToBackend toBackend
            )
    , updateFromBackend =
        \msg model ->
            let
                ( newModel, either ) =
                    update .updateFromBackend config msg model
            in
            ( newModel
            , case either of
                Cmd cmd ->
                    Cmd.map mkMsg cmd

                ToBackend toBackend ->
                    mkToBackend toBackend
            )
    , view = \model -> view config gadget model |> H.map mkMsg
    , subscriptions = \model -> subscriptions config model |> Sub.map mkMsg
    , submit = submit config gadget
    , respond =
        \sessionId toBackend valueGadget value ->
            respond config toBackend gadget (IR.fromInput valueGadget value)
                |> mkToFrontend sessionId
    }


{-| A configuration record that you can use to tweak various details of how to
convert a `Gadget` into a `Form`. For example, you can specify the types of form
controls you would like to use for each primitive type `(Bool`, `Int`, `Float`,
`Char`, `String`), you can configure how controls are laid out and how feedback
is formatted, and so on.
-}
type alias Config =
    { bool : Control Bool
    , int : Control Int
    , float : Control Float
    , char : Control Char
    , string : Control String
    , viewFeedback : String -> H.Html Msg
    , viewControl : Bool -> List (H.Html Msg) -> List (H.Html Msg)
    }


{-| The default configuration record for forms.

`fromGadget == fromGadgetWithConfig defaultConfig`

-}
defaultConfig : Config
defaultConfig =
    { bool = bool
    , int = int
    , float = float
    , char = char
    , string = string
    , viewFeedback = \error -> H.span [] [ H.text error ]
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
    }


{-| A custom form control.
-}
type Control output
    = Control InnerControl


type alias InnerControl =
    { init : ( Value, Cmd Value )
    , load : Value -> Value
    , placeholder : Value
    , update : Value -> Value -> ( Value, Either Value Value )
    , updateFromBackend : Value -> Value -> ( Value, Either Value Value )
    , view : String -> Value -> H.Html Value
    , subscriptions : Value -> Sub Value
    , layout : { label : H.Html Msg, input : H.Html Msg, feedback : H.Html Msg } -> List (H.Html Msg)
    , submit : Path -> Value -> Result (List Error) Value
    , respond : Value -> Value -> Value
    }


{-| A definition for a custom form control.
-}
type alias ControlDefinition toBackend toFrontend backendModel msg model output =
    { msg : IR.Gadget msg
    , model : IR.Gadget model
    , output : IR.Gadget output
    , toBackend : IR.Gadget toBackend
    , toFrontend : IR.Gadget toFrontend
    , backendModel : IR.Gadget backendModel
    , init : ( model, Cmd msg )
    , placeholder : output
    , load : output -> model
    , update : msg -> model -> ( model, Either msg toBackend )
    , updateFromBackend : toFrontend -> model -> ( model, Either msg toBackend )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , respond : toBackend -> backendModel -> toFrontend
    }


type Either a b
    = Cmd (Cmd a)
    | ToBackend b


{-| Turn a `ControlDefinition` into a `Control`.
-}
makeControl : ControlDefinition toBackend toFrontend backendModel msg model output -> Control output
makeControl config =
    let
        placeholderValue =
            config.placeholder
                |> config.load
                |> IR.fromInput config.model
    in
    Control
        { init = config.init |> Tuple.mapBoth (IR.fromInput config.model) (Cmd.map (IR.fromInput config.msg))
        , load =
            \outputValue ->
                IR.toOutput config.output outputValue
                    |> Result.map (\output -> config.load output)
                    |> Result.map (IR.fromInput config.model)
                    |> Result.withDefault placeholderValue
        , placeholder = IR.fromInput config.output config.placeholder
        , update =
            \msg modelValue ->
                let
                    result =
                        Result.map2 config.update
                            (IR.toOutput config.msg msg)
                            (IR.toOutput config.model modelValue)
                in
                case result of
                    Ok ( model, either ) ->
                        ( IR.fromInput config.model model
                        , case either of
                            Cmd cmd ->
                                Cmd (Cmd.map (IR.fromInput config.msg) cmd)

                            ToBackend toBackend ->
                                ToBackend (IR.fromInput config.toBackend toBackend)
                        )

                    Err _ ->
                        ( modelValue
                        , Cmd Cmd.none
                        )
        , updateFromBackend =
            \msg modelValue ->
                let
                    result =
                        Result.map2 config.updateFromBackend
                            (IR.toOutput config.toFrontend msg)
                            (IR.toOutput config.model modelValue)
                in
                case result of
                    Ok ( model, either ) ->
                        ( IR.fromInput config.model model
                        , case either of
                            Cmd cmd ->
                                Cmd (Cmd.map (IR.fromInput config.msg) cmd)

                            ToBackend toBackend ->
                                ToBackend (IR.fromInput config.toBackend toBackend)
                        )

                    Err _ ->
                        ( modelValue
                        , Cmd Cmd.none
                        )
        , view =
            \id modelValue ->
                Result.map (config.view id) (IR.toOutput config.model modelValue)
                    |> Result.Extra.extract (List.map (.error >> H.text) >> H.div [])
                    |> H.map (\msg -> IR.fromInput config.msg msg)
        , layout =
            \ui ->
                [ ui.label, ui.input, ui.feedback ]
        , subscriptions = \_ -> Sub.none
        , submit =
            \path modelValue ->
                IR.toOutput config.model modelValue
                    |> Result.andThen
                        (\model ->
                            config.submit model
                                |> Result.mapError (\error -> [ { error = error, path = path } ])
                                |> Result.map (IR.fromInput config.output)
                        )
        , respond =
            \toBackend backendModel ->
                Result.map2 config.respond
                    (IR.toOutput config.toBackend toBackend)
                    (IR.toOutput config.backendModel backendModel)
                    |> Result.map (IR.fromInput config.toFrontend)
                    |> Result.withDefault IR.UnitValue
        }


withLayout : ({ label : H.Html Msg, input : H.Html Msg, feedback : H.Html Msg } -> List (H.Html Msg)) -> Control output -> Control output
withLayout f (Control c) =
    Control { c | layout = f }


{-| Add a label to a `Gadget` - this will be displayed as an HTML `<label>` element
-}
label : String -> IR.Gadget a -> IR.Gadget a
label l gadget =
    tools.attach "label" Gadget.string l gadget


{-| Add labels to a custom type `Gadget` - this allows you to define a label for
the custom type itself (displayed as an HTML `<legend>` within a `<fieldset>`),
and for each of its variants (displayed as HTML `<input type="radio">` buttons).
-}
customLabels : String -> List String -> IR.Gadget a -> IR.Gadget a
customLabels l ls gadget =
    tools.attach "customLabel"
        (Gadget.tuple Gadget.string (Gadget.list Gadget.string))
        ( l, ls )
        gadget


init : Config -> IR.Gadget a -> ( Model, Cmd Msg )
init config gadget =
    initHelp config [] (IR.irType gadget)


initHelp : Config -> Path -> Type -> ( Model, Cmd Msg )
initHelp config path irType =
    let
        initFor getType primitiveType metadata =
            let
                (Control c) =
                    getType config
            in
            c.init
                |> Tuple.mapBoth
                    (Primitive primitiveType metadata)
                    (Cmd.map (Msg path))
    in
    case irType of
        UnitType _ ->
            ( Unit, Cmd.none )

        BoolType m ->
            initFor .bool PBool m

        CharType m ->
            initFor .char PChar m

        StringType m ->
            initFor .string PString m

        IntType m ->
            initFor .int PInt m

        FloatType m ->
            initFor .float PFloat m

        CustomType m ( firstName, firstVariantType ) restNamesAndVariantTypes ->
            let
                variantTypes =
                    ( firstName, firstVariantType ) :: restNamesAndVariantTypes

                ( namedVariantModels, variantCmds ) =
                    variantTypes
                        |> List.indexedMap
                            (\idx ( variantName, variantType ) ->
                                variantType
                                    |> variantTypeToArgsList
                                    |> List.map
                                        (\( argName, argType ) ->
                                            let
                                                argPath =
                                                    argName :: variantName :: path

                                                ( argModel, argCmd ) =
                                                    initHelp config argPath argType
                                            in
                                            ( ( argName, argModel ), argCmd )
                                        )
                                    |> List.unzip
                                    |> Tuple.mapFirst
                                        (\list -> ( variantName, ( idx, Dict.fromList list ) ))
                            )
                        |> List.unzip
                        |> Tuple.mapBoth Dict.fromList List.concat
            in
            ( Sum firstName m namedVariantModels, Cmd.batch variantCmds )

        RecordType m namedFieldTypes ->
            let
                ( namedFieldModels, fieldCmds ) =
                    namedFieldTypes
                        |> List.indexedMap
                            (\idx ( fieldName, fieldType ) ->
                                let
                                    ( fieldModel, fieldCmd ) =
                                        initHelp config (fieldName :: path) fieldType
                                in
                                ( ( fieldName, ( idx, fieldModel ) ), fieldCmd )
                            )
                        |> List.unzip
                        |> Tuple.mapFirst Dict.fromList
            in
            ( Record m namedFieldModels, Cmd.batch fieldCmds )

        ListType m innerType ->
            ( Collection m innerType Dict.empty, Cmd.none )

        LazyType _ innerType ->
            initHelp config path (innerType ())

        TupleType m a b ->
            let
                ( aModel, aCmd ) =
                    initHelp config ("0" :: path) a

                ( bModel, bCmd ) =
                    initHelp config ("1" :: path) b
            in
            ( Tuple m aModel bModel, Cmd.batch [ aCmd, bCmd ] )

        TripleType m a b c ->
            let
                ( aModel, aCmd ) =
                    initHelp config ("0" :: path) a

                ( bModel, bCmd ) =
                    initHelp config ("1" :: path) b

                ( cModel, cCmd ) =
                    initHelp config ("2" :: path) c
            in
            ( Triple m aModel bModel cModel, Cmd.batch [ aCmd, bCmd, cCmd ] )


respond : Config -> Msg -> IR.Gadget a -> Value -> Msg
respond config toBackend gadget value =
    let
        ( model, _ ) =
            init config gadget
    in
    respondHelp config [] toBackend model value


respondHelp : Config -> Path -> Msg -> Model -> Value -> Msg
respondHelp config modelPath ((Msg msgPath msgValue) as msg) model value =
    let
        _ =
            Debug.log "msg" msg

        _ =
            Debug.log "value" value

        _ =
            Debug.log "modelPath" modelPath
    in
    case model of
        Unit ->
            Msg msgPath UnitValue

        Primitive primitiveType _ _ ->
            if modelPath == msgPath then
                let
                    respondFor getType =
                        let
                            (Control c) =
                                getType config
                        in
                        c.respond msgValue value

                    toFrontend =
                        case primitiveType of
                            PString ->
                                respondFor .string

                            PChar ->
                                respondFor .char

                            PInt ->
                                respondFor .int

                            PFloat ->
                                respondFor .float

                            PBool ->
                                respondFor .bool
                in
                Msg modelPath toFrontend

            else
                Msg [ "primitiveFailed" ] UnitValue

        Record _ fields ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    Msg [] UnitValue

                PrefixMatch { next1 } ->
                    case Dict.get next1 fields of
                        Just ( _, oldField ) ->
                            respondHelp config (next1 :: modelPath) msg oldField value

                        Nothing ->
                            Msg [ "record no field" ] UnitValue

                NoMatch ->
                    Msg [ "record no match" ] UnitValue

        Tuple _ a b ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    Msg [ "tuple full match" ] UnitValue

                PrefixMatch { next1 } ->
                    case next1 of
                        "0" ->
                            respondHelp config ("0" :: modelPath) msg a value

                        "1" ->
                            respondHelp config ("1" :: modelPath) msg b value

                        _ ->
                            Msg [ "tuple field doesn't exist" ] UnitValue

                NoMatch ->
                    Msg [ "tuple no match" ] UnitValue

        Triple _ a b c ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    Msg [ "triple full match" ] UnitValue

                PrefixMatch { next1 } ->
                    case next1 of
                        "0" ->
                            respondHelp config ("0" :: modelPath) msg a value

                        "1" ->
                            respondHelp config ("1" :: modelPath) msg b value

                        "2" ->
                            respondHelp config ("2" :: modelPath) msg c value

                        _ ->
                            Msg [ "triple field doesn't exist" ] UnitValue

                NoMatch ->
                    Msg [ "triple no match" ] UnitValue

        Collection _ itemType _ ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    Msg [ "collection full match" ] UnitValue

                PrefixMatch { next1 } ->
                    respondHelp
                        config
                        (next1 :: modelPath)
                        msg
                        (initHelp config (next1 :: modelPath) itemType |> Tuple.first)
                        value

                NoMatch ->
                    Msg [ "collection no match" ] UnitValue

        Sum _ _ variants ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    Msg [ "sum full match" ] UnitValue

                PrefixMatch { next1, next2 } ->
                    case Dict.get next1 variants of
                        Just ( _, args ) ->
                            case Dict.get next2 args of
                                Just arg ->
                                    respondHelp config (next2 :: next1 :: modelPath) msg arg value

                                Nothing ->
                                    Msg [ "sum arg not found" ] UnitValue

                        Nothing ->
                            Msg [ "sum variant not found" ] UnitValue

                NoMatch ->
                    Msg [ "sum no match" ] UnitValue


update : (InnerControl -> Value -> Value -> ( Value, Either Value Value )) -> Config -> Msg -> Model -> ( Model, Either Msg Msg )
update updater config msg model =
    updateHelp updater config [] msg model


updateHelp : (InnerControl -> Value -> Value -> ( Value, Either Value Value )) -> Config -> Path -> Msg -> Model -> ( Model, Either Msg Msg )
updateHelp updater config modelPath ((Msg msgPath msgValue) as msg) model =
    case model of
        Unit ->
            ( model, Cmd Cmd.none )

        Primitive primitiveType metadata modelValue ->
            if modelPath == msgPath then
                let
                    updateFor getType =
                        let
                            (Control c) =
                                getType config
                        in
                        updater c msgValue modelValue

                    ( newModel, either ) =
                        case primitiveType of
                            PString ->
                                updateFor .string

                            PChar ->
                                updateFor .char

                            PInt ->
                                updateFor .int

                            PFloat ->
                                updateFor .float

                            PBool ->
                                updateFor .bool
                in
                ( Primitive primitiveType metadata newModel
                , case either of
                    Cmd cmd ->
                        Cmd (Cmd.map (Msg modelPath) cmd)

                    ToBackend toBackend ->
                        ToBackend (Msg modelPath toBackend)
                )

            else
                ( model, Cmd Cmd.none )

        Record metadata fields ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    ( model, Cmd Cmd.none )

                PrefixMatch { next1 } ->
                    case Dict.get next1 fields of
                        Just ( idx, oldField ) ->
                            let
                                ( newField, cmd ) =
                                    updateHelp updater config (next1 :: modelPath) msg oldField

                                newFields =
                                    Dict.insert next1
                                        ( idx, newField )
                                        fields
                            in
                            ( Record metadata newFields, cmd )

                        Nothing ->
                            ( model, Cmd Cmd.none )

                NoMatch ->
                    ( model, Cmd Cmd.none )

        Tuple metadata a b ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    ( model, Cmd Cmd.none )

                PrefixMatch { next1 } ->
                    case next1 of
                        "0" ->
                            let
                                ( new, cmd ) =
                                    updateHelp updater config ("0" :: modelPath) msg a
                            in
                            ( Tuple metadata new b, cmd )

                        "1" ->
                            let
                                ( new, cmd ) =
                                    updateHelp updater config ("1" :: modelPath) msg b
                            in
                            ( Tuple metadata a new, cmd )

                        _ ->
                            ( model, Cmd Cmd.none )

                NoMatch ->
                    ( model, Cmd Cmd.none )

        Triple metadata a b c ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    ( model, Cmd Cmd.none )

                PrefixMatch { next1 } ->
                    case next1 of
                        "0" ->
                            let
                                ( new, cmd ) =
                                    updateHelp updater config ("0" :: modelPath) msg a
                            in
                            ( Triple metadata new b c, cmd )

                        "1" ->
                            let
                                ( new, cmd ) =
                                    updateHelp updater config ("1" :: modelPath) msg b
                            in
                            ( Triple metadata a new c, cmd )

                        "2" ->
                            let
                                ( new, cmd ) =
                                    updateHelp updater config ("2" :: modelPath) msg c
                            in
                            ( Triple metadata a b new, cmd )

                        _ ->
                            ( model, Cmd Cmd.none )

                NoMatch ->
                    ( model, Cmd Cmd.none )

        Collection metadata innerType itemModels ->
            let
                ( newItemModels, itemCmd ) =
                    case matchPath msgPath modelPath of
                        FullMatch ->
                            case msgValue of
                                UnitValue ->
                                    let
                                        ( newItemModel, newCmd ) =
                                            initHelp config modelPath innerType
                                    in
                                    ( Dict.insert (String.fromInt (Dict.size itemModels)) newItemModel itemModels
                                    , Cmd newCmd
                                    )

                                _ ->
                                    ( itemModels, Cmd Cmd.none )

                        PrefixMatch { next1 } ->
                            case Dict.get next1 itemModels of
                                Just oldItemModel ->
                                    let
                                        ( newItemModel, newCmd ) =
                                            updateHelp updater config (next1 :: modelPath) msg oldItemModel
                                    in
                                    ( Dict.insert next1 newItemModel itemModels
                                    , newCmd
                                    )

                                Nothing ->
                                    ( itemModels, Cmd Cmd.none )

                        NoMatch ->
                            ( itemModels, Cmd Cmd.none )
            in
            ( Collection metadata innerType newItemModels, itemCmd )

        Sum selected metadata variants ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    case msgValue of
                        StringValue newSelected ->
                            ( Sum newSelected metadata variants, Cmd Cmd.none )

                        _ ->
                            ( model, Cmd Cmd.none )

                PrefixMatch { next1, next2 } ->
                    case Dict.get next1 variants of
                        Just ( idx, args ) ->
                            case Dict.get next2 args of
                                Just arg ->
                                    let
                                        ( newArg, cmd ) =
                                            updateHelp updater config (next2 :: next1 :: modelPath) msg arg

                                        newVariants =
                                            Dict.insert next1
                                                ( idx, Dict.insert next2 newArg args )
                                                variants
                                    in
                                    ( Sum selected metadata newVariants
                                    , cmd
                                    )

                                Nothing ->
                                    ( model, Cmd Cmd.none )

                        Nothing ->
                            ( model, Cmd Cmd.none )

                NoMatch ->
                    ( model, Cmd Cmd.none )


view : Config -> IR.Gadget a -> Model -> H.Html Msg
view config gadget model =
    let
        errs =
            case submit config gadget model of
                Ok _ ->
                    []

                Err errs_ ->
                    errs_
    in
    H.form [] (viewHelp config errs [] model)


viewHelp : Config -> List Error -> Path -> Model -> List (H.Html Msg)
viewHelp config errs modelPath model =
    let
        id =
            pathToString modelPath

        maybeLabel metadata =
            tools.decode "label" Gadget.string metadata

        feedback =
            errs
                |> List.concatMap
                    (\{ path, error } ->
                        if path == modelPath then
                            [ config.viewFeedback error ]

                        else
                            []
                    )

        isValid =
            List.isEmpty feedback
    in
    case model of
        Unit ->
            []

        Primitive primitiveType metadata modelValue ->
            let
                viewMe getType =
                    let
                        (Control c) =
                            getType config
                    in
                    config.viewControl isValid <|
                        c.layout
                            { label =
                                H.label [ HA.for id ] [ H.text (maybeLabel metadata |> Maybe.withDefault id) ]
                            , input =
                                c.view id modelValue |> H.map (Msg modelPath)
                            , feedback =
                                H.output [] feedback
                            }
            in
            case primitiveType of
                PString ->
                    viewMe .string

                PChar ->
                    viewMe .char

                PInt ->
                    viewMe .int

                PFloat ->
                    viewMe .float

                PBool ->
                    viewMe .bool

        Record metadata fields ->
            let
                inner =
                    Dict.toList fields
                        |> List.sortBy (\( _, ( idx, _ ) ) -> idx)
                        |> List.concatMap (\( name, ( _, childModel ) ) -> viewHelp config errs (name :: modelPath) childModel)
            in
            case maybeLabel metadata of
                Nothing ->
                    inner ++ feedback

                Just label_ ->
                    H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) :: feedback

        Tuple metadata a b ->
            let
                inner =
                    viewHelp config errs ("0" :: modelPath) a
                        ++ viewHelp config errs ("1" :: modelPath) b
            in
            case maybeLabel metadata of
                Nothing ->
                    inner ++ feedback

                Just label_ ->
                    H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) :: feedback

        Triple metadata a b c ->
            let
                inner =
                    viewHelp config errs ("0" :: modelPath) a
                        ++ viewHelp config errs ("1" :: modelPath) b
                        ++ viewHelp config errs ("2" :: modelPath) c
            in
            case maybeLabel metadata of
                Nothing ->
                    inner ++ feedback

                Just label_ ->
                    H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) :: feedback

        Collection metadata _ childModels ->
            [ H.fieldset []
                (case maybeLabel metadata of
                    Nothing ->
                        List.concat
                            [ [ H.input [ HA.type_ "button", HE.onClick (Msg modelPath UnitValue), HA.value "Add an item" ] [] ]
                            , childModels
                                |> Dict.map (\idx childModel -> viewHelp config errs (idx :: modelPath) childModel)
                                |> Dict.values
                                |> List.concat
                            , feedback
                            ]

                    Just legend ->
                        List.concat
                            [ [ H.legend [] [ H.text legend ] ]
                            , [ H.input [ HA.type_ "button", HE.onClick (Msg modelPath UnitValue), HA.value "Add an item" ] [] ]
                            , childModels
                                |> Dict.map (\idx childModel -> viewHelp config errs (idx :: modelPath) childModel)
                                |> Dict.values
                                |> List.concat
                            , feedback
                            ]
                )
            ]

        Sum selected metadata childModels ->
            case Dict.get selected childModels of
                Nothing ->
                    [ H.text "ERROR! Missing variant" ]

                Just ( _, variant ) ->
                    let
                        childView =
                            Dict.map (\idx arg -> viewHelp config errs (idx :: selected :: modelPath) arg) variant
                                |> Dict.values
                                |> List.concat

                        ( customLabel_, variantLabels ) =
                            tools.decode "customLabel" (Gadget.tuple Gadget.string (Gadget.list Gadget.string)) metadata
                                |> Maybe.withDefault ( pathToString modelPath, [] )
                    in
                    (H.fieldset [ HA.class "custom-variant-selector" ]
                        (H.legend [] [ H.text customLabel_ ]
                            :: (childModels
                                    |> Dict.map
                                        (\name ( idx, _ ) ->
                                            let
                                                childId =
                                                    pathToString (name :: modelPath)
                                            in
                                            H.span []
                                                [ H.input
                                                    [ HA.id childId
                                                    , HA.name id
                                                    , HA.type_ "radio"
                                                    , HE.onCheck (\_ -> Msg modelPath (StringValue name))
                                                    , HA.checked (selected == name)
                                                    ]
                                                    []
                                                , H.label [ HA.for childId ]
                                                    [ H.text
                                                        (List.Extra.getAt idx variantLabels
                                                            |> Maybe.withDefault (maybeLabel metadata |> Maybe.withDefault "")
                                                        )
                                                    ]
                                                ]
                                        )
                                    |> Dict.values
                               )
                        )
                        :: childView
                    )
                        ++ feedback


subscriptions : Config -> Model -> Sub Msg
subscriptions config model =
    subscriptionsHelp config [] model


subscriptionsHelp : Config -> Path -> Model -> Sub Msg
subscriptionsHelp config path model =
    case model of
        Unit ->
            Sub.none

        Primitive primitiveType _ modelValue ->
            let
                subMe getType =
                    let
                        (Control c) =
                            getType config
                    in
                    c.subscriptions modelValue
                        |> Sub.map (Msg path)
            in
            case primitiveType of
                PBool ->
                    subMe .bool

                PInt ->
                    subMe .int

                PFloat ->
                    subMe .float

                PChar ->
                    subMe .char

                PString ->
                    subMe .string

        Tuple _ a b ->
            Sub.batch
                [ subscriptionsHelp config ("0" :: path) a
                , subscriptionsHelp config ("1" :: path) b
                ]

        Triple _ a b c ->
            Sub.batch
                [ subscriptionsHelp config ("0" :: path) a
                , subscriptionsHelp config ("1" :: path) b
                , subscriptionsHelp config ("2" :: path) c
                ]

        Record _ namedFieldModels ->
            namedFieldModels
                |> Dict.map (\fieldName ( _, fieldModel ) -> subscriptionsHelp config (fieldName :: path) fieldModel)
                |> Dict.values
                |> Sub.batch

        Sum _ _ variantModels ->
            variantModels
                |> Dict.toList
                |> List.concatMap
                    (\( variantName, ( _, argModels ) ) ->
                        argModels
                            |> Dict.toList
                            |> List.map
                                (\( argName, argModel ) ->
                                    subscriptionsHelp config (argName :: variantName :: path) argModel
                                )
                    )
                |> Sub.batch

        Collection _ _ itemModels ->
            itemModels
                |> Dict.map (\itemName itemModel -> subscriptionsHelp config (itemName :: path) itemModel)
                |> Dict.values
                |> Sub.batch


submit : Config -> IR.Gadget a -> Model -> Result (List Error) a
submit config gadget model =
    let
        ( parsedValue, parsingErrors ) =
            parsePrimitiveControls config [] model
    in
    case ( parsingErrors, IR.toOutput gadget parsedValue ) of
        ( [], Ok output ) ->
            Ok output

        ( _, Ok _ ) ->
            Err parsingErrors

        ( [], Err validationErrors ) ->
            Err validationErrors

        ( _, Err validationErrors ) ->
            let
                parsingErrorPaths =
                    parsingErrors
                        |> List.map .path
                        |> List.Extra.unique

                filteredValidationErrors =
                    -- don't keep validation errors for paths that are
                    -- ancestors of parsing errors (because these
                    -- validation errors will potentially be based on
                    -- dummy values, so they should be discarded)
                    List.filter
                        (\validationError ->
                            parsingErrorPaths
                                |> List.any
                                    (\parsingErrorPath ->
                                        validationError.path |> pathIsAncestorOf parsingErrorPath
                                    )
                                |> not
                        )
                        validationErrors
            in
            Err (parsingErrors ++ filteredValidationErrors)


parsePrimitiveControls : Config -> Path -> Model -> ( Value, List Error )
parsePrimitiveControls config path model =
    case model of
        Unit ->
            ( UnitValue, [] )

        Primitive primitiveType _ modelValue ->
            let
                submit_ getter =
                    let
                        (Control c) =
                            getter config
                    in
                    case c.submit path modelValue of
                        Ok v ->
                            ( v, [] )

                        Err errs ->
                            ( c.placeholder, errs )
            in
            case primitiveType of
                PString ->
                    submit_ .string

                PChar ->
                    submit_ .char

                PInt ->
                    submit_ .int

                PFloat ->
                    submit_ .float

                PBool ->
                    submit_ .bool

        Record _ fields ->
            fields
                |> Dict.toList
                |> List.sortBy (\( _, ( idx, _ ) ) -> idx)
                |> List.foldr
                    (\( name, ( _, child ) ) ( namedValues, errs ) ->
                        let
                            ( value, thisErrs ) =
                                parsePrimitiveControls config (name :: path) child
                        in
                        ( ( name, value ) :: namedValues, thisErrs ++ errs )
                    )
                    ( [], [] )
                |> Tuple.mapFirst IR.RecordValue

        Tuple _ a b ->
            let
                ( aValue, aErrs ) =
                    parsePrimitiveControls config ("0" :: path) a

                ( bValue, bErrs ) =
                    parsePrimitiveControls config ("1" :: path) b
            in
            ( TupleValue aValue bValue, aErrs ++ bErrs )

        Triple _ a b c ->
            let
                ( aValue, aErrs ) =
                    parsePrimitiveControls config ("0" :: path) a

                ( bValue, bErrs ) =
                    parsePrimitiveControls config ("1" :: path) b

                ( cValue, cErrs ) =
                    parsePrimitiveControls config ("2" :: path) c
            in
            ( TripleValue aValue bValue cValue, aErrs ++ bErrs ++ cErrs )

        Collection _ _ items ->
            (items
                |> Dict.map (\idx item -> parsePrimitiveControls config (idx :: path) item)
            )
                |> Dict.foldr (\_ ( thisValue, thisErrs ) ( values, errs ) -> ( thisValue :: values, thisErrs ++ errs )) ( [], [] )
                |> Tuple.mapFirst IR.ListValue

        Sum selected _ variants ->
            case Dict.get selected variants of
                Nothing ->
                    -- should be impossible...
                    ( UnitValue, [ { path = path, error = "Invalid Sum variant selection" } ] )

                Just ( idx, variant ) ->
                    let
                        ( argsList, argsErrs ) =
                            (variant
                                |> Dict.map (\argIdx arg -> parsePrimitiveControls config (argIdx :: selected :: path) arg)
                            )
                                |> Dict.foldr (\_ ( thisValue, thisErrs ) ( values, errs ) -> ( thisValue :: values, thisErrs ++ errs )) ( [], [] )
                    in
                    case argsListToVariantValue argsList of
                        Err errs ->
                            ( CustomValue idx ( selected, Variant0Value ), { path = selected :: path, error = errs } :: argsErrs )

                        Ok variantValue ->
                            ( CustomValue idx ( selected, variantValue ), argsErrs )


load : Config -> IR.Gadget a -> a -> Model
load config gadget a =
    loadHelp config (IR.fromInput gadget a) (IR.irType gadget)


loadHelp : Config -> Value -> Type -> Model
loadHelp config value type_ =
    let
        loadMe typ metadata getType =
            let
                (Control c) =
                    getType config
            in
            Primitive typ metadata (c.load value)
    in
    case ( value, type_ ) of
        ( UnitValue, UnitType _ ) ->
            Unit

        ( BoolValue _, BoolType metadata ) ->
            loadMe PBool metadata .bool

        ( CharValue _, CharType metadata ) ->
            loadMe PChar metadata .char

        ( StringValue _, StringType metadata ) ->
            loadMe PString metadata .string

        ( IntValue _, IntType metadata ) ->
            loadMe PInt metadata .int

        ( FloatValue _, FloatType metadata ) ->
            loadMe PFloat metadata .float

        ( RecordValue namedFieldValues, RecordType metadata namedFieldTypes ) ->
            List.Extra.zip namedFieldValues namedFieldTypes
                |> List.indexedMap (\idx ( ( name, fieldValue ), ( _, fieldType ) ) -> ( name, ( idx, loadHelp config fieldValue fieldType ) ))
                |> Dict.fromList
                |> Record metadata

        ( CustomValue selected ( name, variantValue ), CustomType metadata firstNameAndVariantType restNamesAndVariantTypes ) ->
            let
                blank =
                    initHelp config [] type_
                        |> Tuple.first
            in
            case blank of
                Sum _ _ variantModels ->
                    let
                        argValues =
                            variantValueToArgsList variantValue

                        argTypes =
                            List.Extra.getAt selected (firstNameAndVariantType :: restNamesAndVariantTypes)
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault Variant0Type
                                |> variantTypeToArgsList

                        newArgsDict =
                            List.map2
                                (\( argName, argValue ) ( _, argType ) -> ( argName, loadHelp config argValue argType ))
                                argValues
                                argTypes
                                |> Dict.fromList
                    in
                    Sum name metadata (Dict.insert name ( selected, newArgsDict ) variantModels)

                _ ->
                    blank

        ( ListValue itemValues, ListType metadata itemType ) ->
            List.indexedMap (\idx itemValue -> ( String.fromInt idx, loadHelp config itemValue itemType )) itemValues
                |> Dict.fromList
                |> Collection metadata itemType

        ( TupleValue aValue bValue, TupleType metadata aType bType ) ->
            Tuple metadata (loadHelp config aValue aType) (loadHelp config bValue bType)

        ( TripleValue aValue bValue cValue, TripleType metadata aType bType cType ) ->
            Triple metadata (loadHelp config aValue aType) (loadHelp config bValue bType) (loadHelp config cValue cType)

        _ ->
            Unit


int : Control Int
int =
    makeControl
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.int
        , init = ( "", Cmd.none )
        , placeholder = 0
        , load = String.fromInt
        , toBackend = Gadget.unit
        , toFrontend = Gadget.unit
        , backendModel = Gadget.fail
        , respond = \toBackend backendModel -> ()
        , update = \msg _ -> ( msg, Cmd Cmd.none )
        , updateFromBackend = \_ model -> ( model, Cmd Cmd.none )
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
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "This must be an integer"
        }


float : Control Float
float =
    makeControl
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.float
        , init = ( "", Cmd.none )
        , placeholder = 0.0
        , load = String.fromFloat
        , toBackend = Gadget.unit
        , toFrontend = Gadget.unit
        , backendModel = Gadget.fail
        , respond = \toBackend backendModel -> ()
        , update = \msg _ -> ( msg, Cmd Cmd.none )
        , updateFromBackend = \_ model -> ( model, Cmd Cmd.none )
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
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.toFloat model
                    |> Result.fromMaybe "This must be a decimal number"
        }


string : Control String
string =
    makeControl
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.string
        , init = ( "", Cmd.none )
        , placeholder = ""
        , load = identity
        , toBackend = Gadget.unit
        , toFrontend = Gadget.unit
        , backendModel = Gadget.fail
        , respond = \toBackend backendModel -> ()
        , update = \msg _ -> ( msg, Cmd Cmd.none )
        , updateFromBackend = \_ model -> ( model, Cmd Cmd.none )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit = Ok
        }


bool : Control Bool
bool =
    makeControl
        { model = Gadget.bool
        , msg = Gadget.bool
        , output = Gadget.bool
        , init = ( False, Cmd.none )
        , placeholder = False
        , load = identity
        , toBackend = Gadget.unit
        , toFrontend = Gadget.unit
        , backendModel = Gadget.fail
        , respond = \toBackend backendModel -> ()
        , update = \msg _ -> ( msg, Cmd Cmd.none )
        , updateFromBackend = \_ model -> ( model, Cmd Cmd.none )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "checkbox"
                    , HE.onCheck identity
                    , HA.checked model
                    , HA.id id
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit = Ok
        }
        |> withLayout (\ui -> [ ui.input, ui.label, ui.feedback ])


char : Control Char
char =
    makeControl
        { model = Gadget.string
        , msg = Gadget.maybe Gadget.char
        , output = Gadget.char
        , init = ( "", Cmd.none )
        , placeholder = 'a'
        , load = String.fromChar
        , toBackend = Gadget.unit
        , toFrontend = Gadget.unit
        , backendModel = Gadget.fail
        , respond = \toBackend backendModel -> ()
        , update =
            \msg _ ->
                case msg of
                    Nothing ->
                        ( "", Cmd Cmd.none )

                    Just c ->
                        ( String.fromChar c, Cmd Cmd.none )
        , updateFromBackend = \_ model -> ( model, Cmd Cmd.none )
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput (\str -> String.uncons str |> Maybe.map Tuple.first)
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , subscriptions = \_ -> Sub.none
        , submit =
            \model ->
                String.uncons model
                    |> Maybe.map Tuple.first
                    |> Result.fromMaybe "This must not be blank"
        }


argsListToVariantValue : List Value -> Result String IR.VariantValue
argsListToVariantValue l =
    case l of
        [] ->
            Ok IR.Variant0Value

        [ arg1 ] ->
            Ok <| IR.Variant1Value arg1

        [ arg1, arg2 ] ->
            Ok <| IR.Variant2Value arg1 arg2

        [ arg1, arg2, arg3 ] ->
            Ok <| IR.Variant3Value arg1 arg2 arg3

        [ arg1, arg2, arg3, arg4 ] ->
            Ok <| IR.Variant4Value arg1 arg2 arg3 arg4

        [ arg1, arg2, arg3, arg4, arg5 ] ->
            Ok <| IR.Variant5Value arg1 arg2 arg3 arg4 arg5

        _ ->
            Err "Variant has too many args"


variantTypeToArgsList : VariantType -> List ( String, Type )
variantTypeToArgsList v =
    List.indexedMap (\idx item -> ( String.fromInt idx, item )) <|
        case v of
            Variant0Type ->
                []

            Variant1Type arg1 ->
                [ arg1 ]

            Variant2Type arg1 arg2 ->
                [ arg1, arg2 ]

            Variant3Type arg1 arg2 arg3 ->
                [ arg1, arg2, arg3 ]

            Variant4Type arg1 arg2 arg3 arg4 ->
                [ arg1, arg2, arg3, arg4 ]

            Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                [ arg1, arg2, arg3, arg4, arg5 ]


variantValueToArgsList : VariantValue -> List ( String, Value )
variantValueToArgsList v =
    List.indexedMap (\idx item -> ( String.fromInt idx, item )) <|
        case v of
            Variant0Value ->
                []

            Variant1Value arg1 ->
                [ arg1 ]

            Variant2Value arg1 arg2 ->
                [ arg1, arg2 ]

            Variant3Value arg1 arg2 arg3 ->
                [ arg1, arg2, arg3 ]

            Variant4Value arg1 arg2 arg3 arg4 ->
                [ arg1, arg2, arg3, arg4 ]

            Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                [ arg1, arg2, arg3, arg4, arg5 ]



-- PATH


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"



{-

   [] |> pathIsAncestorOf []
   --> True

   [] |> pathIsAncestorOf [ "" ]
   --> True

   [ "" ] |> pathIsAncestorOf []
   --> False

   [ "b", "a" ] |> pathIsAncestorOf [ "c", "b", "a" ]
   --> True

   [ "d", "a" ] |> pathIsAncestorOf [ "c", "b", "a" ]
   --> False

   [ "c", "b", "a" ] |> pathIsAncestorOf [ "c", "b", "a" ]
   --> True

   [ "c", "b", "a" ] |> pathIsAncestorOf [ "b", "a" ]
   --> False

-}


pathIsAncestorOf : Path -> Path -> Bool
pathIsAncestorOf descendant ancestor =
    pathIsAncestorOfHelp (List.reverse descendant) (List.reverse ancestor)


pathIsAncestorOfHelp : List a -> List a -> Bool
pathIsAncestorOfHelp descendant ancestor =
    case ( descendant, ancestor ) of
        ( [], [] ) ->
            True

        ( d :: restD, a :: restA ) ->
            if d == a then
                pathIsAncestorOfHelp restD restA

            else
                False

        ( [], _ :: _ ) ->
            -- descendant is shorter than ancestor, so it can't really be a descendant
            False

        ( _ :: _, [] ) ->
            True


matchPath : Path -> Path -> Match
matchPath revSought revGot =
    let
        sought =
            List.reverse revSought

        got =
            List.reverse revGot
    in
    if got == sought then
        FullMatch

    else
        let
            gotPrefix =
                List.take (List.length sought) got

            soughtPrefix =
                List.take (List.length got) sought
        in
        if gotPrefix == soughtPrefix then
            let
                next1 =
                    List.drop (List.length got) sought
                        |> List.head
                        |> Maybe.withDefault ""

                next2 =
                    List.drop (List.length got + 1) sought
                        |> List.head
                        |> Maybe.withDefault ""
            in
            PrefixMatch { next1 = next1, next2 = next2 }

        else
            NoMatch


type Match
    = FullMatch
    | PrefixMatch { next1 : String, next2 : String }
    | NoMatch
