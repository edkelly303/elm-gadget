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

import Dict
import Gadget
import Gadget.Adapter.Form2.Control as Control
import Gadget.Adapter.Form2.Internal as Internal exposing (..)
import Gadget.IR as IR exposing (Error, Type(..), Value(..), VariantType(..), VariantValue(..))
import Html as H
import Html.Attributes as HA
import List.Extra
import Result.Extra
import Set



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'


-}


type alias Form formMsg appMsg output =
    { init : ( Model, Cmd appMsg )
    , load : output -> Result (List Error) Model
    , update : Msg formMsg -> Model -> ( Model, Cmd appMsg )
    , notify : formMsg -> Model -> Model
    , view : Model -> H.Html appMsg
    , subscriptions : Model -> Sub appMsg
    , submit : Model -> Result (List Error) output
    }


type alias Msg formMsg =
    Internal.Msg formMsg


type Model
    = Sum Int (List (List Model))
    | Collection (List Model)
    | Record Int (List Model)
    | Pair Model Model
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
                |> load config path outputType

        notify_ formMsg model =
            update_ (FormMsg formMsg) model
                |> Tuple.first

        view_ model =
            view config path outputType model
                |> H.form []

        subscriptions_ model =
            subscriptions config path outputType model

        submit_ model =
            submit config model
    in
    { init = init_
    , load = load_
    , update = update_
    , notify = notify_
    , view = view_
    , subscriptions = subscriptions_
    , submit = submit_
    }


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



{-
   d888888b d8b   db d888888b d888888b
     `88'   888o  88   `88'   `~~88~~'
      88    88V8o 88    88       88
      88    88 V8o88    88       88
     .88.   88  V888   .88.      88
   Y888888P VP   V8P Y888888P    YP


-}


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

                PairType _ aType bType ->
                    let
                        ( aValue, aCmd ) =
                            init config ("0" :: path) aType

                        ( bValue, bCmd ) =
                            init config ("1" :: path) bType
                    in
                    ( Pair aValue bValue
                    , Cmd.batch [ aCmd, bCmd ]
                    )

                TripleType _ aType bType cType ->
                    let
                        ( aValue, aCmd ) =
                            init config ("0" :: path) aType

                        ( bValue, bCmd ) =
                            init config ("1" :: path) bType

                        ( cValue, cCmd ) =
                            init config ("2" :: path) cType
                    in
                    ( Triple aValue bValue cValue
                    , Cmd.batch [ aCmd, bCmd, cCmd ]
                    )

                ListType _ _ ->
                    ( Collection [], Cmd.none )

                RecordType _ namedFieldTypes ->
                    let
                        ( fieldModels, cmds ) =
                            List.foldr
                                (\( thisFieldName, thisFieldType ) ( prevFieldModels, prevCmds ) ->
                                    let
                                        ( thisFieldModel, cmd ) =
                                            init config (thisFieldName :: path) thisFieldType
                                    in
                                    ( thisFieldModel :: prevFieldModels
                                    , cmd :: prevCmds
                                    )
                                )
                                ( [], [] )
                                namedFieldTypes
                    in
                    ( Record 0 fieldModels
                    , Cmd.batch cmds
                    )

                CustomType _ firstNamedVariantType restNamedVariantTypes ->
                    let
                        namedVariantTypes =
                            firstNamedVariantType :: restNamedVariantTypes

                        ( variantModels, cmds ) =
                            List.foldr
                                (\( thisVariantName, thisVariantType ) ( prevVariantModels, prevVariantCmds ) ->
                                    let
                                        thisVariantArgTypes =
                                            variantTypeToArgsList thisVariantType

                                        ( thisVariantModel, thisVariantCmds ) =
                                            List.Extra.indexedFoldl
                                                (\idx argType ( prevArgModels, prevArgCmds ) ->
                                                    let
                                                        ( argModel, argCmd ) =
                                                            init config (String.fromInt idx :: thisVariantName :: path) argType
                                                    in
                                                    ( argModel :: prevArgModels
                                                    , argCmd :: prevArgCmds
                                                    )
                                                )
                                                ( [], [] )
                                                thisVariantArgTypes
                                    in
                                    ( List.reverse thisVariantModel :: prevVariantModels
                                    , thisVariantCmds ++ prevVariantCmds
                                    )
                                )
                                ( [], [] )
                                namedVariantTypes
                    in
                    ( Sum 0 variantModels
                    , Cmd.batch cmds
                    )

                LazyType _ thunk ->
                    init config path (thunk ())



{-
   db       .d88b.   .d8b.  d8888b.
   88      .8P  Y8. d8' `8b 88  `8D
   88      88    88 88ooo88 88   88
   88      88    88 88~~~88 88   88
   88booo. `8b  d8' 88   88 88  .8D
   Y88888P  `Y88P'  YP   YP Y8888D'


-}


load : Config formMsg appMsg output -> Path -> Type -> Value -> Result (List Error) Model
load config path type_ value =
    let
        loadPrimitive primitive control =
            control.load value
                |> Result.map (Primitive primitive)
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
            case ( value, type_ ) of
                ( UnitValue, UnitType _ ) ->
                    Ok Unit

                ( IntValue _, IntType _ ) ->
                    loadPrimitive IntPrimitive config.int

                ( FloatValue _, FloatType _ ) ->
                    loadPrimitive FloatPrimitive config.float

                ( StringValue _, StringType _ ) ->
                    loadPrimitive StringPrimitive config.string

                ( CharValue _, CharType _ ) ->
                    loadPrimitive CharPrimitive config.char

                ( BoolValue _, BoolType _ ) ->
                    loadPrimitive BoolPrimitive config.bool

                ( PairValue aValue bValue, PairType _ aType bType ) ->
                    Result.map2 Pair
                        (load config ("0" :: path) aType aValue)
                        (load config ("1" :: path) bType bValue)

                ( TripleValue aValue bValue cValue, TripleType _ aType bType cType ) ->
                    Result.map3 Triple
                        (load config ("0" :: path) aType aValue)
                        (load config ("1" :: path) bType bValue)
                        (load config ("2" :: path) cType cValue)

                ( ListValue itemValues, ListType _ itemType ) ->
                    List.indexedMap
                        (\idx itemValue ->
                            load config (String.fromInt idx :: path) itemType itemValue
                        )
                        itemValues
                        |> Result.Extra.combine
                        |> Result.map Collection

                ( RecordValue fieldValues, RecordType _ fieldTypes ) ->
                    let
                        fieldModels =
                            List.Extra.indexedFoldl
                                (\idx ( ( _, thisFieldValue ), ( _, thisFieldType ) ) prevFieldModels ->
                                    load config (String.fromInt idx :: path) thisFieldType thisFieldValue :: prevFieldModels
                                )
                                []
                                (List.Extra.zip fieldValues fieldTypes)
                    in
                    fieldModels
                        |> List.reverse
                        |> Result.Extra.combine
                        |> Result.map (Record 0)

                ( CustomValue selected ( _, thisVariantValue ), CustomType _ firstNamedVariantType restNamedVariantTypes ) ->
                    let
                        namedVariantTypes =
                            firstNamedVariantType :: restNamedVariantTypes

                        variantModels =
                            List.Extra.indexedFoldl
                                (\variantIdx ( thisVariantName, thisVariantType ) prevVariantModels ->
                                    let
                                        thisVariantArgTypesAndValues =
                                            List.Extra.zip
                                                (variantTypeToArgsList thisVariantType)
                                                (variantValueToArgsList thisVariantValue)

                                        thisVariantModel =
                                            List.Extra.indexedFoldl
                                                (\argIdx ( argType, argValue ) prevArgModels ->
                                                    let
                                                        argPath =
                                                            String.fromInt argIdx :: thisVariantName :: path

                                                        argModel =
                                                            if variantIdx == selected then
                                                                load config argPath argType argValue

                                                            else
                                                                init config argPath argType
                                                                    |> Tuple.first
                                                                    |> Ok
                                                    in
                                                    argModel :: prevArgModels
                                                )
                                                []
                                                thisVariantArgTypesAndValues
                                    in
                                    (List.reverse thisVariantModel |> Result.Extra.combine) :: prevVariantModels
                                )
                                []
                                namedVariantTypes
                    in
                    List.reverse variantModels
                        |> Result.Extra.combine
                        |> Result.map (Sum selected)

                _ ->
                    Err []



{-
   db    db d8888b. d8888b.  .d8b.  d888888b d88888b
   88    88 88  `8D 88  `8D d8' `8b `~~88~~' 88'
   88    88 88oodD' 88   88 88ooo88    88    88ooooo
   88    88 88~~~   88   88 88~~~88    88    88~~~~~
   88b  d88 88      88  .8D 88   88    88    88.
   ~Y8888P' 88      Y8888D' YP   YP    YP    Y88888P


-}


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

        ( Pair aModel bModel, PairType _ aType bType ) ->
            let
                ( newAModel, aCmd ) =
                    update config ("0" :: modelPath) aType msg aModel

                ( newBModel, bCmd ) =
                    update config ("1" :: modelPath) bType msg bModel
            in
            ( Pair newAModel newBModel
            , Cmd.batch [ aCmd, bCmd ]
            )

        ( Triple aModel bModel cModel, TripleType _ aType bType cType ) ->
            let
                ( newAModel, aCmd ) =
                    update config ("0" :: modelPath) aType msg aModel

                ( newBModel, bCmd ) =
                    update config ("1" :: modelPath) bType msg bModel

                ( newCModel, cCmd ) =
                    update config ("2" :: modelPath) cType msg cModel
            in
            ( Triple newAModel newBModel newCModel
            , Cmd.batch [ aCmd, bCmd, cCmd ]
            )

        ( Collection itemModels, ListType _ itemType ) ->
            let
                ( newItemModels, cmds ) =
                    List.indexedMap
                        (\idx itemModel ->
                            update config (String.fromInt idx :: modelPath) itemType msg itemModel
                        )
                        itemModels
                        |> List.unzip
            in
            ( Collection newItemModels, Cmd.batch cmds )

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

        ( Sum selected variantModels, CustomType _ firstNamedVariantType restNamedVariantTypes ) ->
            let
                continue () =
                    let
                        variantModelsAndNamedTypes =
                            List.Extra.zip
                                variantModels
                                (firstNamedVariantType :: restNamedVariantTypes)

                        ( newVariantModels, variantCmds ) =
                            List.foldl
                                (\( thisVariantModel, ( thisVariantName, thisVariantType ) ) ( prevVariantModels, prevVariantCmds ) ->
                                    let
                                        thisVariantArgTypesAndValues =
                                            List.Extra.zip
                                                (variantTypeToArgsList thisVariantType)
                                                thisVariantModel

                                        ( newThisVariantModel, thisVariantCmds ) =
                                            List.Extra.indexedFoldl
                                                (\argIdx ( argType, argModel ) ( prevArgModels, prevArgCmds ) ->
                                                    let
                                                        argPath =
                                                            String.fromInt argIdx :: thisVariantName :: modelPath

                                                        ( newArgModel, newArgCmd ) =
                                                            update config argPath argType msg argModel
                                                    in
                                                    ( newArgModel :: prevArgModels
                                                    , newArgCmd :: prevArgCmds
                                                    )
                                                )
                                                ( [], [] )
                                                thisVariantArgTypesAndValues
                                    in
                                    ( List.reverse newThisVariantModel :: prevVariantModels
                                    , thisVariantCmds ++ prevVariantCmds
                                    )
                                )
                                ( [], [] )
                                variantModelsAndNamedTypes
                    in
                    ( Sum selected (List.reverse newVariantModels)
                    , Cmd.batch variantCmds
                    )
            in
            case msg of
                ControlMsg msgPath msgValue ->
                    if msgPath == modelPath then
                        case IR.toOutput Gadget.int msgValue of
                            Ok newSelected ->
                                ( Sum newSelected variantModels, Cmd.none )

                            Err _ ->
                                ( Sum selected variantModels, Cmd.none )

                    else
                        continue ()

                FormMsg _ ->
                    continue ()

        _ ->
            ( Unit, Cmd.none )


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



{-
   db    db d888888b d88888b db   d8b   db
   88    88   `88'   88'     88   I8I   88
   Y8    8P    88    88ooooo 88   I8I   88
   `8b  d8'    88    88~~~~~ Y8   I8I   88
    `8bd8'    .88.   88.     `8b d8'8b d8'
      YP    Y888888P Y88888P  `8b8' `8d8'


-}


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

        maybeLabel =
            tools.extract type_
                |> tools.decode "label" Gadget.string

        optionalLabel =
            case maybeLabel of
                Nothing ->
                    H.text ""

                Just l ->
                    H.label [ HA.for id ] [ H.text l ]

        requiredLabel =
            H.label [ HA.for id ] [ H.text (Maybe.withDefault id maybeLabel) ]
    in
    case ( model, type_ ) of
        ( Unit, _ ) ->
            []

        ( Primitive primitive primitiveModel, _ ) ->
            case lookupControl primitive config of
                Just control ->
                    config.viewControl False
                        [ requiredLabel
                        , control.view id primitiveModel
                            |> H.map (msgMapper config path)
                        , H.div []
                            (case control.submit path primitiveModel of
                                Err errors ->
                                    List.map (\{ error } -> config.viewFeedback error) errors

                                Ok _ ->
                                    []
                            )
                        ]

                Nothing ->
                    [ H.text "view error: control not found" ]

        ( Record _ fieldModels, RecordType _ fieldTypes ) ->
            let
                fields =
                    List.Extra.zip fieldTypes fieldModels
                        |> List.concatMap
                            (\( ( fieldName, fieldType ), fieldModel ) ->
                                view config (fieldName :: path) fieldType fieldModel
                            )
            in
            optionalLabel :: fields

        ( Collection itemModels, ListType _ itemType ) ->
            requiredLabel
                :: List.concat
                    (List.indexedMap
                        (\idx itemModel ->
                            view config (String.fromInt idx :: path) itemType itemModel
                        )
                        itemModels
                    )

        ( Pair aModel bModel, PairType _ aType bType ) ->
            optionalLabel
                :: List.concat
                    [ view config ("0" :: path) aType aModel
                    , view config ("1" :: path) bType bModel
                    ]

        ( Triple aModel bModel cModel, TripleType _ aType bType cType ) ->
            optionalLabel
                :: List.concat
                    [ view config ("0" :: path) aType aModel
                    , view config ("1" :: path) bType bModel
                    , view config ("2" :: path) cType cModel
                    ]

        ( Sum selected variantModels, CustomType _ firstNamedVariantType restNamedVariantTypes ) ->
            let
                namedVariantTypesAndModels =
                    List.Extra.zip
                        (firstNamedVariantType :: restNamedVariantTypes)
                        variantModels
            in
            case List.Extra.getAt selected namedVariantTypesAndModels of
                Just ( ( variantName, variantType ), argModels ) ->
                    let
                        argTypes =
                            variantTypeToArgsList variantType

                        argTypesAndModels =
                            List.Extra.zip argTypes argModels

                        args =
                            List.indexedMap
                                (\idx ( argType, argModel ) ->
                                    view config (String.fromInt idx :: variantName :: path) argType argModel
                                )
                                argTypesAndModels
                                |> List.concat
                    in
                    optionalLabel :: args

                Nothing ->
                    [ H.text "uh oh! invalid variant selection in view function" ]

        _ ->
            [ H.text "uh oh! type mismatch in view function" ]



{-
   .d8888. db    db d8888b. .d8888.
   88'  YP 88    88 88  `8D 88'  YP
   `8bo.   88    88 88oooY' `8bo.
     `Y8b. 88    88 88~~~b.   `Y8b.
   db   8D 88b  d88 88   8D db   8D
   `8888Y' ~Y8888P' Y8888P' `8888Y'


-}


subscriptions : Config formMsg appMsg output -> Path -> Type -> Model -> Sub appMsg
subscriptions config path type_ model =
    case ( model, type_ ) of
        ( Unit, _ ) ->
            Sub.none

        ( Primitive primitive modelValue, _ ) ->
            case lookupControl primitive config of
                Just control ->
                    control.subscriptions modelValue
                        |> Sub.map (msgMapper config path)

                Nothing ->
                    Sub.none

        ( Pair a b, PairType _ aType bType ) ->
            Sub.batch
                [ subscriptions config ("0" :: path) aType a
                , subscriptions config ("1" :: path) bType b
                ]

        ( Triple a b c, TripleType _ aType bType cType ) ->
            Sub.batch
                [ subscriptions config ("0" :: path) aType a
                , subscriptions config ("1" :: path) bType b
                , subscriptions config ("2" :: path) cType c
                ]

        ( Record _ fieldModels, RecordType _ fieldTypes ) ->
            List.Extra.zip fieldModels fieldTypes
                |> List.map
                    (\( fieldModel, ( fieldName, fieldType ) ) ->
                        subscriptions config (fieldName :: path) fieldType fieldModel
                    )
                |> Sub.batch

        ( Collection itemModels, ListType _ itemType ) ->
            itemModels
                |> List.indexedMap (\idx itemModel -> subscriptions config (String.fromInt idx :: path) itemType itemModel)
                |> Sub.batch

        ( Sum _ variantModels, CustomType _ firstNamedVariantType restNamedVariantTypes ) ->
            List.Extra.zip variantModels (firstNamedVariantType :: restNamedVariantTypes)
                |> List.concatMap
                    (\( argModels, ( variantName, variantType ) ) ->
                        argModels
                            |> List.Extra.zip (variantTypeToArgsList variantType)
                            |> List.indexedMap
                                (\idx ( argType, argModel ) ->
                                    subscriptions config (String.fromInt idx :: variantName :: path) argType argModel
                                )
                    )
                |> Sub.batch

        _ ->
            Sub.none



{-
   .d8888. db    db d8888b. .88b  d88. d888888b d888888b
   88'  YP 88    88 88  `8D 88'YbdP`88   `88'   `~~88~~'
   `8bo.   88    88 88oooY' 88  88  88    88       88
     `Y8b. 88    88 88~~~b. 88  88  88    88       88
   db   8D 88b  d88 88   8D 88  88  88   .88.      88
   `8888Y' ~Y8888P' Y8888P' YP  YP  YP Y888888P    YP


-}


submit : Config formMsg msg output -> Model -> Result (List Error) output
submit config model =
    let
        ( parsedValue, parsingErrors ) =
            submitHelp config [] (IR.irType config.outputGadget) model
    in
    case ( parsingErrors, IR.toOutput config.outputGadget parsedValue ) of
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


submitHelp : Config formMsg msg output -> Path -> Type -> Model -> ( Value, List Error )
submitHelp config path type_ model =
    case ( model, type_ ) of
        ( Unit, _ ) ->
            ( UnitValue, [] )

        ( Primitive primitive modelValue, _ ) ->
            case lookupControl primitive config of
                Just control ->
                    case control.submit path modelValue of
                        Ok value ->
                            ( value, [] )

                        Err errs ->
                            ( control.dummyOutputValue, errs )

                Nothing ->
                    ( UnitValue
                    , [ { path = path, error = "uh oh, control not found in submit" } ]
                    )

        ( Pair a b, PairType _ aType bType ) ->
            let
                ( aValue, aErrs ) =
                    submitHelp config ("0" :: path) aType a

                ( bValue, bErrs ) =
                    submitHelp config ("1" :: path) bType b
            in
            ( PairValue aValue bValue, aErrs ++ bErrs )

        ( Triple a b c, TripleType _ aType bType cType ) ->
            let
                ( aValue, aErrs ) =
                    submitHelp config ("0" :: path) aType a

                ( bValue, bErrs ) =
                    submitHelp config ("1" :: path) bType b

                ( cValue, cErrs ) =
                    submitHelp config ("2" :: path) cType c
            in
            ( TripleValue aValue bValue cValue, aErrs ++ bErrs ++ cErrs )

        ( Collection itemModels, ListType _ itemType ) ->
            let
                ( itemValues, errors ) =
                    List.indexedMap
                        (\idx itemModel ->
                            let
                                ( value, errs ) =
                                    submitHelp config (String.fromInt idx :: path) itemType itemModel
                            in
                            ( value, errs )
                        )
                        itemModels
                        |> List.unzip
            in
            ( ListValue itemValues, List.concat errors )

        ( Record _ fieldModels, RecordType _ fieldTypes ) ->
            let
                ( namedValues, errors ) =
                    List.Extra.zip fieldModels fieldTypes
                        |> List.map
                            (\( fieldModel, ( fieldName, fieldType ) ) ->
                                let
                                    ( value, errs ) =
                                        submitHelp config (fieldName :: path) fieldType fieldModel
                                in
                                ( ( fieldName, value ), errs )
                            )
                        |> List.unzip
            in
            ( RecordValue namedValues, List.concat errors )

        ( Sum selected variantModels, CustomType _ firstNamedVariantType restNamedVariantTypes ) ->
            let
                maybe =
                    List.Extra.zip variantModels (firstNamedVariantType :: restNamedVariantTypes)
                        |> List.Extra.getAt selected
                        |> Maybe.map
                            (\( thisVariantModel, ( thisVariantName, thisVariantType ) ) ->
                                let
                                    argModelsAndTypes =
                                        List.Extra.zip
                                            thisVariantModel
                                            (variantTypeToArgsList thisVariantType)
                                in
                                List.indexedMap
                                    (\argIdx ( thisArgModel, thisArgType ) ->
                                        submitHelp config (String.fromInt argIdx :: thisVariantName :: path) thisArgType thisArgModel
                                    )
                                    argModelsAndTypes
                                    |> List.unzip
                                    |> Tuple.mapFirst
                                        (argsListToVariantValue
                                            -- pretty sure this can't fail
                                            >> Result.withDefault Variant0Value
                                        )
                                    |> Tuple.mapSecond List.concat
                                    |> (\( value, errs ) -> ( thisVariantName, value, errs ))
                            )
            in
            case maybe of
                Just ( variantName, variantValue, errors ) ->
                    ( CustomValue selected ( variantName, variantValue ), errors )

                Nothing ->
                    ( UnitValue, [ { path = path, error = "uh oh, variant not found at selected index in submit" } ] )

        _ ->
            ( UnitValue, [ { path = path, error = "uh oh, type/value mismatch in submit" } ] )


msgMapper : Config formMsg appMsg output -> Path -> Send Value formMsg appMsg -> appMsg
msgMapper config path msg =
    case msg of
        ToSelf toSelf ->
            config.msgToAppMsg (ControlMsg path toSelf)

        ToForm toAll ->
            config.msgToAppMsg (FormMsg toAll)

        ToApp toApp ->
            toApp



{-
   d8888b.  .d8b.  d888888b db   db
   88  `8D d8' `8b `~~88~~' 88   88
   88oodD' 88ooo88    88    88ooo88
   88~~~   88~~~88    88    88~~~88
   88      88   88    88    88   88
   88      YP   YP    YP    YP   YP


-}


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"


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



{-
   db    db  .d8b.  d8888b. d888888b  .d8b.  d8b   db d888888b
   88    88 d8' `8b 88  `8D   `88'   d8' `8b 888o  88 `~~88~~'
   Y8    8P 88ooo88 88oobY'    88    88ooo88 88V8o 88    88
   `8b  d8' 88~~~88 88`8b      88    88~~~88 88 V8o88    88
    `8bd8'  88   88 88 `88.   .88.   88   88 88  V888    88
      YP    YP   YP 88   YD Y888888P YP   YP VP   V8P    YP



-}


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


variantTypeToArgsList : VariantType -> List Type
variantTypeToArgsList v =
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


variantValueToArgsList : VariantValue -> List Value
variantValueToArgsList v =
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
