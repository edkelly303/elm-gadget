module Gadget.Adapter.Form exposing
    ( Form, Model, Msg, fromGadget, fromGadgetWithConfig, FormConfig, default
    , Control, ControlConfig, control
    , label
    )

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

TODO


## Example

    TODO


## API

@docs Form, Model, Msg, fromGadget, fromGadgetWithConfig, FormConfig, default

@docs Control, ControlConfig, control

@docs label

-}

import Dict exposing (Dict)
import Gadget
import Gadget.IR as IR exposing (Type(..), Value(..), VariantType(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Form"


type alias Path =
    List Int


{-| TODO
-}
type Model
    = Model InnerModel


type alias InnerModel =
    { values : Dict Path Value
    , selectedCustomTypes : Dict Path Int
    }


{-| TODO
-}
type Msg
    = FieldUpdated ( Path, Value )
    | CustomTypeUpdated ( Path, Int )


{-| TODO
-}
type alias Form a msg =
    { init : Model
    , update : Msg -> Model -> Model
    , view : Model -> H.Html msg
    , submit : Model -> Result String a
    }


{-| TODO
-}
type alias FormConfig =
    { int : Control
    , string : Control
    }


{-| TODO
-}
type Control
    = Control
        { init : Value
        , update : Value -> Value -> Value
        , view : String -> Value -> H.Html Value
        , submit : Value -> Result String Value
        }


{-| TODO
-}
type alias ControlConfig msg model output =
    { msg : IR.Gadget msg
    , model : IR.Gadget model
    , output : IR.Gadget output
    , init : model
    , update : msg -> model -> model
    , view : String -> model -> H.Html msg
    , submit : model -> Result String output
    }


{-| TODO
-}
default : FormConfig
default =
    { int = int
    , string = string
    }


{-| TODO
-}
fromGadget : (Msg -> msg) -> IR.Gadget a -> Form a msg
fromGadget toMsg gadget =
    fromGadgetWithConfig default toMsg gadget


{-| TODO
-}
fromGadgetWithConfig : FormConfig -> (Msg -> msg) -> IR.Gadget a -> Form a msg
fromGadgetWithConfig config toMsg gadget =
    { init = init config gadget
    , update = update config gadget
    , view = view config gadget >> H.map toMsg
    , submit = submit config gadget
    }


{-| TODO
-}
label : String -> IR.Gadget a -> IR.Gadget a
label l gadget =
    tools.attach "label" Gadget.string l gadget


{-| TODO
-}
control : ControlConfig msg model output -> Control
control config =
    Control
        { init = IR.fromInput config.model config.init
        , update =
            \msg model ->
                Result.map2 config.update
                    (IR.toOutput config.msg msg)
                    (IR.toOutput config.model model)
                    |> Result.map (IR.fromInput config.model)
                    |> Result.withDefault model
        , view =
            \id model ->
                Result.map (config.view id) (IR.toOutput config.model model)
                    |> Result.Extra.extract H.text
                    |> H.map (\msg -> IR.fromInput config.msg msg)
        , submit =
            \model ->
                Result.andThen config.submit (IR.toOutput config.model model)
                    |> Result.map (IR.fromInput config.output)
        }


int : Control
int =
    control
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.int
        , init = ""
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "number"
                    , HE.onInput identity
                    , HA.id id
                    ]
                    [ H.text model ]
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "Not an integer"
        }


string : Control
string =
    control
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.string
        , init = ""
        , update = \msg _ -> msg
        , view =
            \id model ->
                H.input
                    [ HA.type_ "text"
                    , HE.onInput identity
                    , HA.id id
                    ]
                    [ H.text model ]
        , submit = Ok
        }


init : FormConfig -> IR.Gadget a -> Model
init config gadget =
    initHelp config { values = Dict.empty, selectedCustomTypes = Dict.empty } [ 0 ] (IR.irType gadget)
        |> Model


initHelp : FormConfig -> InnerModel -> Path -> Type -> InnerModel
initHelp config dict path irType =
    let
        insert value =
            { dict | values = Dict.insert path value dict.values }

        go getter =
            let
                (Control c) =
                    getter config
            in
            insert c.init
    in
    case irType of
        UnitType _ ->
            insert UnitValue

        BoolType _ ->
            insert (BoolValue False)

        CharType _ ->
            insert (StringValue "")

        StringType _ ->
            go .string

        IntType _ ->
            go .int

        FloatType _ ->
            insert (StringValue "")

        CustomType _ ( firstName, firstVariant ) restNamesAndVariants ->
            let
                variantTypes =
                    firstVariant :: List.map Tuple.second restNamesAndVariants
            in
            List.Extra.indexedFoldl
                (\variantIdx variantType variantTypesOutput ->
                    List.Extra.indexedFoldl
                        (\argIdx argType argTypesOutput ->
                            initHelp config
                                argTypesOutput
                                (argIdx :: variantIdx :: path)
                                argType
                        )
                        variantTypesOutput
                        (variantTypeToArgsList variantType)
                )
                dict
                variantTypes
                |> (\d -> { d | selectedCustomTypes = Dict.insert path 0 d.selectedCustomTypes })

        RecordType _ namedFieldTypes ->
            List.Extra.indexedFoldl
                (\idx ( _, fieldType ) output ->
                    initHelp config output (idx :: path) fieldType
                )
                dict
                namedFieldTypes

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"


update : FormConfig -> IR.Gadget a -> Msg -> Model -> Model
update config gadget msg (Model dict) =
    case msg of
        FieldUpdated ( path, msgValue ) ->
            Maybe.map2
                (\modelValue irType ->
                    { dict
                        | values =
                            Dict.insert path (updateHelp config irType msgValue modelValue) dict.values
                    }
                        |> Model
                )
                (Dict.get path dict.values)
                (typeFromPath path (IR.irType gadget))
                |> Maybe.withDefault (Model dict)

        CustomTypeUpdated ( path, idx ) ->
            Model { dict | selectedCustomTypes = Dict.insert path idx dict.selectedCustomTypes }


updateHelp : FormConfig -> Type -> Value -> Value -> Value
updateHelp config irType msgValue modelValue =
    let
        do getter =
            let
                (Control c) =
                    getter config
            in
            c.update msgValue modelValue
    in
    case irType of
        UnitType _ ->
            UnitValue

        BoolType _ ->
            msgValue

        CharType _ ->
            msgValue

        StringType _ ->
            do .string

        IntType _ ->
            do .int

        FloatType _ ->
            msgValue

        CustomType _ _ _ ->
            Debug.log "implement me!" modelValue

        RecordType _ _ ->
            Debug.todo "branch 'RecordType _ _' not implemented"

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"


view : FormConfig -> IR.Gadget a -> Model -> H.Html Msg
view config gadget (Model dict) =
    let
        viewSelectedCustomTypes =
            dict.selectedCustomTypes
                |> Dict.map
                    (\path idx ->
                        let
                            variants =
                                case typeFromPath path (IR.irType gadget) of
                                    Just (CustomType _ fst rest) ->
                                        fst :: rest

                                    _ ->
                                        []
                        in
                        H.fieldset []
                            (List.indexedMap
                                (\i ( name, v ) ->
                                    H.span
                                        []
                                        [ H.input
                                            [ HA.type_ "radio"
                                            , HA.name (pathToString path)
                                            , HA.checked (i == idx)
                                            , HE.onCheck (\_ -> CustomTypeUpdated ( path, i ))
                                            , HA.id name
                                            ]
                                            []
                                        , H.label [ HA.for name ] [ H.text name ]
                                        ]
                                )
                                variants
                            )
                    )
    in
    dict.values
        |> Dict.filter
            (\path _ ->
                List.foldr
                    (\digit ( acc, continue ) ->
                        case Dict.get acc dict.selectedCustomTypes of
                            Nothing ->
                                ( acc ++ [ digit ], continue )

                            Just selected ->
                                ( acc ++ [ digit ], selected == digit )
                    )
                    ( [], True )
                    path
                    |> Tuple.second
            )
        |> Dict.map
            (\path modelValue ->
                case typeFromPath path (IR.irType gadget) of
                    Just modelType ->
                        let
                            label_ =
                                tools.decode "label" Gadget.string (tools.extract modelType)
                                    |> Maybe.withDefault
                                        ("[Label missing at "
                                            ++ pathToString path
                                            ++ "]"
                                        )

                            do getter =
                                let
                                    (Control c) =
                                        getter config
                                in
                                c.view (pathToString path) modelValue
                        in
                        H.div []
                            [ H.label [ HA.for (pathToString path) ] [ H.text label_ ]
                            , case modelType of
                                IntType _ ->
                                    do .int

                                StringType _ ->
                                    do .string

                                other ->
                                    H.text ("Unhandled type: " ++ Debug.toString other)
                            ]
                            |> H.map (\msg -> FieldUpdated ( path, msg ))

                    Nothing ->
                        H.text "error: type not found"
            )
        |> Dict.union viewSelectedCustomTypes
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> H.form []


submit : FormConfig -> IR.Gadget a -> Model -> Result String a
submit config gadget (Model dict) =
    submitHelp config [ 0 ] (IR.irType gadget) dict.values
        |> Result.andThen (IR.toOutput gadget)


submitHelp : FormConfig -> Path -> Type -> Dict Path Value -> Result String Value
submitHelp config path irType dict =
    let
        output getter =
            let
                (Control c) =
                    getter config
            in
            Dict.get path dict
                |> Result.fromMaybe ("Value not found at path" ++ pathToString path)
                |> Result.andThen c.submit
    in
    case irType of
        UnitType _ ->
            Ok UnitValue

        StringType _ ->
            output .string

        IntType _ ->
            output .int

        BoolType _ ->
            Debug.todo "branch 'BoolType _' not implemented"

        CharType _ ->
            Debug.todo "branch 'CharType _' not implemented"

        FloatType _ ->
            Debug.todo "branch 'FloatType _' not implemented"

        CustomType _ _ _ ->
            Debug.log "branch 'CustomType _ _ _' not implemented" (Err "custom type")

        RecordType _ namedFieldTypes ->
            List.indexedMap
                (\idx ( name, fieldType ) ->
                    submitHelp config (idx :: path) fieldType dict
                        |> Result.map (Tuple.pair name)
                )
                namedFieldTypes
                |> Result.Extra.combine
                |> Result.map RecordValue

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"



-- helpers


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


pathToString : List Int -> String
pathToString path =
    List.reverse path
        |> List.map String.fromInt
        |> String.join "-"


typeFromPath : Path -> Type -> Maybe Type
typeFromPath soughtPath irType =
    typeFromPathHelp [ 0 ] soughtPath irType


typeFromPathHelp : Path -> Path -> Type -> Maybe Type
typeFromPathHelp thisPath soughtPath irType =
    if thisPath == soughtPath then
        Just irType

    else
        case irType of
            RecordType _ namedFieldTypes ->
                List.Extra.indexedFoldl
                    (\idx ( _, fieldType ) acc ->
                        case acc of
                            Just foundType ->
                                Just foundType

                            Nothing ->
                                typeFromPathHelp (idx :: thisPath) soughtPath fieldType
                    )
                    Nothing
                    namedFieldTypes

            CustomType selected ( firstName, firstVariant ) restNamesAndVariants ->
                let
                    variantTypes =
                        firstVariant :: List.map Tuple.second restNamesAndVariants
                in
                List.Extra.indexedFoldl
                    (\variantIdx variantType variantTypesOutput ->
                        List.Extra.indexedFoldl
                            (\argIdx argType argTypesOutput ->
                                case argTypesOutput of
                                    Just foundType ->
                                        Just foundType

                                    Nothing ->
                                        typeFromPathHelp
                                            (argIdx :: variantIdx :: thisPath)
                                            soughtPath
                                            argType
                            )
                            variantTypesOutput
                            (variantTypeToArgsList variantType)
                    )
                    Nothing
                    variantTypes

            ListType _ itemType ->
                if List.drop 1 soughtPath == thisPath then
                    Just itemType

                else
                    Nothing

            LazyType _ _ ->
                Debug.todo "branch 'LazyType _ _' not implemented"

            TupleType _ _ _ ->
                Debug.todo "branch 'TupleType _ _ _' not implemented"

            TripleType _ _ _ _ ->
                Debug.todo "branch 'TripleType _ _ _ _' not implemented"

            _ ->
                Nothing
