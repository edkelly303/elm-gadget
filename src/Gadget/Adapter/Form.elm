module Gadget.Adapter.Form exposing
    ( Form, Model, Msg, FormConfig, fromGadget, fromGadgetWithConfig, default
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
import Gadget.Adapter.Pretty as Pretty
import Gadget.IR as IR exposing (Type(..), Value(..))
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
    = Model (Dict Path Value)


{-| TODO
-}
type Msg
    = Msg ( Path, Value )


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
    initHelp config Dict.empty [ 0 ] (IR.irType gadget)
        |> Model


initHelp : FormConfig -> Dict Path Value -> Path -> Type -> Dict Path Value
initHelp config dict path irType =
    let
        insert value =
            Dict.insert path value dict

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

        CustomType _ _ _ ->
            Debug.todo "branch 'CustomType _ _ _' not implemented"

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
update config gadget (Msg ( path, msgValue )) (Model dict) =
    Maybe.map2
        (\modelValue irType ->
            Dict.insert path (updateHelp config irType msgValue modelValue) dict
                |> Model
        )
        (Dict.get path dict)
        (typeFromPath path (IR.irType gadget))
        |> Maybe.withDefault (Model dict)


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
            Debug.todo "branch 'CustomType _ _ _' not implemented"

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
    Dict.map
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

                            _ ->
                                H.text "error: mismatched modelType and modelValue"
                        ]
                        |> H.map (\msg -> Msg ( path, msg ))

                Nothing ->
                    H.text "error: type not found"
        )
        dict
        |> Dict.values
        |> H.form []


submit : FormConfig -> IR.Gadget a -> Model -> Result String a
submit config gadget (Model dict) =
    submitHelp config [ 0 ] (IR.irType gadget) dict
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
            Debug.todo "branch 'CustomType _ _ _' not implemented"

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

            CustomType _ _ _ ->
                Debug.todo "branch 'CustomType _ _ _' not implemented"

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
