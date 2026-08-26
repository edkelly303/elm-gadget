module Gadget.Adapter.Form exposing
    ( Form, Model, Msg, fromGadget, fromGadgetWithConfig, FormConfig, default
    , Control, ControlConfig, control
    , label
    , customLabels
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

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Gadget
import Gadget.IR as IR exposing (Type(..), Value(..), VariantType(..), VariantValue(..))
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
    = Control InnerControl


type alias InnerControl =
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


type Model
    = Combinator CombinatorType IR.Metadata (Array.Array Model)
    | Primitive PrimitiveType IR.Metadata Value


type Msg
    = Msg Path Value


type CombinatorType
    = Collection
    | Variant
    | Product
    | Sum Int


type PrimitiveType
    = PString
    | PChar
    | PInt
    | PFloat
    | PBool
    | PUnit


init : FormConfig -> IR.Gadget a -> Model
init config gadget =
    initHelp config (IR.irType gadget)


run : (InnerControl -> method) -> FormConfig -> (FormConfig -> Control) -> method
run getMethod config getType =
    let
        (Control c) =
            getType config
    in
    getMethod c


initHelp : FormConfig -> Type -> Model
initHelp config irType =
    let
        initFor =
            run .init config
    in
    case irType of
        UnitType m ->
            Primitive PUnit m UnitValue

        BoolType m ->
            Primitive PBool m (BoolValue False)

        CharType m ->
            Primitive PChar m (StringValue "")

        StringType m ->
            Primitive PString m (initFor .string)

        IntType m ->
            Primitive PInt m (initFor .int)

        FloatType m ->
            Primitive PFloat m (StringValue "")

        CustomType m firstNameAndVariantType restNamesAndVariantTypes ->
            let
                variantTypes =
                    firstNameAndVariantType :: restNamesAndVariantTypes

                variants =
                    variantTypes
                        |> List.map
                            (\( n, v ) ->
                                v
                                    |> variantTypeToArgsArray
                                    |> Array.map (initHelp config)
                                    |> Combinator Variant IR.emptyMetadata
                            )
                        |> Array.fromList
            in
            Combinator (Sum 0) m variants

        RecordType m namedFieldTypes ->
            let
                fields =
                    namedFieldTypes
                        |> List.map (\( _, fieldType ) -> initHelp config fieldType)
                        |> Array.fromList
            in
            Combinator Product m fields

        ListType m _ ->
            Combinator Collection m Array.empty

        LazyType m innerType ->
            initHelp config (innerType ())

        TupleType m a b ->
            Combinator Product m (Array.fromList (List.map (initHelp config) [ a, b ]))

        TripleType m a b c ->
            Combinator Product m (Array.fromList (List.map (initHelp config) [ a, b, c ]))


update : FormConfig -> Msg -> Model -> Model
update config msg model =
    updateHelp config [ 0 ] (msg |> Debug.log "msg") model


updateHelp : FormConfig -> Path -> Msg -> Model -> Model
updateHelp config modelPath ((Msg msgPath msgValue) as msg) model =
    let
        updateFor =
            run .update config
    in
    case model of
        Primitive primitiveType metadata modelValue ->
            Primitive primitiveType metadata <|
                if modelPath == msgPath then
                    case primitiveType of
                        PUnit ->
                            modelValue

                        PString ->
                            updateFor .string msgValue modelValue
                                |> Debug.log "string updated"

                        PChar ->
                            Debug.todo "branch 'PChar' not implemented"

                        PInt ->
                            updateFor .int msgValue modelValue
                                |> Debug.log "int updated"

                        PFloat ->
                            Debug.todo "branch 'PFloat' not implemented"

                        PBool ->
                            Debug.todo "branch 'PBool' not implemented"

                else
                    modelValue |> Debug.log "no match"

        Combinator combinatorType metadata childModels ->
            case matchPath msgPath modelPath |> Debug.log "match" of
                FullMatch ->
                    case combinatorType of
                        Sum selected ->
                            case msgValue of
                                IntValue i ->
                                    Combinator (Sum i) metadata childModels

                                _ ->
                                    model

                        _ ->
                            model

                PrefixMatch { next } ->
                    Array.Extra.update next (updateHelp config (next :: modelPath) msg) childModels
                        |> Combinator combinatorType metadata

                NoMatch ->
                    model


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
                next =
                    List.drop (List.length got) sought
                        |> List.head
                        |> Maybe.withDefault 0
            in
            PrefixMatch { next = next }

        else
            NoMatch


type Match
    = FullMatch
    | PrefixMatch { next : Int }
    | NoMatch


view : FormConfig -> Model -> H.Html Msg
view config model =
    viewHelp config [ 0 ] model


viewHelp : FormConfig -> Path -> Model -> H.Html Msg
viewHelp config modelPath model =
    let
        id =
            pathToString modelPath

        viewFor typ =
            run .view config typ id

        label_ defaultLabel metadata =
            tools.decode "label" Gadget.string metadata
                |> Maybe.withDefault defaultLabel
    in
    case model of
        Primitive primitiveType metadata modelValue ->
            H.map (\msg -> Msg modelPath msg) <|
                H.div []
                    [ H.label [ HA.for id ] [ H.text (label_ id metadata) ]
                    , case primitiveType of
                        PUnit ->
                            Debug.todo "unit"

                        PString ->
                            viewFor .string modelValue

                        PChar ->
                            Debug.todo "branch 'PChar' not implemented"

                        PInt ->
                            viewFor .int modelValue

                        PFloat ->
                            Debug.todo "branch 'PFloat' not implemented"

                        PBool ->
                            Debug.todo "branch 'PBool' not implemented"
                    ]

        Combinator combinatorType metadata childModels ->
            case combinatorType of
                Sum selected ->
                    let
                        childView =
                            Array.get selected childModels
                                |> Maybe.map (viewHelp config (selected :: modelPath))
                                |> Maybe.withDefault (H.text "sum error")

                        ( customLabel_, variantLabels ) =
                            tools.decode "customLabel" (Gadget.tuple Gadget.string (Gadget.list Gadget.string)) metadata
                                |> Maybe.withDefault ( pathToString modelPath, [] )
                    in
                    H.div []
                        [ H.fieldset []
                            (H.legend [] [ H.text customLabel_ ]
                                :: List.indexedMap
                                    (\idx _ ->
                                        let
                                            childId =
                                                pathToString (idx :: modelPath)
                                        in
                                        H.span []
                                            [ H.input
                                                [ HA.id childId
                                                , HA.type_ "radio"
                                                , HE.onCheck (\_ -> Msg modelPath (IntValue idx))
                                                , HA.checked (selected == idx)
                                                ]
                                                []
                                            , H.label [ HA.for childId ]
                                                [ H.text
                                                    (List.Extra.getAt idx variantLabels
                                                        |> Maybe.withDefault (label_ childId metadata)
                                                    )
                                                ]
                                            ]
                                    )
                                    (Array.toList childModels)
                            )
                        , childView
                        ]

                _ ->
                    Array.indexedMap (\idx childModel -> viewHelp config (idx :: modelPath) childModel) childModels
                        |> Array.toList
                        |> H.div []


submit : FormConfig -> IR.Gadget a -> Model -> Result String a
submit config gadget model =
    submitHelp config [ 0 ] model
        |> Result.andThen (IR.toOutput gadget)


submitHelp : FormConfig -> Path -> Model -> Result String Value
submitHelp config path model =
    let
        submit_ getter =
            let
                (Control c) =
                    getter config
            in
            c.submit
    in
    case model of
        Primitive primitiveType metadata value ->
            case primitiveType of
                PUnit ->
                    Ok UnitValue

                PString ->
                    submit_ .string value

                PChar ->
                    Debug.todo "branch 'PChar' not implemented"

                PInt ->
                    submit_ .int value

                PFloat ->
                    Debug.todo "branch 'PFloat' not implemented"

                PBool ->
                    Debug.todo "branch 'PBool' not implemented"

        Combinator combinatorType metadata children ->
            case combinatorType of
                Sum selected ->
                    Array.get selected children
                        |> Result.fromMaybe ("Invalid Sum variant selection at " ++ pathToString path)
                        |> Result.andThen
                            (\v ->
                                case v of
                                    Combinator Variant _ args ->
                                        Array.toList args
                                            |> List.indexedMap (\idx arg -> submitHelp config (idx :: path) arg)
                                            |> Result.Extra.combine
                                            |> Result.andThen argsListToVariantValue

                                    _ ->
                                        Err "The child of a Sum should always be a Variant"
                            )
                        |> Result.map (\v -> CustomValue selected ( "", v ))

                Collection ->
                    Array.toList children
                        |> List.indexedMap (\idx child -> submitHelp config (idx :: path) child)
                        |> Result.Extra.combine
                        |> Result.map IR.ListValue

                Variant ->
                    Err "This branch should be unreachable, as Variants should always be handled by the Sum case"

                Product ->
                    Array.toList children
                        |> List.indexedMap (\idx child -> submitHelp config (idx :: path) child)
                        |> Result.Extra.combine
                        |> Result.map (List.map (Tuple.pair ""))
                        |> Result.map IR.RecordValue


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
    , update = update config
    , view = view config >> H.map toMsg
    , submit = submit config gadget
    }


{-| TODO
-}
label : String -> IR.Gadget a -> IR.Gadget a
label l gadget =
    tools.attach "label" Gadget.string l gadget


customLabels : String -> List String -> IR.Gadget d -> IR.Gadget d
customLabels l ls gadget =
    tools.attach "customLabel"
        (Gadget.tuple Gadget.string (Gadget.list Gadget.string))
        ( l, ls )
        gadget


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


variantTypeToArgsArray : VariantType -> Array Type
variantTypeToArgsArray v =
    Array.fromList <|
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
