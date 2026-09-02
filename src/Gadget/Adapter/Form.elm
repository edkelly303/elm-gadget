module Gadget.Adapter.Form exposing
    ( Form, Model, Msg, fromGadget, fromGadgetWithConfig, FormConfig, default
    , Control, ControlConfig, control
    , label, customLabels
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

@docs label, customLabels

-}

import Array exposing (Array)
import Array.Extra
import Gadget
import Gadget.IR as IR exposing (Error, Path, Type(..), Value(..), VariantType(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra


tools : IR.MetadataTools meta a
tools =
    IR.makeMetadataTools "Gadget.Adapter.Form"


{-| TODO
-}
type alias Form a msg =
    { init : Model
    , update : Msg -> Model -> Model
    , view : Model -> H.Html msg
    , submit : Model -> Result (List Error) a
    }


{-| TODO
-}
type Control
    = Control InnerControl


type alias InnerControl =
    { init : Value
    , update : Value -> Value -> Value
    , view : String -> Value -> H.Html Value
    , submit : Path -> Value -> Result (List Error) Value
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
type alias FormConfig =
    { bool : Control
    , int : Control
    , float : Control
    , char : Control
    , string : Control
    , feedback : String -> H.Html Msg
    }


{-| TODO
-}
default : FormConfig
default =
    { bool = bool
    , int = int
    , float = float
    , char = char
    , string = string
    , feedback = \error -> H.strong [ HA.style "color" "red" ] [ H.text error ]
    }


{-| TODO
-}
type Model
    = Combinator CombinatorType IR.Metadata (Array.Array Model)
    | Primitive PrimitiveType IR.Metadata Value


{-| TODO
-}
type Msg
    = Msg Path Value


type CombinatorType
    = Collection Type
    | Variant
    | Product ProductType
    | Sum Int


type ProductType
    = Record
    | Tuple
    | Triple


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
            Primitive PBool m (initFor .bool)

        CharType m ->
            Primitive PChar m (initFor .char)

        StringType m ->
            Primitive PString m (initFor .string)

        IntType m ->
            Primitive PInt m (initFor .int)

        FloatType m ->
            Primitive PFloat m (initFor .float)

        CustomType m firstNameAndVariantType restNamesAndVariantTypes ->
            let
                variantTypes =
                    firstNameAndVariantType :: restNamesAndVariantTypes

                variants =
                    variantTypes
                        |> List.map
                            (\( _, v ) ->
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
            Combinator (Product Record) m fields

        ListType m innerType ->
            Combinator (Collection innerType) m Array.empty

        LazyType _ innerType ->
            initHelp config (innerType ())

        TupleType m a b ->
            Combinator (Product Tuple) m (Array.fromList (List.map (initHelp config) [ a, b ]))

        TripleType m a b c ->
            Combinator (Product Triple) m (Array.fromList (List.map (initHelp config) [ a, b, c ]))


update : FormConfig -> Msg -> Model -> Model
update config msg model =
    updateHelp config [] (msg |> Debug.log "msg") model


updateHelp : FormConfig -> Path -> Msg -> Model -> Model
updateHelp config modelPath ((Msg msgPath msgValue) as msg) model =
    case model of
        Primitive primitiveType metadata modelValue ->
            Primitive primitiveType metadata <|
                if modelPath == msgPath then
                    let
                        updateFor typ_ =
                            run .update config typ_ msgValue modelValue
                    in
                    case primitiveType of
                        PUnit ->
                            modelValue

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

                else
                    modelValue

        Combinator combinatorType metadata childModels ->
            case matchPath msgPath modelPath |> Debug.log "match" of
                FullMatch ->
                    case combinatorType of
                        Sum _ ->
                            case msgValue of
                                IntValue i ->
                                    Combinator (Sum i) metadata childModels

                                _ ->
                                    model

                        Collection innerType ->
                            case msgValue of
                                UnitValue ->
                                    Combinator (Collection innerType) metadata (Array.append (Array.fromList [ initHelp config innerType ]) childModels)

                                _ ->
                                    model

                        _ ->
                            model

                PrefixMatch { next } ->
                    Array.Extra.update next (updateHelp config (String.fromInt next :: modelPath) msg) childModels
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
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0
            in
            PrefixMatch { next = next }

        else
            NoMatch


type Match
    = FullMatch
    | PrefixMatch { next : Int }
    | NoMatch


view : FormConfig -> IR.Gadget a -> Model -> H.Html Msg
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


viewHelp : FormConfig -> List Error -> Path -> Model -> List (H.Html Msg)
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
                            [ config.feedback error ]

                        else
                            []
                    )

        input =
            case model of
                Primitive primitiveType metadata modelValue ->
                    let
                        viewFor typ =
                            run .view config typ id modelValue
                                |> List.singleton
                    in
                    List.map (H.map (\msg -> Msg modelPath msg)) <|
                        H.label [ HA.for id ] [ H.text (maybeLabel metadata |> Maybe.withDefault id) ]
                            :: (case primitiveType of
                                    PUnit ->
                                        []

                                    PString ->
                                        viewFor .string

                                    PChar ->
                                        viewFor .char

                                    PInt ->
                                        viewFor .int

                                    PFloat ->
                                        viewFor .float

                                    PBool ->
                                        viewFor .bool
                               )

                Combinator combinatorType metadata childModels ->
                    case combinatorType of
                        Sum selected ->
                            let
                                childView =
                                    Array.get selected childModels
                                        |> Maybe.map (viewHelp config errs (String.fromInt selected :: modelPath))
                                        |> Maybe.withDefault [ H.text "sum error" ]

                                ( customLabel_, variantLabels ) =
                                    tools.decode "customLabel" (Gadget.tuple Gadget.string (Gadget.list Gadget.string)) metadata
                                        |> Maybe.withDefault ( pathToString modelPath, [] )
                            in
                            H.fieldset []
                                (H.legend [] [ H.text customLabel_ ]
                                    :: (childModels
                                            |> Array.toList
                                            |> List.indexedMap
                                                (\idx _ ->
                                                    let
                                                        childId =
                                                            pathToString (String.fromInt idx :: modelPath)
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
                                                                    |> Maybe.withDefault (maybeLabel metadata |> Maybe.withDefault "")
                                                                )
                                                            ]
                                                        ]
                                                )
                                       )
                                )
                                :: childView

                        Product _ ->
                            let
                                inner =
                                    Array.indexedMap (\idx childModel -> viewHelp config errs (String.fromInt idx :: modelPath) childModel) childModels
                                        |> Array.toList
                                        |> List.concat
                            in
                            case maybeLabel metadata of
                                Nothing ->
                                    inner

                                Just label_ ->
                                    [ H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) ]

                        Collection _ ->
                            [ H.fieldset []
                                (case maybeLabel metadata of
                                    Nothing ->
                                        []

                                    Just legend ->
                                        List.concat
                                            [ [ H.legend [] [ H.text legend ] ]
                                            , [ H.input [ HA.type_ "button", HE.onClick (Msg modelPath UnitValue), HA.value "Add an item" ] [] ]
                                            , childModels
                                                |> Array.indexedMap (\idx childModel -> viewHelp config errs (String.fromInt idx :: modelPath) childModel)
                                                |> Array.toList
                                                |> List.concat
                                            ]
                                )
                            ]

                        Variant ->
                            Array.indexedMap (\idx childModel -> viewHelp config errs (String.fromInt idx :: modelPath) childModel) childModels
                                |> Array.toList
                                |> List.concat
    in
    input ++ feedback


submit : FormConfig -> IR.Gadget a -> Model -> Result (List Error) a
submit config gadget model =
    submitHelp config [] model
        |> Result.andThen
            (\ir ->
                IR.toOutput gadget ir
                    |> Result.mapError List.singleton
            )


submitHelp : FormConfig -> Path -> Model -> Result (List Error) Value
submitHelp config path model =
    case model of
        Primitive primitiveType _ modelValue ->
            let
                submit_ getter =
                    let
                        (Control c) =
                            getter config
                    in
                    c.submit path modelValue
            in
            case primitiveType of
                PUnit ->
                    Ok UnitValue

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

        Combinator combinatorType _ children ->
            case combinatorType of
                Sum selected ->
                    Array.get selected children
                        |> Result.fromMaybe [ { path = path, error = "Invalid Sum variant selection" } ]
                        |> Result.andThen
                            (\v ->
                                case v of
                                    Combinator Variant _ args ->
                                        Array.toList args
                                            |> List.indexedMap (\idx arg -> submitHelp config (String.fromInt idx :: String.fromInt selected :: path) arg)
                                            |> combineAndAccumulateErrors
                                            |> Result.andThen (argsListToVariantValue >> Result.mapError (\error -> [ { path = String.fromInt selected :: path, error = error } ]))

                                    _ ->
                                        Err [ { path = path, error = "The child of a Sum should always be a Variant" } ]
                            )
                        |> Result.map (\v -> CustomValue selected ( "", v ))

                Collection _ ->
                    Array.toList children
                        |> List.indexedMap (\idx child -> submitHelp config (String.fromInt idx :: path) child)
                        |> combineAndAccumulateErrors
                        |> Result.map IR.ListValue

                Variant ->
                    Err [ { path = path, error = "This branch should be unreachable, as Variants should always be handled by the Sum case" } ]

                Product productType ->
                    Array.toList children
                        |> List.indexedMap (\idx child -> submitHelp config (String.fromInt idx :: path) child)
                        |> combineAndAccumulateErrors
                        |> Result.andThen
                            (\fields ->
                                case productType of
                                    Record ->
                                        List.map (Tuple.pair "") fields |> IR.RecordValue |> Ok

                                    Tuple ->
                                        case fields of
                                            [ a, b ] ->
                                                IR.TupleValue a b |> Ok

                                            _ ->
                                                Err [ { path = path, error = "Tuple has wrong number of elements" } ]

                                    Triple ->
                                        case fields of
                                            [ a, b, c ] ->
                                                IR.TripleValue a b c |> Ok

                                            _ ->
                                                Err [ { path = path, error = "Triple has wrong number of elements" } ]
                            )


combineAndAccumulateErrors : List (Result (List error) a) -> Result (List error) (List a)
combineAndAccumulateErrors list =
    combineAndAccumulateErrorsHelp list (Ok [])


combineAndAccumulateErrorsHelp : List (Result (List error) value) -> Result (List error) (List value) -> Result (List error) (List value)
combineAndAccumulateErrorsHelp list acc =
    case list of
        (Ok thisOutput) :: rest ->
            combineAndAccumulateErrorsHelp rest <|
                case acc of
                    Ok outputs ->
                        Ok (thisOutput :: outputs)

                    Err errs ->
                        Err errs

        (Err thisError) :: rest ->
            combineAndAccumulateErrorsHelp rest <|
                case acc of
                    Ok outputs ->
                        Err thisError

                    Err errors ->
                        Err (thisError ++ errors)

        [] ->
            case acc of
                Ok outputs ->
                    Ok (List.reverse outputs)

                Err errors ->
                    Err (List.reverse errors)


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
customLabels : String -> List String -> IR.Gadget a -> IR.Gadget a
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
                    |> Result.Extra.extract (.error >> H.text)
                    |> H.map (\msg -> IR.fromInput config.msg msg)
        , submit =
            \path model ->
                IR.toOutput config.model model
                    |> Result.mapError (\e -> [ { e | path = path } ])
                    |> Result.andThen (\value -> config.submit value |> Result.mapError (\error -> [ { path = path, error = error } ]))
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
                    , HA.attribute "inputmode" "numeric"
                    , HE.onInput identity
                    , HA.id id
                    , HA.value model
                    ]
                    []
        , submit =
            \model ->
                String.toInt model
                    |> Result.fromMaybe "Not an integer"
        }


float : Control
float =
    control
        { model = Gadget.string
        , msg = Gadget.string
        , output = Gadget.float
        , init = ""
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
                    |> Result.fromMaybe "Not a decimal number"
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
                    , HA.value model
                    ]
                    []
        , submit = Ok
        }


bool : Control
bool =
    control
        { model = Gadget.bool
        , msg = Gadget.bool
        , output = Gadget.bool
        , init = False
        , update = \msg _ -> msg
        , view =
            \id _ ->
                H.input
                    [ HA.type_ "checkbox"
                    , HE.onCheck identity
                    , HA.id id
                    ]
                    []
        , submit = Ok
        }


char : Control
char =
    control
        { model = Gadget.string
        , msg = Gadget.maybe Gadget.char
        , output = Gadget.char
        , init = ""
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
                    |> Result.fromMaybe "Cannot be blank"
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


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"
