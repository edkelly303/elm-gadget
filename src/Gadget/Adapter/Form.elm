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
import Dict exposing (Dict)
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
    = Combinator CombinatorType IR.Metadata (Dict String Model)
    | Record IR.Metadata (Dict String ( Int, Model ))
    | Tuple IR.Metadata Model Model
    | Triple IR.Metadata Model Model Model
    | Primitive PrimitiveType IR.Metadata Value


{-| TODO
-}
type Msg
    = Msg Path Value


type CombinatorType
    = Collection Type
    | Variant
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
                        |> List.indexedMap
                            (\idx ( n, v ) ->
                                ( String.fromInt idx
                                , v
                                    |> variantTypeToArgsDict
                                    |> Dict.map (\_ arg -> initHelp config arg)
                                    |> Combinator Variant IR.emptyMetadata
                                )
                            )
                        |> Dict.fromList
            in
            Combinator (Sum 0) m variants

        RecordType m namedFieldTypes ->
            let
                fields =
                    namedFieldTypes
                        |> List.indexedMap (\idx ( n, f ) -> ( n, ( idx, f ) ))
                        |> Dict.fromList
                        |> Dict.map (\_ ( idx, fieldType ) -> ( idx, initHelp config fieldType ))
            in
            Record m fields

        ListType m innerType ->
            Combinator (Collection innerType) m Dict.empty

        LazyType _ innerType ->
            initHelp config (innerType ())

        TupleType m a b ->
            Tuple m (initHelp config a) (initHelp config b)

        TripleType m a b c ->
            Triple m (initHelp config a) (initHelp config b) (initHelp config c)


update : FormConfig -> Msg -> Model -> Model
update config msg model =
    updateHelp config [] msg model


updateHelp : FormConfig -> Path -> Msg -> Model -> Model
updateHelp config modelPath ((Msg msgPath msgValue) as msg) model =
    case model of
        Primitive primitiveType metadata modelValue ->
            Primitive primitiveType metadata <|
                if (modelPath |> Debug.log "modelPath") == (msgPath |> Debug.log "msgPath") then
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

        Record metadata fields ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    model

                PrefixMatch { next } ->
                    Dict.update next
                        (\m ->
                            case m of
                                Just ( idx, field ) ->
                                    Just ( idx, updateHelp config (next :: modelPath) msg field )

                                Nothing ->
                                    Nothing
                        )
                        fields
                        |> Record metadata

                NoMatch ->
                    model

        Tuple metadata a b ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    model

                PrefixMatch { next } ->
                    case next of
                        "0" ->
                            Tuple metadata (updateHelp config ("0" :: modelPath) msg a) b

                        "1" ->
                            Tuple metadata a (updateHelp config ("1" :: modelPath) msg b)

                        _ ->
                            model

                NoMatch ->
                    model

        Triple metadata a b c ->
            case matchPath msgPath modelPath of
                FullMatch ->
                    model

                PrefixMatch { next } ->
                    case next of
                        "0" ->
                            Triple metadata (updateHelp config ("0" :: modelPath) msg a) b c

                        "1" ->
                            Triple metadata a (updateHelp config ("1" :: modelPath) msg b) c

                        "2" ->
                            Triple metadata a b (updateHelp config ("2" :: modelPath) msg c)

                        _ ->
                            model

                NoMatch ->
                    model

        Combinator combinatorType metadata childModels ->
            case matchPath msgPath modelPath of
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
                                    Combinator (Collection innerType)
                                        metadata
                                        (Dict.insert (String.fromInt (Dict.size childModels + 1))
                                            (initHelp config innerType)
                                            childModels
                                        )

                                _ ->
                                    model

                        Variant ->
                            model

                PrefixMatch { next } ->
                    Dict.update next
                        (\m ->
                            case m of
                                Just x ->
                                    Just (updateHelp config (next :: modelPath) msg x)

                                Nothing ->
                                    Nothing
                        )
                        childModels
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
                        |> Maybe.withDefault ""
            in
            PrefixMatch { next = next }

        else
            NoMatch


type Match
    = FullMatch
    | PrefixMatch { next : String }
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

                Record metadata fields ->
                    let
                        inner =
                            Dict.toList fields
                                |> List.sortBy (\( _, ( idx, _ ) ) -> idx)
                                |> List.map (\( name, ( _, childModel ) ) -> viewHelp config errs (name :: modelPath) childModel)
                                |> List.concat
                    in
                    case maybeLabel metadata of
                        Nothing ->
                            inner

                        Just label_ ->
                            [ H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) ]

                Tuple metadata a b ->
                    let
                        inner =
                            viewHelp config errs ("0" :: modelPath) a
                                ++ viewHelp config errs ("1" :: modelPath) b
                    in
                    case maybeLabel metadata of
                        Nothing ->
                            inner

                        Just label_ ->
                            [ H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) ]

                Triple metadata a b c ->
                    let
                        inner =
                            viewHelp config errs ("0" :: modelPath) a
                                ++ viewHelp config errs ("1" :: modelPath) b
                                ++ viewHelp config errs ("2" :: modelPath) c
                    in
                    case maybeLabel metadata of
                        Nothing ->
                            inner

                        Just label_ ->
                            [ H.fieldset [] (H.legend [] [ H.text label_ ] :: inner) ]

                Combinator combinatorType metadata childModels ->
                    case combinatorType of
                        Sum selected ->
                            let
                                childView =
                                    Dict.get (String.fromInt selected) childModels
                                        |> Maybe.map (viewHelp config errs (String.fromInt selected :: modelPath))
                                        |> Maybe.withDefault [ H.text "sum error" ]

                                ( customLabel_, variantLabels ) =
                                    tools.decode "customLabel" (Gadget.tuple Gadget.string (Gadget.list Gadget.string)) metadata
                                        |> Maybe.withDefault ( pathToString modelPath, [] )
                            in
                            H.fieldset []
                                (H.legend [] [ H.text customLabel_ ]
                                    :: (childModels
                                            |> Dict.values
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
                                                |> Dict.map (\idx childModel -> viewHelp config errs (idx :: modelPath) childModel)
                                                |> Dict.values
                                                |> List.concat
                                            ]
                                )
                            ]

                        Variant ->
                            Dict.map (\idx childModel -> viewHelp config errs (idx :: modelPath) childModel) childModels
                                |> Dict.values
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

        Record metadata fields ->
            fields
                |> Dict.map (\key ( idx, child ) -> submitHelp config (key :: path) child |> Result.map (Tuple.pair idx))
                |> combineAndAccumulateErrorsDict
                |> Result.map
                    (\r ->
                        r
                            |> Dict.toList
                            |> List.sortBy (\( key, ( idx, child ) ) -> idx)
                            |> List.map (\( key, ( idx, child ) ) -> ( key, child ))
                            |> IR.RecordValue
                    )

        Tuple metadata a b ->
            Result.map2 IR.TupleValue
                (submitHelp config ("0" :: path) a)
                (submitHelp config ("1" :: path) b)

        Triple metadata a b c ->
            Result.map3 IR.TripleValue
                (submitHelp config ("0" :: path) a)
                (submitHelp config ("1" :: path) b)
                (submitHelp config ("2" :: path) c)

        Combinator combinatorType _ children ->
            case combinatorType of
                Sum selected ->
                    Dict.get (String.fromInt selected) children
                        |> Result.fromMaybe [ { path = path, error = "Invalid Sum variant selection" } ]
                        |> Result.andThen
                            (\v ->
                                case v of
                                    Combinator Variant _ args ->
                                        args
                                            |> Dict.map (\idx arg -> submitHelp config (idx :: String.fromInt selected :: path) arg)
                                            |> Dict.values
                                            |> combineAndAccumulateErrors
                                            |> Result.andThen (argsListToVariantValue >> Result.mapError (\error -> [ { path = String.fromInt selected :: path, error = error } ]))

                                    _ ->
                                        Err [ { path = path, error = "The child of a Sum should always be a Variant" } ]
                            )
                        |> Result.map (\v -> CustomValue selected ( "", v ))

                Collection _ ->
                    children
                        |> Dict.map (\idx child -> submitHelp config (idx :: path) child)
                        |> Dict.values
                        |> combineAndAccumulateErrors
                        |> Result.map IR.ListValue

                Variant ->
                    Err [ { path = path, error = "This branch should be unreachable, as Variants should always be handled by the Sum case" } ]


combineAndAccumulateErrorsDict :
    Dict String (Result (List error) a)
    -> Result (List error) (Dict String a)
combineAndAccumulateErrorsDict dict =
    combineAndAccumulateErrorsDictHelp dict (Ok Dict.empty)


combineAndAccumulateErrorsDictHelp :
    Dict String (Result (List error) value)
    -> Result (List error) (Dict String value)
    -> Result (List error) (Dict String value)
combineAndAccumulateErrorsDictHelp dict acc =
    Dict.foldl
        (\k v out ->
            case v of
                Ok thisOutput ->
                    case out of
                        Ok outputs ->
                            Ok (Dict.insert k thisOutput outputs)

                        Err errs ->
                            Err errs

                Err thisError ->
                    case acc of
                        Ok outputs ->
                            Err thisError

                        Err errs ->
                            Err (thisError ++ errs)
        )
        acc
        dict


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


variantTypeToArgsDict : VariantType -> Dict.Dict String Type
variantTypeToArgsDict v =
    Dict.fromList <|
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


pathToString : Path -> String
pathToString path =
    List.reverse path
        |> String.join "-"
