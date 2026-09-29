module Main exposing (..)

import Browser
import Fuzz
import Gadget
import Gadget.Adapter.Diff
import Gadget.Adapter.Form.Lamdera as Form
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Html
import Gadget.Adapter.Json
import Gadget.Adapter.Pretty
import Gadget.Adapter.Quine
import Gadget.Adapter.Random
import Gadget.Adapter.String
import Gadget.IR
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Parser
import Process
import Random
import Task


type alias Person =
    { name : String
    , heightInCentimetres : Float
    , pets : List Pet
    , tuple : ( Bool, Bool )
    , triple : ( Bool, Bool, Bool )
    }


type Pet
    = Dog { dogName : String }
    | Robot Char (Maybe Int)


personGadget : Gadget.Gadget Person
personGadget =
    Gadget.record Person
        |> Gadget.field "name"
            .name
            (Gadget.string
                |> Gadget.filterMap
                    (\s ->
                        case
                            List.filterMap identity
                                [ if String.isEmpty s then
                                    Just "This must not be blank"

                                  else
                                    Nothing
                                , if String.length s < 2 then
                                    Just "This must be at least 2 characters"

                                  else
                                    Nothing
                                ]
                        of
                            [] ->
                                Ok s

                            errs ->
                                Err errs
                    )
                    identity
                |> Gadget.Adapter.Random.choose "Ed" [ "Leonardo", "Wolfgang", "Rupert", "Mario", "Martin" ]
                |> Form.label "What is your name?"
            )
        |> Gadget.field "heightInCentimetres"
            .heightInCentimetres
            (Gadget.float
                |> Gadget.Adapter.Random.range 100 180
                |> Form.label "What is your height (in centimetres)?"
                |> Form.override "heightInCentimetres"
                |> Gadget.filterMap
                    (\f ->
                        if f < 50 then
                            Err [ "This must be at least 50cm" ]

                        else
                            Ok f
                    )
                    identity
            )
        |> Gadget.field "pets"
            .pets
            (Gadget.list petGadget
                |> Gadget.Adapter.Random.listLength 0 3
                |> Form.label "Do you have any pets?"
            )
        |> Gadget.field "tuple"
            .tuple
            (Gadget.tuple
                (Gadget.bool |> Form.label "one")
                (Gadget.bool |> Form.label "two")
                |> Form.label "What's your favourite pair of booleans?"
            )
        |> Gadget.field "triple"
            .triple
            (Gadget.triple
                (Gadget.bool |> Form.label "one")
                (Gadget.bool |> Form.label "two")
                (Gadget.bool |> Form.label "three")
                |> Form.label "And what about your favourite triple?"
            )
        |> Gadget.endRecord


petGadget : Gadget.Gadget Pet
petGadget =
    Gadget.custom
        (\dog robot variant ->
            case variant of
                Dog name ->
                    dog name

                Robot series model ->
                    robot series model
        )
        |> Gadget.variant1
            "Dog"
            Dog
            (Gadget.record (\dogName -> { dogName = dogName })
                |> Gadget.field "dogName"
                    .dogName
                    (Gadget.string
                        |> Gadget.Adapter.Fuzz.useOverride "dogName"
                        |> Gadget.Adapter.Random.choose "Rex" [ "Fido", "Kevin", "Rover", "Fifi", "George", "Winnie" ]
                        |> Form.label "What is your dog's name?"
                        |> Gadget.filterMap
                            (\s ->
                                if String.isEmpty s then
                                    Err [ "This must not be blank" ]

                                else
                                    Ok s
                            )
                            identity
                    )
                |> Gadget.endRecord
            )
        |> Gadget.variant2
            "Robot"
            Robot
            (Gadget.char
                |> Gadget.Adapter.Fuzz.useOverride "series"
                |> Gadget.Adapter.Random.choose 'A' (List.range 66 90 |> List.map Char.fromCode)
                |> Form.label "What is your robot's model series?"
            )
            (Gadget.maybe
                (Gadget.int
                    |> Gadget.Adapter.Fuzz.useOverride "model"
                    |> Gadget.Adapter.Random.range 1000 5000
                    |> Form.label "What is your robot's model number?"
                )
                |> Form.customLabels "Does your robot have a model number?" [ "Yes", "No" ]
            )
        |> Gadget.endCustom
        |> Form.customLabels "What type of pet do you have?" [ "Dog", "Robot" ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }


type alias Model =
    { seed : Int
    , prettyWidth : Int
    , form : Form.Model
    }


type Msg
    = UserClickedRegenerate
    | UserChangedPrettyWidth String
    | NewSeed Int
    | FormUpdated Form.Msg
    | SimulateBackend ToBackend
    | SimulateFrontend ToFrontend


type ToBackend
    = ToBackend Form.Msg


type ToFrontend
    = ToFrontend Form.Msg


update msg model =
    case msg of
        UserClickedRegenerate ->
            ( model
            , Random.generate NewSeed (Random.int 0 Random.maxInt)
            )

        UserChangedPrettyWidth s ->
            ( { model | prettyWidth = String.toInt s |> Maybe.withDefault model.prettyWidth }
            , Cmd.none
            )

        NewSeed newSeed ->
            ( { model | seed = newSeed }
            , Cmd.none
            )

        FormUpdated formMsg ->
            let
                ( formModel, formCmd ) =
                    form.update formMsg model.form
            in
            ( { model | form = formModel }
            , formCmd
            )

        SimulateBackend (ToBackend toBackend) ->
            ( model
            , form.respond "" toBackend { int = 1, float = 1.2 }
            )

        SimulateFrontend (ToFrontend toFrontend) ->
            let
                ( formModel, formCmd ) =
                    form.updateFromBackend toFrontend model.form
            in
            ( { model | form = formModel }
            , Cmd.none
            )


form =
    let
        config =
            Form.defaultConfig
                |> Form.addOverride "heightInCentimetres" myFloat
                |> (\c -> { c | int = myInt })
    in
    Form.fromGadgetWithConfig
        config
        { mkMsg = FormUpdated
        , mkToBackend = lamdera_sendToBackend
        , mkToFrontend = lamdera_sendToFrontend
        , backendModelGadget = backendModelGadget
        }
        gadget


lamdera_sendToBackend : Form.Msg -> Cmd Msg
lamdera_sendToBackend toBackend =
    let
        _ =
            Debug.log "sendToBackend" toBackend
    in
    Task.perform (\() -> SimulateBackend (ToBackend toBackend)) (Process.sleep 0)


lamdera_sendToFrontend : sessionId -> Form.Msg -> Cmd Msg
lamdera_sendToFrontend sessionId toFrontend =
    Task.perform
        (\() ->
            let
                _ =
                    Debug.log "sendToFrontend" toFrontend
            in
            SimulateFrontend (ToFrontend toFrontend)
        )
        (Process.sleep 1000)


backendModelGadget =
    Gadget.record (\int float -> { int = int, float = float })
        |> Gadget.field "int" .int Gadget.int
        |> Gadget.field "float" .float Gadget.float
        |> Gadget.endRecord


myInt =
    Form.makeControl
        { toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.int
        , frontendModelGadget = Gadget.int
        , frontendMsgGadget = Gadget.unit
        , outputGadget = Gadget.int
        , view =
            \id model ->
                H.div []
                    [ H.text (String.fromInt model)
                    , H.input
                        [ HA.type_ "button"
                        , HE.onClick ()
                        , HA.value "click me"
                        ]
                        []
                    ]
        , update = \() model -> ( model, Form.ToBackend () )
        , updateFromBackend = \int model -> ( int, Form.Cmd Cmd.none )
        , subscriptions = \model -> Sub.none
        , submit = Ok
        , init = ( 0, Cmd.none )
        , placeholder = 0
        , load = identity
        , respond = \() { int } -> int
        }


myFloat =
    Form.makeControl
        { toBackendGadget = Gadget.unit
        , toFrontendGadget = Gadget.float
        , frontendModelGadget = Gadget.float
        , frontendMsgGadget = Gadget.unit
        , outputGadget = Gadget.float
        , view =
            \id model ->
                H.div []
                    [ H.text (String.fromFloat model)
                    , H.input
                        [ HA.type_ "button"
                        , HE.onClick ()
                        , HA.value "click me"
                        ]
                        []
                    ]
        , update = \() model -> ( model, Form.ToBackend () )
        , updateFromBackend = \float model -> ( float, Form.Cmd Cmd.none )
        , subscriptions = \model -> Sub.none
        , submit = Ok
        , init = ( 0.5, Cmd.none )
        , placeholder = 0.0
        , load = identity
        , respond = \() { float } -> float
        }


init _ =
    let
        ( formModel, formCmd ) =
            form.init

        loadedFormModel =
            form.load
                { name = "Ed"
                , heightInCentimetres = 180
                , pets = [ Robot 'A' (Just 3000) ]
                , tuple = ( True, False )
                , triple = ( False, True, False )
                }
    in
    ( { seed = 0
      , prettyWidth = 120
      , form = loadedFormModel
      }
    , formCmd
    )


gadget =
    personGadget



-- Gadget.maybe Gadget.int
-- Gadget.result
--     (Gadget.result
--         (Gadget.int |> Form.label "hello")
--         (Gadget.int |> Form.label "world")
--     )
--     (Gadget.result Gadget.int Gadget.int)
-- petGadget
-- Gadget.record (\x y -> { x = x, y = y })
--     |> Gadget.field "x" .x (Gadget.int |> Form.label "How much is x?")
--     |> Gadget.field "y"
--         .y
--         (Gadget.string
--             |> Form.label "What is y?"
--             |> Form.validate
--                 (\s ->
--                     if String.isEmpty s then
--                         Err "This must not be blank"
--                     else
--                         Ok s
--                 )
--         )
--     |> Gadget.endRecord
--     |> Form.validate (\_ -> Err "filterMap failed")


view : Model -> H.Html Msg
view model =
    let
        fuzzOverrides =
            [ Gadget.Adapter.Fuzz.override "dogName" Gadget.string (Fuzz.oneOf (List.map Fuzz.constant [ "Fido", "Kevin", "Rover", "Fifi", "George", "Winnie" ]))
            , Gadget.Adapter.Fuzz.override "series" Gadget.char (Fuzz.oneOf (List.range 65 90 |> List.map Char.fromCode |> List.map Fuzz.constant))
            , Gadget.Adapter.Fuzz.override "model" Gadget.int (Fuzz.oneOf (List.range 1 5 |> List.map (\n -> n * 1000) |> List.map Fuzz.constant))
            ]

        fuzzer =
            Gadget.Adapter.Fuzz.fuzzerWithOverrides fuzzOverrides gadget

        fuzzed =
            Fuzz.examples 1 fuzzer

        randomGenerator =
            Gadget.Adapter.Random.generator gadget

        firstValue =
            Random.step randomGenerator (Random.initialSeed model.seed)
                |> Tuple.first
                |> Result.fromMaybe "Generator failed"

        formOutput =
            form.submit model.form
                |> Result.mapError (\errors -> List.map (\{ path, error } -> "[" ++ String.join "-" path ++ "]: " ++ error) errors |> String.join "\n")

        pretty g x =
            H.pre [] [ H.text (Gadget.Adapter.Pretty.print g model.prettyWidth x) ]

        diff =
            Result.map2 (Gadget.Adapter.Diff.diff gadget) firstValue formOutput

        patched =
            Result.map2 (Gadget.Adapter.Diff.patch gadget) diff firstValue
                |> Result.andThen identity

        encoded =
            Result.map (Gadget.Adapter.Json.encode gadget >> JE.encode 2) firstValue

        decoded =
            encoded
                |> Result.andThen (JD.decodeString (Gadget.Adapter.Json.decoder gadget) >> Result.mapError (\_ -> "Decoding failed!"))

        printed =
            firstValue
                |> Result.map (Gadget.Adapter.String.print gadget)

        parsed =
            printed
                |> Result.andThen (Parser.run (Gadget.Adapter.String.parser gadget) >> Result.mapError Parser.deadEndsToString)
    in
    H.div []
        [ H.h1 [] [ H.text "elm-gadget examples" ]
        , widthAdjuster model
        , demo "Form"
            [ head "Form input"
            , form.view model.form
            , head "Form output"
            , H.pre []
                [ H.text
                    (formOutput
                        |> Gadget.Adapter.Pretty.print
                            (Gadget.result Gadget.string gadget)
                            model.prettyWidth
                    )
                ]
            ]
        , demo "Random generator"
            [ H.button [ HE.onClick UserClickedRegenerate ] [ H.text "Click to regenerate!" ]
            , Result.map (pretty gadget) firstValue |> Result.withDefault (H.text "")
            ]
        , demo "Differ & patcher"
            [ head "Diff between the randomly generated value and the form output value"
            , pretty (Gadget.result Gadget.string Gadget.Adapter.Diff.changes) diff
            , head "Result of patching the randomly generated value with diff"
            , patched
                |> Result.map (pretty gadget)
                |> Result.withDefault (H.text "")
            , head "Does the patched value equal the form output value?"
            , pretty Gadget.bool (patched == formOutput)
            ]
        , demo "Html viewer"
            [ head "Randomly generated value"
            , Result.map (Gadget.Adapter.Html.view gadget) firstValue |> Result.withDefault (H.text "")
            , case formOutput of
                Ok v ->
                    H.div [] [ head "Form output value", Gadget.Adapter.Html.view gadget v ]

                Err _ ->
                    H.text ""
            ]
        , demo "String printer"
            [ H.code [ HA.class "withoutSpaces" ] [ H.text (Result.withDefault "" printed) ] ]
        , demo "String parser"
            [ pretty (Gadget.result Gadget.string gadget) parsed ]
        , demo "JSON encoder"
            [ H.pre [] [ H.text (Result.withDefault "" encoded) ] ]
        , demo "JSON decoder"
            [ pretty (Gadget.result Gadget.string gadget) decoded ]
        , demo "Fuzzer"
            [ pretty (Gadget.list gadget) fuzzed ]
        , demo "Quine"
            [ H.pre [] [ H.text (Gadget.Adapter.Quine.quine model.prettyWidth gadget) ] ]
        ]


widthAdjuster model =
    H.span [ HA.class "widthAdjuster" ]
        [ H.strong [] [ H.text "Pretty printer width: " ]
        , H.input
            [ HA.type_ "range"
            , HA.min "0"
            , HA.max "120"
            , HA.step "10"
            , HA.value (String.fromInt model.prettyWidth)
            , HE.onInput UserChangedPrettyWidth
            , HA.list "markers"
            , HA.style "width" "500px"
            , HA.style "margin" "0px"
            ]
            []
        , H.text (" " ++ String.fromInt model.prettyWidth ++ " columns")
        , H.datalist
            [ HA.id "markers"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "justify-content" "space-between"
            , HA.style "writing-mode" "vertical-lr"
            , HA.style "width" "500px"
            ]
            (List.map
                (\n ->
                    H.option
                        [ HA.value (String.fromInt (10 * n))
                        , HA.style "padding" "0px"
                        ]
                        []
                )
                (List.range 0 12)
            )
        ]


demo title contents =
    H.details [ HA.class "demo" ] (H.summary [] [ H.strong [] [ H.text title ] ] :: contents)


head : String -> H.Html msg
head txt =
    H.h3 [] [ H.text txt ]
