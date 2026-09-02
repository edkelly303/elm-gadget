module Main exposing (..)

import Browser
import Fuzz
import Gadget
import Gadget.Adapter.Diff
import Gadget.Adapter.Form
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
import Random


type alias Person =
    { name : String
    , heightInCentimetres : List Float
    , pets : List Pet
    , tuple : ( Bool, Bool )
    , triple : ( Bool, Bool, Bool )
    }


type Pet
    = Dog { name : String }
    | Robot Char (Maybe Int)


personGadget : Gadget.Gadget Person
personGadget =
    Gadget.record Person
        |> Gadget.field "name"
            .name
            (Gadget.string
                |> Gadget.Adapter.Random.choose "Ed" [ "Leonardo", "Wolfgang", "Rupert", "Mario", "Martin" ]
                |> Gadget.Adapter.Form.label "Name"
            )
        |> Gadget.field "heightInCentimetres"
            .heightInCentimetres
            (Gadget.list
                (Gadget.float
                    |> Gadget.Adapter.Random.range 100 180
                    |> Gadget.Adapter.Form.label "What is your height?"
                )
                |> Gadget.Adapter.Form.label "Heights"
            )
        |> Gadget.field "pets"
            .pets
            (Gadget.list petGadget
                |> Gadget.Adapter.Random.listLength 0 3
                |> Gadget.Adapter.Form.label "Pets"
            )
        |> Gadget.field "tuple"
            .tuple
            (Gadget.tuple
                (Gadget.bool |> Gadget.Adapter.Form.label "one")
                (Gadget.bool |> Gadget.Adapter.Form.label "two")
                |> Gadget.Adapter.Form.label "Tuple"
            )
        |> Gadget.field "triple"
            .triple
            (Gadget.triple
                (Gadget.bool |> Gadget.Adapter.Form.label "one")
                (Gadget.bool |> Gadget.Adapter.Form.label "two")
                (Gadget.bool |> Gadget.Adapter.Form.label "three")
                |> Gadget.Adapter.Form.label "Triple"
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
            (Gadget.record (\name -> { name = name })
                |> Gadget.field "name"
                    .name
                    (Gadget.string
                        |> Gadget.Adapter.Fuzz.useOverride "dogName"
                        |> Gadget.Adapter.Random.choose "Rex" [ "Fido", "Kevin", "Rover", "Fifi", "George", "Winnie" ]
                        |> Gadget.Adapter.Form.label "What is your dog's name?"
                    )
                |> Gadget.endRecord
            )
        |> Gadget.variant2
            "Robot"
            Robot
            (Gadget.char
                |> Gadget.Adapter.Fuzz.useOverride "series"
                |> Gadget.Adapter.Random.choose 'A' (List.range 66 90 |> List.map Char.fromCode)
                |> Gadget.Adapter.Form.label "What is your robot's model series?"
            )
            (Gadget.maybe
                (Gadget.int
                    |> Gadget.Adapter.Fuzz.useOverride "model"
                    |> Gadget.Adapter.Random.range 1000 5000
                    |> Gadget.Adapter.Form.label "What is your robot's model number?"
                )
                |> Gadget.Adapter.Form.customLabels "Does your robot have a model number?" [ "Yes", "No" ]
            )
        |> Gadget.endCustom
        |> Gadget.Adapter.Form.customLabels "What type of pet do you have?" [ "Dog", "Robot" ]


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
    , form : Gadget.Adapter.Form.Model
    }


type Msg
    = UserClickedRegenerate
    | UserChangedPrettyWidth String
    | NewSeed Int
    | FormUpdated Gadget.Adapter.Form.Msg


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
            ( { model | form = form.update formMsg model.form }
            , Cmd.none
            )


form =
    Gadget.Adapter.Form.fromGadget FormUpdated gadget


init _ =
    ( { seed = 0
      , prettyWidth = 120
      , form = form.init
      }
    , Cmd.none
    )


gadget =
    personGadget



-- Gadget.maybe Gadget.int
-- Gadget.result
--     (Gadget.result
--         (Gadget.int |> Gadget.Adapter.Form.label "hello")
--         (Gadget.int |> Gadget.Adapter.Form.label "world")
--     )
--     (Gadget.result Gadget.int Gadget.int)
-- petGadget
-- Gadget.record (\x y -> { x = x, y = y })
--     |> Gadget.field "x" .x (Gadget.int |> Gadget.Adapter.Form.label "How much is x?")
--     |> Gadget.field "y"
--         .y
--         (Gadget.maybe (Gadget.string |> Gadget.Adapter.Form.label "What is y?")
--             |> Gadget.Adapter.Form.customLabels "Is there a value for y?"
--                 [ "Yes", "No" ]
--         )
--     |> Gadget.endRecord


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

        formOutput =
            form.submit model.form
                |> Result.mapError (\errors -> List.map (\{ id, error } -> id ++ ": " ++ error) errors |> String.join "\n")

        pretty g x =
            H.pre [] [ H.text (Gadget.Adapter.Pretty.print g model.prettyWidth x) ]

        diff =
            Result.map (Gadget.Adapter.Diff.diff gadget firstValue) formOutput

        patched =
            Result.andThen (\diff_ -> Gadget.Adapter.Diff.patch gadget diff_ firstValue) diff

        encoded =
            JE.encode 2 (Gadget.Adapter.Json.encode gadget firstValue)

        decoded =
            JD.decodeString (Gadget.Adapter.Json.decoder gadget) encoded
                |> Result.mapError (\_ -> "Decoding failed!")

        printed =
            Gadget.Adapter.String.print gadget firstValue

        parsed =
            Parser.run (Gadget.Adapter.String.parser gadget) printed
                |> Result.mapError Parser.deadEndsToString
    in
    H.div []
        [ H.h1 [] [ H.text "elm-gadget examples" ]
        , widthAdjuster model
        , demo "Form"
            [ form.view model.form
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
            [ H.button [ HE.onClick UserClickedRegenerate ] [ H.text "Click to regenerate!"]
            , pretty gadget firstValue 
            ]
        , demo "Differ & patcher"
            [ pretty (Gadget.result Gadget.string Gadget.Adapter.Diff.changes) diff
            , head "Patch generated value with diff"
            , pretty (Gadget.result Gadget.string gadget) patched
            , head "Patched value equals form value?"
            , pretty Gadget.bool (patched == formOutput)
            ]
        , demo "Html viewer"
            [ Gadget.Adapter.Html.view gadget firstValue ]
        , demo "String printer"
            [ H.code [ HA.class "withoutSpaces" ] [ H.text printed ] ]
        , demo "String parser"
            [ pretty (Gadget.result Gadget.string gadget) parsed ]
        , demo "JSON encoder"
            [ H.pre [] [ H.text encoded ] ]
        , demo "JSON decoder"
            [ pretty (Gadget.result Gadget.string gadget) decoded ]
        , demo "Fuzzer"
            [ pretty (Gadget.list gadget) fuzzed ]
        , demo "Quine" 
            [ H.pre [] [ H.text (Gadget.Adapter.Quine.quine model.prettyWidth gadget) ] ]
        ]


widthAdjuster model =
    H.span [HA.class "widthAdjuster"]
        [ H.strong [] [H.text "Pretty printer width: "]
        , 
             H.input
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
    H.details [HA.class "demo"] (H.summary [] [ H.strong [] [ H.text title ] ] :: contents)


head : String -> H.Html msg
head txt =
    H.h2 [] [ H.text txt ]
