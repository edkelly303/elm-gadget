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
import List.Extra
import Parser
import Process
import Random
import Set
import Task


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
    }


type Msg
    = UserClickedRegenerate
    | UserChangedPrettyWidth String
    | NewSeed Int


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


init _ =
    ( { seed = 10
      , prettyWidth = 120
      }
    , Cmd.none
    )


gadget =
    databaseGadget


type PersonId
    = PersonId Int


personIdGadget =
    Gadget.map PersonId (\(PersonId n) -> n) Gadget.int


type alias Person =
    { id : PersonId, name : String }


personGadget =
    Gadget.record Person
        |> Gadget.field "id"
            .id
            (Gadget.Adapter.Fuzz.isPrimaryKey "personId" personIdGadget)
        |> Gadget.field "name" .name Gadget.string
        |> Gadget.endRecord


type alias Pet =
    { name : String, owner : PersonId }


petGadget =
    Gadget.record Pet
        |> Gadget.field "name" .name Gadget.string
        |> Gadget.field "owner"
            .owner
            (Gadget.Adapter.Fuzz.hasForeignKey "personId" personIdGadget)
        |> Gadget.endRecord


type alias Database =
    { people : List Person, pets : List Pet }


databaseGadget =
    Gadget.record Database
        |> Gadget.field "people" .people (Gadget.list personGadget)
        |> Gadget.field "pets" .pets (Gadget.list petGadget)
        |> Gadget.endRecord


databaseFuzzer : Fuzz.Fuzzer Database
databaseFuzzer =
    Gadget.Adapter.Fuzz.fuzzer databaseGadget


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

        secondValue =
            Random.step randomGenerator (Random.initialSeed (model.seed + 1))
                |> Tuple.first
                |> Result.fromMaybe "Generator failed"

        pretty g x =
            H.pre [] [ H.text (Gadget.Adapter.Pretty.print g model.prettyWidth x) ]

        diff =
            Result.map2 (Gadget.Adapter.Diff.diff gadget) firstValue secondValue

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
            , pretty Gadget.bool (patched == secondValue)
            ]
        , demo "Html viewer"
            [ head "Randomly generated value"
            , Result.map (Gadget.Adapter.Html.view gadget) firstValue |> Result.withDefault (H.text "")
            , case secondValue of
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
