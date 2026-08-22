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
            (Gadget.string |> Gadget.Adapter.Random.choose "Ed" [ "Leonardo", "Wolfgang", "Rupert", "Mario", "Martin" ])
        |> Gadget.field "heightInCentimetres"
            .heightInCentimetres
            (Gadget.list (Gadget.float |> Gadget.Adapter.Random.range 100 180))
        |> Gadget.field "pets"
            .pets
            (Gadget.list petGadget |> Gadget.Adapter.Random.listLength 0 3)
        |> Gadget.field "tuple" .tuple (Gadget.tuple Gadget.bool Gadget.bool)
        |> Gadget.field "triple" .triple (Gadget.triple Gadget.bool Gadget.bool Gadget.bool)
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
                        |> Gadget.Adapter.Fuzz.label "dogName"
                        |> Gadget.Adapter.Random.choose "Rex" [ "Fido", "Kevin", "Rover", "Fifi", "George", "Winnie" ]
                    )
                |> Gadget.endRecord
            )
        |> Gadget.variant2
            "Robot"
            Robot
            (Gadget.char
                |> Gadget.Adapter.Fuzz.label "series"
                |> Gadget.Adapter.Random.choose 'A' (List.range 66 90 |> List.map Char.fromCode)
            )
            (Gadget.maybe
                (Gadget.int
                    |> Gadget.Adapter.Fuzz.label "model"
                    |> Gadget.Adapter.Random.range 1000 5000
                )
            )
        |> Gadget.endCustom


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }


type alias Model =
    { seeds : ( Int, Int )
    , prettyWidth : Int
    , form : Gadget.Adapter.Form.Model
    }


type Msg
    = UserClickedRegenerate
    | UserChangedPrettyWidth String
    | NewSeeds ( Int, Int )
    | FormUpdated Gadget.Adapter.Form.Msg


update msg model =
    case msg of
        UserClickedRegenerate ->
            ( model
            , Random.generate NewSeeds (Random.pair (Random.int 0 Random.maxInt) (Random.int 0 Random.maxInt))
            )

        UserChangedPrettyWidth s ->
            ( { model | prettyWidth = String.toInt s |> Maybe.withDefault model.prettyWidth }
            , Cmd.none
            )

        NewSeeds newSeeds ->
            ( { model | seeds = newSeeds }
            , Cmd.none
            )

        FormUpdated formMsg ->
            ( { model | form = Gadget.Adapter.Form.update gadget formMsg model.form }
            , Cmd.none
            )


init _ =
    ( { seeds = ( 0, 1 )
      , prettyWidth = 120
      , form = Gadget.Adapter.Form.init gadget
      }
    , Cmd.none
    )


gadget =
    -- personGadget
    -- Gadget.maybe
    Gadget.record (\x y -> { x = x, y = y })
        |> Gadget.field "x" .x Gadget.int
        |> Gadget.field "y" .y Gadget.string
        |> Gadget.endRecord


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

        ( seed1, seed2 ) =
            model.seeds

        firstValue =
            Random.step randomGenerator (Random.initialSeed seed1)
                |> Tuple.first

        secondValue =
            Random.step randomGenerator (Random.initialSeed seed2)
                |> Tuple.first

        pretty g x =
            H.pre [] [ H.text (Gadget.Adapter.Pretty.print g model.prettyWidth x) ]

        diff =
            Gadget.Adapter.Diff.diff gadget firstValue secondValue

        patched =
            Gadget.Adapter.Diff.patch gadget diff firstValue

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
        , H.button [ HE.onClick UserClickedRegenerate ] [ H.text "Click to regenerate!" ]
        , head "Form"
        , Gadget.Adapter.Form.view gadget model.form |> H.map FormUpdated
        , H.pre []
            [ H.text
                (Gadget.Adapter.Form.submit gadget model.form
                    |> Gadget.Adapter.Pretty.print (Gadget.result Gadget.string gadget) model.prettyWidth
                )
            ]
        , head "Pretty printer width"
        , H.span []
            [ H.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "120"
                , HA.step "10"
                , HA.value (String.fromInt model.prettyWidth)
                , HE.onInput UserChangedPrettyWidth
                , HA.style "width" "500px"
                , HA.list "markers"
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
        , head "Quine"
        , H.pre [] [ H.text (Gadget.Adapter.Quine.quine model.prettyWidth gadget) ]
        , head "Random generator (first value, pretty-printed)"
        , pretty gadget firstValue
        , head "Random generator (second value, pretty-printed)"
        , pretty gadget secondValue
        , head "Diff between first & second values"
        , pretty Gadget.Adapter.Diff.changes diff
        , head "Patch first value with diff"
        , pretty (Gadget.result Gadget.string gadget) patched
        , head "Patched value equals second value?"
        , pretty Gadget.bool (patched == Ok secondValue)
        , head "Html viewer (first value)"
        , Gadget.Adapter.Html.view gadget firstValue
        , head "Html viewer (second value)"
        , Gadget.Adapter.Html.view gadget secondValue
        , head "String printer (first value)"
        , H.code [ HA.class "withoutSpaces" ] [ H.text printed ]
        , head "String parser (first value)"
        , pretty (Gadget.result Gadget.string gadget) parsed
        , head "JSON encoder (first value)"
        , H.pre [] [ H.text encoded ]
        , head "JSON decoder (first value)"
        , pretty (Gadget.result Gadget.string gadget) decoded
        , head "Fuzzer"
        , pretty (Gadget.list gadget) fuzzed
        ]


head : String -> H.Html msg
head txt =
    H.h2 [] [ H.text txt ]
