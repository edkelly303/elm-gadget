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
                        |> Gadget.Adapter.Form.label "What is your dog's name?"
                    )
                |> Gadget.endRecord
            )
        |> Gadget.variant2
            "Robot"
            Robot
            (Gadget.char
                |> Gadget.Adapter.Fuzz.label "series"
                |> Gadget.Adapter.Random.choose 'A' (List.range 66 90 |> List.map Char.fromCode)
                |> Gadget.Adapter.Form.label "What is your robot's model series?"
            )
            (Gadget.maybe
                (Gadget.int
                    |> Gadget.Adapter.Fuzz.label "model"
                    |> Gadget.Adapter.Random.range 1000 5000
                    |> Gadget.Adapter.Form.label "What is your robot's model number?"
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
    -- personGadget
    -- Gadget.maybe Gadget.int
    -- Gadget.result
    --     (Gadget.result
    --         (Gadget.int |> Gadget.Adapter.Form.label "hello")
    --         (Gadget.int |> Gadget.Adapter.Form.label "world")
    --     )
    --     (Gadget.result Gadget.int Gadget.int)
    -- petGadget
    Gadget.record (\x y -> { x = x, y = y })
        |> Gadget.field "x" .x (Gadget.int |> Gadget.Adapter.Form.label "How much is x?")
        |> Gadget.field "y"
            .y
            (Gadget.maybe (Gadget.string |> Gadget.Adapter.Form.label "What is y?")
                |> Gadget.Adapter.Form.customLabels "Is there a value for y?"
                    [ "Yes", "No" ]
            )
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

        firstValue =
            Random.step randomGenerator (Random.initialSeed model.seed)
                |> Tuple.first

        formOutput =
            form.submit model.form
                |> Result.mapError (\{ id, error } -> error)

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
        , H.button [ HE.onClick UserClickedRegenerate ] [ H.text "Click to regenerate!" ]
        , head "Form"
        , form.view model.form
        , H.pre []
            [ H.text
                (formOutput
                    |> Gadget.Adapter.Pretty.print
                        (Gadget.result
                            Gadget.string
                            -- (Gadget.record (\id error -> { id = id, error = error })
                            --     |> Gadget.field "id" .id Gadget.string
                            --     |> Gadget.field "error" .error Gadget.string
                            --     |> Gadget.endRecord
                            -- )
                            gadget
                        )
                        model.prettyWidth
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
        , head "Randomly generated value"
        , pretty gadget firstValue
        , head "Diff between generated and form values"
        , pretty (Gadget.result Gadget.string Gadget.Adapter.Diff.changes) diff
        , head "Patch generated value with diff"
        , pretty (Gadget.result Gadget.string gadget) patched
        , head "Patched value equals form value?"
        , pretty Gadget.bool (patched == formOutput)
        , head "Html viewer (generated value)"
        , Gadget.Adapter.Html.view gadget firstValue
        , head "String printer (generated value)"
        , H.code [ HA.class "withoutSpaces" ] [ H.text printed ]
        , head "String parser (generated value)"
        , pretty (Gadget.result Gadget.string gadget) parsed
        , head "JSON encoder (generated value)"
        , H.pre [] [ H.text encoded ]
        , head "JSON decoder (generated value)"
        , pretty (Gadget.result Gadget.string gadget) decoded
        , head "Fuzzer"
        , pretty (Gadget.list gadget) fuzzed
        ]


head : String -> H.Html msg
head txt =
    H.h2 [] [ H.text txt ]
