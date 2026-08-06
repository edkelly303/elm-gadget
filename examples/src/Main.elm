module Main exposing (..)

import Browser
import Fuzz
import Gadget
import Gadget.Adapter.Diff
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Html
import Gadget.Adapter.Json
import Gadget.Adapter.Random
import Gadget.Adapter.String
import Gadget.IR
import Gadget.Named
import Html
import Html.Attributes
import Html.Events
import Json.Decode as JD
import Json.Encode as JE
import Parser
import Random


type alias Person =
    { name : String
    , heightInCentimetres : Float
    , pets : List Pet
    }


type Pet
    = Dog { name : String }
    | Robot Char Int


personGadget : Gadget.Gadget Person
personGadget =
    Gadget.Named.record Person
        |> Gadget.Named.field "name"
            .name
            (Gadget.string
                |> Gadget.Adapter.Random.label "name"
                |> Gadget.Adapter.Fuzz.label "name"
            )
        |> Gadget.Named.field "heightInCentimetres"
            .heightInCentimetres
            (Gadget.float |> Gadget.Adapter.Random.floatRange 100 180)
        |> Gadget.Named.field "pets"
            .pets
            (Gadget.list petGadget |> Gadget.Adapter.Random.listLength 0 3)
        |> Gadget.Named.endRecord


petGadget : Gadget.Gadget Pet
petGadget =
    Gadget.Named.custom
        (\dog robot variant ->
            case variant of
                Dog name ->
                    dog name

                Robot series model ->
                    robot series model
        )
        |> Gadget.Named.variant1 "Dog"
            Dog
            (Gadget.record (\name -> { name = name })
                |> Gadget.field .name
                    (Gadget.string
                        |> Gadget.Adapter.Fuzz.label "dogName"
                        |> Gadget.Adapter.Random.label "dogName"
                    )
                |> Gadget.endRecord
            )
        |> Gadget.Named.variant2 "Robot"
            Robot
            (Gadget.char
                |> Gadget.Adapter.Fuzz.label "series"
                |> Gadget.Adapter.Random.label "series"
            )
            (Gadget.int
                |> Gadget.Adapter.Fuzz.label "model"
                |> Gadget.Adapter.Random.intRange 1000 5000
            )
        |> Gadget.Named.endCustom


main : Program () ( Int, Int ) Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }


type Msg
    = Clicked
    | NewSeeds ( Int, Int )


update msg model =
    case msg of
        Clicked ->
            ( model
            , Random.generate NewSeeds (Random.pair (Random.int 0 Random.maxInt) (Random.int 0 Random.maxInt))
            )

        NewSeeds newSeeds ->
            ( newSeeds
            , Cmd.none
            )


init _ =
    ( ( 0, 1 )
    , Cmd.none
    )


view : ( Int, Int ) -> Html.Html Msg
view ( seed1, seed2 ) =
    let
        gadget =
            personGadget

        fuzzOverrides =
            [ Gadget.Adapter.Fuzz.override "name" Gadget.string (Fuzz.oneOf (List.map Fuzz.constant [ "Ed", "Mario", "Leonardo", "Jeroen" ]))
            , Gadget.Adapter.Fuzz.override "dogName" Gadget.string (Fuzz.oneOf (List.map Fuzz.constant [ "Fido", "Kevin", "Rover", "Fifi" ]))
            , Gadget.Adapter.Fuzz.override "series" Gadget.char (Fuzz.oneOf (List.range 65 90 |> List.map Char.fromCode |> List.map Fuzz.constant))
            , Gadget.Adapter.Fuzz.override "model" Gadget.int (Fuzz.oneOf (List.range 1 5 |> List.map (\n -> n * 1000) |> List.map Fuzz.constant))
            ]

        fuzzer =
            Gadget.Adapter.Fuzz.fuzzerWithOverrides fuzzOverrides gadget

        fuzzed =
            Fuzz.examples 1 fuzzer

        randomOverrides =
            [ Gadget.Adapter.Random.override "name" Gadget.string (Random.uniform "Bill" [ "George", "Sue" ])
            , Gadget.Adapter.Random.override "dogName" Gadget.string (Random.uniform "Max" [ "Archie", "Finch" ])
            , Gadget.Adapter.Random.override "series" Gadget.char (Random.uniform 'A' (List.range 66 90 |> List.map Char.fromCode))
            ]

        randomGenerator =
            Gadget.Adapter.Random.generatorWithOverrides randomOverrides gadget

        firstValue =
            Random.step randomGenerator (Random.initialSeed seed1)
                |> Tuple.first

        secondValue =
            Random.step randomGenerator (Random.initialSeed seed2)
                |> Tuple.first

        diff =
            Gadget.Adapter.Diff.diff gadget firstValue secondValue

        patched =
            Gadget.Adapter.Diff.patch gadget diff firstValue

        encoded =
            JE.encode 2 (Gadget.Adapter.Json.encode gadget firstValue)

        decoded =
            JD.decodeString (Gadget.Adapter.Json.decoder gadget) encoded

        printed =
            Gadget.Adapter.String.print gadget firstValue

        parsed =
            Parser.run (Gadget.Adapter.String.parser gadget) printed
    in
    Html.div []
        [ Html.h1 [] [ Html.text "elm-gadget examples" ]
        , Html.button [ Html.Events.onClick Clicked ] [ Html.text "Click to regenerate!" ]
        , head "Random generator (first value)"
        , show firstValue
        , head "Random generator (second value)"
        , show secondValue
        , head "Diff between first & second values"
        , show diff
        , head "Patch first value with diff"
        , show patched
        , head "Patched value equals second value?"
        , show (patched == Ok secondValue)
        , head "Html viewer (first value)"
        , Gadget.Adapter.Html.view gadget firstValue
        , head "Html viewer (second value)"
        , Gadget.Adapter.Html.view gadget secondValue
        , head "Printer (first value)"
        , Html.code [ Html.Attributes.class "withoutSpaces" ] [ Html.text printed ]
        , head "Parser (first value)"
        , show parsed
        , head "JSON encoder (first value)"
        , Html.pre [] [ Html.text encoded ]
        , head "JSON decoder (first value)"
        , show decoded
        , head "Fuzzer"
        , show fuzzed
        ]


head : String -> Html.Html msg
head txt =
    Html.h2 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.code [] [ Html.text (Debug.toString a) ]
