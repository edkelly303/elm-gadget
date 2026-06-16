module Main exposing (..)

import Browser
import Fuzz
import Gadget
import Gadget.Adapter.Diff
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Html
import Gadget.Adapter.Json
import Gadget.Adapter.Miner
import Gadget.Adapter.Miner2
import Gadget.Adapter.Random
import Gadget.Adapter.String
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

    --, pets : List Pet
    }


type Pet
    = Dog { name : String }
    | Robot Char Int


personGadget : Gadget.Gadget Person
personGadget =
    Gadget.record Person
        |> Gadget.field .name
            (Gadget.string
             -- |> Gadget.label "random-name"
             -- |> Gadget.label "fuzz-name"
            )
        |> Gadget.field .heightInCentimetres
            (Gadget.float
             -- |> Gadget.label "heightInCentimetres"
            )
        -- |> Gadget.field .pets
        --     (Gadget.list petGadget)
        |> Gadget.endRecord


petGadget : Gadget.Gadget Pet
petGadget =
    Gadget.custom
        (\dog robot variant ->
            case variant of
                Dog rec ->
                    dog rec

                Robot series model ->
                    robot series model
        )
        |> Gadget.variant1 Dog
            (Gadget.record (\name -> { name = name })
                |> Gadget.field .name
                    (Gadget.string |> Gadget.label "dogName")
                |> Gadget.endRecord
            )
        |> Gadget.variant2 Robot
            (Gadget.char |> Gadget.label "series")
            (Gadget.int |> Gadget.label "model")
        |> Gadget.endCustom


main : Program () ( Int, Int ) Msg
main =
    Browser.element
        { view = view2
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }

view2 _ = 
    case Gadget.Adapter.Miner2.exampleResult of
        Err es -> 
            Html.ul [] (List.map (\item -> Html.li [] [Html.text item]) es)
        Ok _ -> Html.text "OK!"

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
            [ Gadget.Adapter.Fuzz.override "fuzz-name" Gadget.string (Fuzz.oneOf (List.map Fuzz.constant [ "Ed", "Mario", "Leonardo", "Jeroen" ]))
            , Gadget.Adapter.Fuzz.override "heightInCentimetres" Gadget.float (Fuzz.floatRange 160 196)
            , Gadget.Adapter.Fuzz.override "dogName" Gadget.string (Fuzz.oneOf (List.map Fuzz.constant [ "Fido", "Kevin", "Rover", "Fifi" ]))
            , Gadget.Adapter.Fuzz.override "series" Gadget.char (Fuzz.oneOf (List.range 65 90 |> List.map Char.fromCode |> List.map Fuzz.constant))
            , Gadget.Adapter.Fuzz.override "model" Gadget.int (Fuzz.oneOf (List.range 1 5 |> List.map (\n -> n * 1000) |> List.map Fuzz.constant))
            ]

        fuzzer =
            Gadget.Adapter.Fuzz.fuzzerWithOverrides fuzzOverrides gadget

        fuzzed =
            Fuzz.examples 50 fuzzer

        randomOverrides =
            [ Gadget.Adapter.Random.override "random-name" Gadget.string (Random.uniform "Bill" [ "George", "Sue" ])
            , Gadget.Adapter.Random.override "heightInCentimetres" Gadget.float (Random.float 160 196)
            , Gadget.Adapter.Random.override "dogName" Gadget.string (Random.uniform "Fido" [ "Kevin", "Rover", "Fifi" ])
            , Gadget.Adapter.Random.override "series" Gadget.char (Random.uniform 'A' (List.range 66 90 |> List.map Char.fromCode))
            , Gadget.Adapter.Random.override "model" Gadget.int (Random.uniform 1000 (List.range 2 5 |> List.map (\n -> n * 1000)))
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
        , head "Miner (50 fuzzed examples)"
        , Gadget.Adapter.Miner.view (Gadget.Adapter.Miner.observe gadget fuzzed)
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
        , head "Fuzzer (first example)"
        , show (List.head fuzzed)
        ]


head : String -> Html.Html msg
head txt =
    Html.h2 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.code [] [ Html.text (Debug.toString a) ]
