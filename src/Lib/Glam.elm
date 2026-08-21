module Lib.Glam exposing (..)


type Document
    = Line { size : Int }
    | Concat { docs : List Document }
    | Text { text : String, length : Int }
    | Nest { doc : Document, indentation : Int }
    | ForceBreak { doc : Document }
    | Break { unbroken : String, broken : String }
    | FlexBreak { unbroken : String, broken : String }
    | Group { doc : Document }


append : Document -> Document -> Document
append first second =
    case first of
        Concat { docs } ->
            Concat { docs = second :: docs }

        _ ->
            Concat { docs = [ second, first ] }


prepend : Document -> Document -> Document
prepend first second =
    case first of
        Concat { docs } ->
            Concat { docs = docs ++ [ second ] }

        _ ->
            Concat { docs = [ first, second ] }


concat : List Document -> Document
concat docs =
    Concat { docs = docs }


concatJoin : List Document -> List Document -> Document
concatJoin separators docs =
    join (concat separators) docs


empty : Document
empty =
    Concat { docs = [] }


fromString : String -> Document
fromString string =
    Text { text = string, length = String.length string }


zeroWidthString : String -> Document
zeroWidthString string =
    Text { text = string, length = 0 }


group : Document -> Document
group doc =
    Group { doc = doc }


join : Document -> List Document -> Document
join separator docs =
    let
        nonEmptyDocs =
            List.filter (\doc -> doc /= empty) docs
    in
    concat (List.intersperse separator nonEmptyDocs)


line : Document
line =
    Line { size = 1 }


lines : Int -> Document
lines size =
    Line { size = size }


space : Document
space =
    Break { unbroken = " ", broken = "" }


flexSpace : Document
flexSpace =
    FlexBreak { unbroken = " ", broken = "" }


break : String -> String -> Document
break unbroken broken =
    Break { unbroken = unbroken, broken = broken }


softBreak : Document
softBreak =
    Break { unbroken = "", broken = "" }


flexBreak : String -> String -> Document
flexBreak unbroken broken =
    FlexBreak { unbroken = unbroken, broken = broken }


forceBreak : Document -> Document
forceBreak doc =
    ForceBreak { doc = doc }


nest : Int -> Document -> Document
nest indentation doc =
    Nest { doc = doc, indentation = indentation }


nestDocs : Int -> List Document -> Document
nestDocs indentation docs =
    Nest { doc = concat docs, indentation = indentation }


toString : Int -> Document -> String
toString limit doc =
    doToString "" limit 0 [ ( 0, Unbroken, doc ) ]


type Mode
    = Broken
    | ForceBroken
    | Unbroken


fits : List ( Int, Mode, Document ) -> Int -> Int -> Bool
fits docs_ maxWidth currentWidth =
    if currentWidth > maxWidth then
        False

    else
        case docs_ of
            [] ->
                True

            ( indent, mode, doc_ ) :: rest ->
                case doc_ of
                    Line _ ->
                        True

                    ForceBreak _ ->
                        False

                    Text { length } ->
                        fits rest maxWidth (currentWidth + length)

                    Nest { doc, indentation } ->
                        (( indent + indentation, mode, doc )
                            :: rest
                        )
                            |> (\x -> fits x maxWidth currentWidth)

                    Break { unbroken } ->
                        case mode of
                            Broken ->
                                True

                            ForceBroken ->
                                True

                            Unbroken ->
                                fits rest maxWidth (currentWidth + String.length unbroken)

                    FlexBreak { unbroken } ->
                        case mode of
                            Broken ->
                                True

                            ForceBroken ->
                                True

                            Unbroken ->
                                fits rest maxWidth (currentWidth + String.length unbroken)

                    Group { doc } ->
                        fits (( indent, mode, doc ) :: rest) maxWidth currentWidth

                    Concat { docs } ->
                        docs
                            |> List.map (\doc -> ( indent, mode, doc ))
                            |> List.append rest
                            |> (\x -> fits x maxWidth currentWidth)


doToString : String -> Int -> Int -> List ( Int, Mode, Document ) -> String
doToString acc maxWidth currentWidth docs_ =
    case docs_ of
        [] ->
            acc

        ( indent, mode, doc_ ) :: rest ->
            case doc_ of
                Line { size } ->
                    doToString
                        (acc ++ String.repeat size "\n" ++ indentationToString indent)
                        maxWidth
                        indent
                        rest

                -- Flex breaks ignore the current mode and are always reevaluated
                FlexBreak { unbroken, broken } ->
                    let
                        newUnbrokenWidth =
                            currentWidth + String.length unbroken
                    in
                    if fits rest maxWidth newUnbrokenWidth then
                        doToString (acc ++ unbroken) maxWidth newUnbrokenWidth rest

                    else
                        doToString (acc ++ broken ++ "\n" ++ indentationToString indent) maxWidth indent rest

                Break { unbroken, broken } ->
                    case mode of
                        Unbroken ->
                            let
                                newWidth =
                                    currentWidth + String.length unbroken
                            in
                            doToString (acc ++ unbroken) maxWidth newWidth rest

                        Broken ->
                            doToString (acc ++ broken ++ "\n" ++ indentationToString indent) maxWidth indent rest

                        ForceBroken ->
                            doToString (acc ++ broken ++ "\n" ++ indentationToString indent) maxWidth indent rest

                ForceBreak { doc } ->
                    let
                        docs =
                            ( indent, ForceBroken, doc ) :: rest
                    in
                    doToString acc maxWidth currentWidth docs

                Concat { docs } ->
                    let
                        newDocs =
                            List.map (\doc -> ( indent, mode, doc )) docs ++ rest
                    in
                    doToString acc maxWidth currentWidth newDocs

                Group { doc } ->
                    let
                        thisFits =
                            fits [ ( indent, Unbroken, doc ) ] maxWidth currentWidth

                        newMode =
                            if thisFits then
                                Unbroken

                            else
                                Broken

                        docs =
                            ( indent, newMode, doc ) :: rest
                    in
                    doToString acc maxWidth currentWidth docs

                Nest { doc, indentation } ->
                    let
                        docs =
                            ( indent + indentation, mode, doc ) :: rest
                    in
                    doToString acc maxWidth currentWidth docs

                Text { text, length } ->
                    doToString (acc ++ text) maxWidth (currentWidth + length) rest


indentationToString : Int -> String
indentationToString size =
    String.repeat size " "
