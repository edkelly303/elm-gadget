module Gadget.Adapter.Form2.Safe exposing (..)

import Gadget as G
import Gadget.Adapter.Form2 as F
import Gadget.Adapter.Form2.Control as C
import Gadget.Adapter.Form2.Internal as I
import Html as H


type Safe formMsg appMsg output
    = Safe ( G.Gadget output, List (I.Control formMsg appMsg Never) )


type RecordBuilder input output formMsg appMsg
    = RecordBuilder ( G.RecordGadgetBuilder input output, List (I.Control formMsg appMsg Never) )


type alias Send controlMsg formMsg appMsg =
    C.Send controlMsg formMsg appMsg


sendToSelf : controlMsg -> C.Send controlMsg formMsg appMsg
sendToSelf =
    C.toSelf


sendToApp : appMsg -> C.Send controlMsg formMsg appMsg
sendToApp =
    C.toApp


sendToForm : formMsg -> C.Send controlMsg formMsg appMsg
sendToForm =
    C.toForm


fromGadget : G.Gadget output -> Safe formMsg appMsg output
fromGadget gadget =
    Safe ( gadget, [] )


toGadget : Safe formMsg appMsg output -> G.Gadget output
toGadget (Safe ( gadget, _ )) =
    gadget


int : Safe formMsg appMsg Int
int =
    fromGadget G.int


float : Safe formMsg appMsg Float
float =
    fromGadget G.float


string : Safe formMsg appMsg String
string =
    fromGadget G.string


record : output -> RecordBuilder input output formMsg appMsg
record output =
    RecordBuilder ( G.record output, [] )


field :
    String
    -> (input -> field)
    -> Safe formMsg appMsg field
    -> RecordBuilder input (field -> output) formMsg appMsg
    -> RecordBuilder input output formMsg appMsg
field name getter (Safe ( g, cs )) (RecordBuilder ( builder, controls )) =
    RecordBuilder ( G.field name getter g builder, controls ++ cs )


endRecord :
    RecordBuilder output output formMsg appMsg
    -> Safe formMsg appMsg output
endRecord (RecordBuilder ( builder, controls )) =
    Safe ( G.endRecord builder, controls )


toForm :
    (F.Msg formMsg -> appMsg)
    -> Safe formMsg appMsg output
    -> F.Form formMsg appMsg output
toForm toAppMsg (Safe ( gadget, controls )) =
    List.foldl
        (\control formBuilder ->
            F.registerControl control formBuilder
        )
        (F.new toAppMsg gadget)
        controls
        |> F.end


new :
    { modelGadget : G.Gadget model
    , msgGadget : G.Gadget controlMsg
    , outputGadget : G.Gadget output
    , placeholder : output
    , init : ( model, Cmd (C.Send controlMsg formMsg appMsg) )
    , load : output -> model
    , update : controlMsg -> model -> ( model, Cmd (C.Send controlMsg formMsg appMsg) )
    , notify : formMsg -> model -> model
    , view : String -> model -> H.Html (C.Send controlMsg formMsg appMsg)
    , subscriptions : model -> Sub (C.Send controlMsg formMsg appMsg)
    , submit : model -> Result String output
    , id : String
    }
    -> Safe formMsg appMsg output
new args =
    let
        control =
            C.new args
    in
    Safe
        ( args.outputGadget
            |> F.useControl control
        , [ eraseOutputType control ]
        )


label : String -> Safe formMsg appMsg output -> Safe formMsg appMsg output
label l (Safe ( g, cs )) =
    Safe ( g |> F.label l, cs )


eraseOutputType : I.Control formMsg appMsg output -> I.Control formMsg appMsg Never
eraseOutputType (I.Control id c) =
    I.Control id c
