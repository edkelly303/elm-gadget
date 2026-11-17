module Gadget.Adapter.Form2.Safe exposing (..)

import Gadget as G
import Gadget.Adapter.Form2 as F
import Gadget.Adapter.Form2.Control as C
import Gadget.Adapter.Form2.Internal as I


type alias Safe formMsg appMsg output =
    ( G.Gadget output, List (I.Control formMsg appMsg ()) )


labelled gadget label =
    ( F.label label gadget, [] )


int =
    labelled G.int


float =
    labelled G.float


string =
    labelled G.string


record output =
    ( G.record output, [] )


field name getter ( g, cs ) ( builder, controls ) =
    ( G.field name getter g builder, controls ++ cs )


endRecord ( builder, controls ) =
    ( G.endRecord builder, controls )


toForm toAppMsg ( gadget, controls ) =
    List.foldl
        (\control formBuilder ->
            F.registerControl control formBuilder
        )
        (F.new toAppMsg gadget)
        controls
        |> F.end


new :
    { modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , placeholder : output
    , init : ( model, Cmd (Send controlMsg formMsg appMsg) )
    , load : output -> model
    , update : controlMsg -> model -> ( model, Cmd (Send controlMsg formMsg appMsg) )
    , notify : formMsg -> model -> model
    , view : String -> model -> H.Html (Send controlMsg formMsg appMsg)
    , subscriptions : model -> Sub (Send controlMsg formMsg appMsg)
    , submit : model -> Result String output
    , id : String
    }
    ->
        (String
         -> Safe formMsg appMsg output
        )
new builder =
    let
        control =
            C.new builder
    in
    \label ->
        ( builder.outputGadget
            |> F.label label
            |> F.useControl control
        , [ control |> forget ]
        )


forget : I.Control formMsg appMsg output -> I.Control formMsg appMsg ()
forget (I.Control id c) =
    I.Control id c
