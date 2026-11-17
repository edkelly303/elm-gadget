module Gadget.Adapter.Form2.Safe exposing (..)

import Gadget as G
import Gadget.Adapter.Form2 as F
import Gadget.Adapter.Form2.Control as C
import Gadget.Adapter.Form2.Internal as I


unwrap : I.Control formMsg appMsg output -> ( String, I.InternalControl formMsg appMsg )
unwrap (I.Control id c) =
    ( id, c )


int =
    ( G.int, [ unwrap C.int ] )


float =
    ( G.float, [ unwrap C.float ] )


record output =
    ( G.record output, [] )


field name getter ( g, cs ) ( builder, controls ) =
    ( G.field name getter g builder, controls ++ cs )


endRecord ( builder, controls ) =
    ( G.endRecord builder, controls )


toForm toAppMsg ( gadget, controls ) =
    List.foldl (\( id, c ) formBuilder -> formBuilder |> F.useControl (I.Control id c)) (F.new toAppMsg gadget) controls
