module Gadget.Adapter.Form.Control exposing
    ( Control, sandbox, element, advanced, label, customLabels
    , Router, routeToApp, routeToForm, routeToSelf
    , int, float, string
    , RecordBuilder, record, field, endRecord
    , CustomBuilder, custom, variant0, variant1, variant2, endCustom
    , fromGadget, toGadget
    )

{-|

@docs Control, sandbox, element, advanced, label, customLabels
@docs Router, routeToApp, routeToForm, routeToSelf
@docs int, float, string
@docs RecordBuilder, record, field, endRecord
@docs CustomBuilder, custom, variant0, variant1, variant2, endCustom
@docs fromGadget, toGadget

-}

import Gadget as G
import Gadget.Adapter.Form.Internal.Control as C
import Gadget.Adapter.Form.Internal.Form as F
import Gadget.Adapter.Form.Internal.Types as I
import Gadget.IR as IR
import Html as H


type alias Control formMsg appMsg output =
    I.Control formMsg appMsg output


type RecordBuilder input output formMsg appMsg
    = RecordBuilder ( G.RecordGadgetBuilder input output, List (I.InternalControl formMsg appMsg Never) )


type CustomBuilder input hasAtLeastOneVariant output formMsg appMsg
    = CustomBuilder ( G.CustomGadgetBuilder input hasAtLeastOneVariant output, List (I.InternalControl formMsg appMsg Never) )


type alias Router controlMsg formMsg appMsg =
    C.Send controlMsg formMsg appMsg


routeToSelf : controlMsg -> C.Send controlMsg formMsg appMsg
routeToSelf =
    C.toSelf


routeToApp : appMsg -> C.Send controlMsg formMsg appMsg
routeToApp =
    C.toApp


routeToForm : formMsg -> C.Send controlMsg formMsg appMsg
routeToForm =
    C.toForm


fromGadget : G.Gadget output -> Control formMsg appMsg output
fromGadget gadget =
    I.Control ( gadget, [] )


toGadget : Control formMsg appMsg output -> G.Gadget output
toGadget (I.Control ( gadget, _ )) =
    gadget


int : Control formMsg appMsg Int
int =
    fromGadget G.int


float : Control formMsg appMsg Float
float =
    fromGadget G.float


string : Control formMsg appMsg String
string =
    fromGadget G.string


record : output -> RecordBuilder input output formMsg appMsg
record output =
    RecordBuilder ( G.record output, [] )


field :
    String
    -> (input -> field)
    -> Control formMsg appMsg field
    -> RecordBuilder input (field -> output) formMsg appMsg
    -> RecordBuilder input output formMsg appMsg
field name getter (I.Control ( g, cs )) (RecordBuilder ( builder, controls )) =
    RecordBuilder ( G.field name getter g builder, controls ++ cs )


endRecord :
    RecordBuilder output output formMsg appMsg
    -> Control formMsg appMsg output
endRecord (RecordBuilder ( builder, controls )) =
    I.Control ( G.endRecord builder, controls )


custom : input -> CustomBuilder input Never output formMsg appMsg
custom match =
    CustomBuilder ( G.custom match, [] )


variant0 :
    String
    -> output
    -> CustomBuilder (IR.Value -> input) hasAtLeastOneVariant output formMsg appMsg
    -> CustomBuilder input () output formMsg appMsg
variant0 name output (CustomBuilder ( builder, prevControls )) =
    CustomBuilder
        ( G.variant0 name output builder
        , prevControls
        )


variant1 :
    String
    -> (output -> a)
    -> Control formMsg appMsg output
    -> CustomBuilder ((output -> IR.Value) -> input) hasAtLeastOneVariant a formMsg appMsg
    -> CustomBuilder input () a formMsg appMsg
variant1 name ctor (I.Control ( gadget, controls )) (CustomBuilder ( builder, prevControls )) =
    CustomBuilder
        ( G.variant1 name ctor gadget builder
        , controls ++ prevControls
        )


variant2 :
    String
    -> (output -> a -> b)
    -> Control formMsg appMsg output
    -> Control formMsg appMsg a
    -> CustomBuilder ((output -> a -> IR.Value) -> input) hasAtLeastOneVariant b formMsg appMsg
    -> CustomBuilder input () b formMsg appMsg
variant2 name ctor (I.Control ( gadget1, controls1 )) (I.Control ( gadget2, controls2 )) (CustomBuilder ( builder, prevControls )) =
    CustomBuilder
        ( G.variant2 name ctor gadget1 gadget2 builder
        , controls1 ++ controls2 ++ prevControls
        )


endCustom :
    CustomBuilder (output -> IR.Value) () output formMsg appMsg
    -> Control formMsg appMsg output
endCustom (CustomBuilder ( builder, controls )) =
    I.Control ( G.endCustom builder, controls )


advanced :
    { modelGadget : G.Gadget model
    , msgGadget : G.Gadget controlMsg
    , outputGadget : G.Gadget output
    , init : ( model, Cmd (C.Send controlMsg formMsg appMsg) )
    , load : output -> model
    , update : controlMsg -> model -> ( model, Cmd (C.Send controlMsg formMsg appMsg) )
    , notify : formMsg -> model -> model
    , view : String -> model -> H.Html (C.Send controlMsg formMsg appMsg)
    , subscriptions : model -> Sub (C.Send controlMsg formMsg appMsg)
    , submit : model -> Result String output
    , id : String
    }
    -> Control formMsg appMsg output
advanced args =
    let
        control =
            C.new args
    in
    I.Control
        ( args.outputGadget
            |> F.useControl control
        , [ eraseOutputType control ]
        )


sandbox :
    { init : model
    , load : output -> model
    , update : controlMsg -> model -> model
    , view : String -> model -> H.Html controlMsg
    , submit : model -> Result String output
    , modelGadget : IR.Gadget model
    , msgGadget : IR.Gadget controlMsg
    , outputGadget : IR.Gadget output
    , id : String
    }
    -> Control formMsg appMsg output
sandbox args =
    let
        control =
            C.sandbox args
    in
    I.Control
        ( args.outputGadget
            |> F.useControl control
        , [ eraseOutputType control ]
        )


element :
    { init : ( model, Cmd msg )
    , load : output -> model
    , update : msg -> model -> ( model, Cmd msg )
    , view : String -> model -> H.Html msg
    , subscriptions : model -> Sub msg
    , submit : model -> Result String output
    , modelGadget : G.Gadget model
    , msgGadget : G.Gadget msg
    , outputGadget : G.Gadget output
    , id : String
    }
    -> Control formMsg appMsg output
element args =
    let
        control =
            C.element args
    in
    I.Control
        ( args.outputGadget
            |> F.useControl control
        , [ eraseOutputType control ]
        )


label : String -> Control formMsg appMsg output -> Control formMsg appMsg output
label l (I.Control ( g, cs )) =
    I.Control ( g |> F.label l, cs )


customLabels : String -> List String -> Control formMsg appMsg output -> Control formMsg appMsg output
customLabels typeLabel variantLabels control =
    Debug.log "implement customLabels" control


eraseOutputType : I.InternalControl formMsg appMsg output -> I.InternalControl formMsg appMsg Never
eraseOutputType (I.InternalControl id c) =
    I.InternalControl id c
