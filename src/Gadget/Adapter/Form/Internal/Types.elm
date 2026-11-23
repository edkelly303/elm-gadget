module Gadget.Adapter.Form.Internal.Types exposing (..)

import Dict exposing (Dict)
import Gadget.IR as IR exposing (Error, Value)
import Html as H
import Set


type Send controlMsg formMsg appMsg
    = ToSelf controlMsg
    | ToForm formMsg
    | ToApp appMsg


type Control formMsg appMsg output
    = Control ( IR.Gadget output, List (InternalControl formMsg appMsg Never) )


type InternalControl formMsg appMsg output
    = InternalControl String (InternalControlData formMsg appMsg)


type alias InternalControlData formMsg appMsg =
    { init : ( Value, Cmd (Send Value formMsg appMsg) )
    , load : Value -> Result (List Error) Value
    , dummyOutputValue : Value
    , update : Value -> Value -> ( Value, Cmd (Send Value formMsg appMsg) )
    , notify : formMsg -> Value -> Value
    , view : String -> Value -> H.Html (Send Value formMsg appMsg)
    , subscriptions : Value -> Sub (Send Value formMsg appMsg)
    , submit : Path -> Value -> Result (List Error) Value
    , layout : List UI
    }


type UI
    = Label
    | Input
    | Feedback


type alias Path =
    List String


type Msg formMsg
    = ControlMsg Path Value
    | FormMsg formMsg


type alias Config formMsg appMsg output =
    { bool : InternalControlData formMsg appMsg
    , int : InternalControlData formMsg appMsg
    , float : InternalControlData formMsg appMsg
    , char : InternalControlData formMsg appMsg
    , string : InternalControlData formMsg appMsg
    , overrides : Dict String (InternalControlData formMsg appMsg)
    , viewFeedback : String -> H.Html appMsg
    , viewControl : Bool -> List (H.Html appMsg) -> List (H.Html appMsg)
    , viewTopLevel :
        { switchActiveField : Int -> appMsg
        , activeField : Int
        , fieldViews : List (H.Html appMsg)
        , result : Result (Set.Set Int) output
        }
        -> List (H.Html appMsg)
    , msgToAppMsg : Msg formMsg -> appMsg
    , outputGadget : IR.Gadget output
    }
