module Gadget.Adapter.Form exposing
    ( Form, Msg, Model
    , init, load, update, notify, view, subscriptions, submit
    , withControlView, withFeedbackView, withTopLevelView
    )

{-|

@docs Form, Msg, Model
@docs init, load, update, notify, view, subscriptions, submit
@docs withControlView, withFeedbackView, withTopLevelView

-}

import Gadget.Adapter.Form.Internal.Form as F
import Gadget.IR as IR
import Html as H
import Set exposing (Set)


type alias Form formMsg appMsg output =
    F.Builder formMsg appMsg output


type alias Model =
    F.Model


type alias Msg formMsg =
    F.Msg formMsg


init : Form formMsg appMsg output -> ( Model, Cmd appMsg )
init builder =
    F.end builder
        |> .init


load : Form formMsg appMsg output -> output -> Result (List IR.Error) Model
load builder =
    F.end builder
        |> .load


update : Form formMsg appMsg output -> Msg formMsg -> Model -> ( Model, Cmd appMsg )
update builder =
    F.end builder
        |> .update


view : Form formMsg appMsg output -> Model -> H.Html appMsg
view builder =
    F.end builder
        |> .view


subscriptions : Form formMsg appMsg output -> Model -> Sub appMsg
subscriptions builder =
    F.end builder
        |> .subscriptions


submit : Form formMsg appMsg output -> Model -> Result (List IR.Error) output
submit builder =
    F.end builder
        |> .submit


notify : Form formMsg appMsg output -> formMsg -> Model -> Model
notify builder =
    F.end builder
        |> .notify


withControlView : (Bool -> List (H.Html appMsg) -> List (H.Html appMsg)) -> Form formMsg appMsg output -> Form formMsg appMsg output
withControlView =
    F.withControlView


withFeedbackView : (String -> H.Html appMsg) -> Form formMsg appMsg output -> Form formMsg appMsg output
withFeedbackView =
    F.withFeedbackView


withTopLevelView :
    ({ switchActiveField : Int -> appMsg
     , activeField : Int
     , fieldViews : List (H.Html appMsg)
     , result : Result (Set Int) output
     }
     -> List (H.Html appMsg)
    )
    -> Form formMsg appMsg output
    -> Form formMsg appMsg output
withTopLevelView =
    F.withTopLevelView
