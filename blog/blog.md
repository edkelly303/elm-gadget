Friends, have you ever been in this situation?

Gotta write my application, gonna make some types...

```elm
type alias User =
    { name : String
    , isCool : Bool
    }
```

Mmm, need to load these users from the backend, better write a JSON decoder...

```elm
import Json.Decode as JD -- elm/json
import Json.Decode.Pipeline as JDP -- NoRedInk/elm-json-decode-pipeline

userDecoder = 
    JDP.succeed User
        |> JDP.required "name" JD.string
        |> JDP.required "isCool" JD.bool
```

Mmm, want to do some fuzz testing, better write a fuzzer...

```elm
import Fuzz as F -- elm-explorations/test

userFuzzer =
    F.constant User
        |> F.andMap F.string
        |> F.andMap F.bool
```

Mmm, want to generate some random users for my test environment, better write a
generator...

```elm
import Random as R -- elm/random
import Random.Extra as RE -- elm-community/random-extra
import Random.String as RS -- elm-community/random-extra
import Random.Char as RC -- elm-community/random-extra

userGenerator =
    R.constant User
        |> RE.andMap (RS.string 5 RC.unicode)
        |> RE.andMap RE.bool
```

I'm getting deja-vu at this point. It seems like I'm kinda doing the same thing over and over again. All these packages (if you squint a bit) seem to have the same API.

"Yes, that's an applicative"

Thank you Mr Haskell guy! But hang on a second, it goes deeper than that.

We've got a JSON decoder, but let's face it, we probably want an encoder too...

```elm
import Json.Encode as JE -- elm/json

encodeUser user =
    JE.object
        [ ( "name", JE.string user.name )
        , ( "isCool", JE.bool user.isCool )
        ]
```

But with a separate encoder and decoder, there's always a risk that things could get out of sync. What if we followed the example of that clever fellow miniBill and used a _codec_ instead?

```elm
import Codec as C -- miniBill/elm-codec

userCodec =
    C.object User
        |> C.field "name" .name C.string
        |> C.field "isCool" .isCool C.bool
        |> C.buildObject
```

This is cool because we get an encoder and a decoder in one go. And the API is only a _tiny_ bit more complicated than the applicative one. Notice that with each call of `C.field`, we need to add a getter (`.name` and `.isCool`), and that at the end we need to pipe into a final function (`C.buildObject`).

But although this is way better, it only helps us with the case of JSON encoding and decoding. We still need separate boilerplate for the fuzzer and generator. And if we wanted to add other functionality to our type (say, a function that could print it as a nicely formatted string, or a parser that can turn a string into a `User` value), that would mean yet more boilerplate.

What if we could generalise?

```elm
import Gadget -- edkelly303/elm-gadget

userGadget =
    Gadget.record User
        |> Gadget.field "name" .name Gadget.string
        |> Gadget.field "isCool" .isCool Gadget.bool
        |> Gadget.endRecord
```

Ok, so what does this `Gadget` thing do? By itself, nothing very useful. It effectively teaches Elm how a `User` value is structured by converting it into an "intermediate representation" (IR):

```elm
import Gadget.IR

Gadget.IR.fromInput { name = "Ed", isCool = False }

--> 
    Gadget.IR.RecordValue 
        [ ( "name", Gadget.IR.StringValue "Ed" )
        , ( "isCool", Gadget.IR.BoolValue False )
        ]
```