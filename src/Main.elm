port module Main exposing (..)

import Diane exposing (..)
import MyParser exposing (parse)

import Dict exposing (Dict)

import Json.Decode as Decode
import Browser
import Html exposing (Html, div, text, textarea, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, preventDefaultOn)
import Time

type alias Model =
  { config : Config
  , going : Bool
  , savedProgram : Prog
  , history : List Config
  }

mkErrMsg : String -> String
mkErrMsg s = "ERROR: " ++ s

errMsg : Error -> String
errMsg e =
  let
    m =
      case e of
        StackUnderflow -> "Stack underflow"
        UnknownVariable -> "Unknown variable"
        InvalidCall -> "Invalid call"
        InvalidLookup -> "Invalid lookup"
  in mkErrMsg m

panic : String -> Model -> Model
panic msg m =
  let c = m.config in
  { m
  | config = { c | trace = msg :: c.trace }
  , going = False
  }

step : Bool -> Model -> Model
step updateHistory m =
  let
    go ({ config, history } as model) =
      case parse config.program of
        Ok { command, unconsumed } ->
          case evalCommand command { config | program = unconsumed } of
            Ok nextConfig ->
              { model
              | config = nextConfig
              , history =
                if updateHistory
                then config :: history
                else history
              }
            Err e -> panic (errMsg e) model
        Err _ -> panic (mkErrMsg "Parse error") model
  in
  if done m.config
  then { m | going = False }
  else go m

eval : Model -> Model
eval model =
  let
    go m =
      if m.going
      then go (step False m)
      else m
  in go { model | going = True, history = model.config :: model.history }

reset : Model -> Model
reset m =
  let c = m.config in
  { m
  | config = { c | stack = [], program = m.savedProgram, env = initEnv }
  , going = False
  }

undo : Model -> Model
undo m =
  let c = m.config in
  case m.history of
    [] -> m
    old :: rest ->
      { m
      | config = { c | stack = old.stack, env = old.env, program = old.program }
      , history = rest }

initConfig prog =
  { stack = []
  , program = prog
  , env = initEnv
  , trace = []
  }

initModel prog =
  { config = initConfig prog
  , going = False
  , savedProgram = prog
  , history = []
  }

init : String -> ( Model , Cmd Msg )
init prog = ( initModel prog , Cmd.none )

type Msg
  = Step
  | Eval
  | Undo
  | Reset
  | Start
  | Stop
  | Toggle
  | Save
  | ClearConsole
  | ClearData
  | Change String
  | Tick Time.Posix

port messageReceiver : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 100 Tick
    , messageReceiver Change
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
  let mk x = ( x, Cmd.none ) in
  case msg of
    Step -> mk (step True m)
    Eval -> mk (eval m)
    Undo -> mk (undo m)
    Reset -> mk (reset m)
    Start -> mk { m | going = True }
    Stop -> mk { m | going = False }
    Toggle -> mk { m | going = not m.going }
    Save -> mk { m | savedProgram = m.config.program }
    Change p ->
      let c = m.config in
      mk { m | config = { c | program = p }, history = [] }
    Tick _ -> if m.going then mk (step False m) else mk m
    ClearConsole ->
      let c = m.config in
      mk { m | config = { c | trace = [] } }
    ClearData ->
      let c = m.config in
      mk { m | config = { c | stack = [], env = initEnv }, history = [] }

succeededIfKey : Int -> Int -> Decode.Decoder Int
succeededIfKey n key =
  if key == n
  then Decode.succeed key
  else Decode.fail "non-key"

succeededIfTabKey : Int -> Decode.Decoder Int
succeededIfTabKey = succeededIfKey 9

shortcut : Int -> Msg -> Decode.Decoder ( Msg, Bool )
shortcut n msg =
  Decode.andThen (succeededIfKey n) keyCode
      |> Decode.map (always msg)
      |> Decode.map (\m -> ( m, True ))

tabPressed : Decode.Decoder ( Msg, Bool )
tabPressed = shortcut 9 Step

shortcuts : Decode.Decoder ( Msg, Bool )
shortcuts =
  let
    succeeded key =
      if key == 220 || key == 192 || key == 18
      then Decode.succeed key
      else Decode.fail "non-key"
  in
  let
    choose key =
      if key == 220 -- BACKSLASH
      then Step
      else if key == 18 -- OPTION
      then Eval
      else if key == 192 -- BACKTICK
      then Undo
      else Reset
  in
  Decode.andThen succeeded keyCode
    |> Decode.map choose
    |> Decode.map (\m -> ( m, True ))


window : Model -> Html Msg
window m =
  div [ id "editor-window" ]
    [ textarea
      [ id "editor"
      , placeholder "Write your program here..."
      , value m.config.program
      , disabled m.going
      , onInput Change
      ]
      []
    , button
      [ id "save-button"
      , onClick Save
      , disabled (m.savedProgram == m.config.program)
      ]
      [ text "save" ]
    ]

buttonBar: Model -> Html Msg
buttonBar m =
  div [ id "button-bar" ]
    [ div [ id "buttons" ]
      [ button
        [ onClick Step
        , disabled (done m.config)
        ]
        [ text "step" ]
      , button
        [ onClick Undo
        , disabled (List.isEmpty m.history)
        ]
        [ text "undo" ]
      , button
        [ onClick Eval
        , disabled (done m.config)
        ]
        [ text "run" ]
      , button
        [ onClick Reset
        , disabled (m.config.program == m.savedProgram)
        ] [ text "reset" ]
      -- , button
      --   [ onClick Toggle
      --   , disabled (done m.config)
      --   ]
      --   [ text (if m.going then "stop" else "play") ]
      ]
    ]

console : Model -> Html Msg
console m =
  let line s = div [] [ text s ] in
  div [ id "console-window" ]
    [ div [ id "console" ]
      (List.map line (List.reverse m.config.trace))
    , button
      [ id "clear-console"
      , disabled (List.isEmpty m.config.trace)
      , onClick ClearConsole
      ]
      [ text "clear"
      ]
    ]

stackStr : List Int -> String
stackStr s =
  case s of
    [] -> "⊥"
    _ -> String.join " :: " (List.map String.fromInt s) ++ " :: ⊥"

valString : Value -> String
valString v =
    case v of
        Number n -> String.fromInt n
        Subroutine _ -> "<function>"

envString : Env -> String
envString e =
    let go bs = case bs of
                    [] -> []
                    (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
    in
    String.join "\n" (go (Dict.toList e))

envHtmls : Env -> List (Html Msg)
envHtmls e =
    let go bs = case bs of
                    [] -> []
                    (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
    in
    List.map (\s -> div [] [ text s ]) (go (Dict.toList e))

viz : Model -> Html Msg
viz m =
  div [ id "viz-window" ]
    [ div [ id "viz" ]
      [ Html.h3 [] [ text "Stack" ]
      , div [] [ text (stackStr m.config.stack) ]
      , Html.h3 [] [ text "Environment" ]
      , Html.ul
        []
        (List.map (\x -> Html.li [] [ x ]) (envHtmls m.config.env))
      ]
    , button
      [ id "clear-data"
      , disabled (List.isEmpty m.config.stack && Dict.isEmpty m.config.env)
      , onClick ClearData
      ]
      [ text "clear"
      ]
    ]

view : Model -> Html Msg
view m =
  div
    [ id "view"
    , preventDefaultOn "keydown" shortcuts
    ]
    [ window m
    , buttonBar m
    , console m
    , viz m
  ]

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions=subscriptions
    }
