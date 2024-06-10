port module Main exposing (..)

import MyParser exposing (parse)
import Diane exposing (..)
import Dict exposing (Dict)
import Browser
import Browser.Events as E
import Time
import Json.Decode as Decode
import Html exposing (Html, div, text, textarea, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, preventDefaultOn, onMouseDown)

type alias Model =
  { config : Config
  , going : Bool
  , savedProgram : Prog
  , history : List Config
  , dragState : DragState
  , dragStateY : DragState
  }

type DragState
  = Static Float
  | Moving Float

toFraction : DragState -> Float
toFraction s =
  case s of
    Static f -> f
    Moving f -> f

toPointerEvents : DragState -> String
toPointerEvents s =
  case s of
    Static _ -> "auto"
    Moving _ -> "none"

isMoving : Model -> Bool
isMoving m =
  case m.dragState of
    Moving _ -> True
    _ -> False

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
  in
  let out = go { model | going = True } in
  { out | history = model.config :: model.history }

reset : Model -> Model
reset m =
  let c = m.config in
  { m
  | config =
    { c
    | stack = []
    , program = m.savedProgram
    , env = emptyEnv
    }
  , going = False
  }

undo : Model -> Model
undo m =
  let c = m.config in
  case m.history of
    [] -> m
    old :: rest ->
      { m
      | config =
        { c
        | stack = old.stack
        , env = old.env
        , program = old.program
        }
      , history = rest
      }

initConfig prog =
  { stack = emptyStack
  , program = prog
  , env = emptyEnv
  , trace = []
  }

initModel prog =
  { config = initConfig prog
  , going = False
  , savedProgram = prog
  , history = []
  , dragState = Static 0.5
  , dragStateY = Static 0.7
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
  | DragStart
  | DragMove Bool Float
  | DragStop Float
  | DragStartY
  | DragMoveY Bool Float
  | DragStopY Float

port messageReceiver : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions m =
  let
    dragSubs =
      case m.dragState of
        Static _ -> Sub.none
        Moving _ ->
          Sub.batch
            [ E.onMouseMove (Decode.map2 DragMove decodeButtons decodeFraction)
            , E.onMouseUp (Decode.map DragStop decodeFraction)
            ]
  in
  let
    dragSubsY =
      case m.dragStateY of
        Static _ -> Sub.none
        Moving _ ->
          Sub.batch
            [ E.onMouseMove (Decode.map2 DragMoveY decodeButtons decodeFractionY)
            , E.onMouseUp (Decode.map DragStopY decodeFractionY)
            ]
  in
  Sub.batch
    [ Time.every 100 Tick
    , messageReceiver Change
    , dragSubs
    , dragSubsY
    ]

decodeFraction : Decode.Decoder Float
decodeFraction =
  Decode.map2 (/)
    (Decode.field "pageX" Decode.float)
    (Decode.at ["currentTarget","defaultView","innerWidth"] Decode.float)

decodeFractionY : Decode.Decoder Float
decodeFractionY =
  Decode.map2 (/)
    (Decode.field "pageY" Decode.float)
    (Decode.at ["currentTarget","defaultView","innerHeight"] Decode.float)

decodeButtons : Decode.Decoder Bool
decodeButtons =
  Decode.field "buttons" (Decode.map (\buttons -> buttons == 1) Decode.int)

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
      mk { m | config = { c | stack = [], env = emptyEnv }, history = [] }
    DragStart ->
      mk { m | dragState = Moving (toFraction m.dragState) }
    DragMove isDown frac ->
      mk { m | dragState = if isDown then Moving frac else Static (toFraction m.dragState) }
    DragStop frac ->
      mk { m | dragState = Static frac }
    DragStartY ->
      mk { m | dragStateY = Moving (toFraction m.dragStateY) }
    DragMoveY isDown frac ->
      mk { m | dragStateY = if isDown then Moving frac else Static (toFraction m.dragStateY) }
    DragStopY frac ->
      mk { m | dragStateY = Static frac }

type Shortkey
  = Backslash
  | Option
  | Backtick

keycodes : Dict Int Shortkey
keycodes =
  Dict.fromList
    [ ( 220, Backslash )
    , ( 18, Option )
    , ( 192, Backtick )
    ]

shortcuts : Decode.Decoder ( Msg, Bool )
shortcuts =
  let mk m = Decode.succeed ( m, True ) in
  let
    go key =
      case Dict.get key keycodes of
        Just Backslash -> mk Step
        Just Option -> mk Eval
        Just Backtick -> mk Undo
        _ -> Decode.fail "unknown-shortcut"
  in Decode.andThen go keyCode

window : Model -> Html Msg
window m =
  div [ id "editor-window" ]
    [ textarea
      [ id "editor"
      , placeholder "Write your program here..."
      , value m.config.program
      , disabled (m.going)
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
      ]
    ]

console : Model -> Html Msg
console m =
  let line s = div [] [ text s ] in
  div
    [ id "console-pane"
    , style "pointer-events" (toPointerEvents m.dragStateY)
    , style "user-select" (toPointerEvents m.dragStateY)
    , style "height" (String.fromFloat (100 * (1 - toFraction m.dragStateY)) ++ "%")
    ]
    [ div [ id "console-window" ]
      [ div [ id "console" ]
        (List.map line (List.reverse m.config.trace))
      ]
    ,  button
      [ id "clear-console"
      , disabled (List.isEmpty m.config.trace)
      , onClick ClearConsole
      ]
      [ text "clear" ]
    ]

envHtmls : Env -> List (Html Msg)
envHtmls e =
    let go bs = case bs of
                    [] -> []
                    (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
    in
    List.map (\s -> div [] [ text s ]) (go (Dict.toList e))

viz : Model -> Html Msg
viz m =
  div
    [ id "viz-window"
    , style "pointer-events" (toPointerEvents m.dragStateY)
    , style "user-select" (toPointerEvents m.dragStateY)
    , style "height" (String.fromFloat (100 * toFraction m.dragStateY) ++ "%")
    ]
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
      [ text "clear" ]
    ]

vsplit : Model -> Html Msg
vsplit m =
  div
    [ id "vsplit"
    , style "left" (String.fromFloat (100 * toFraction m.dragState) ++ "%")
    , onMouseDown DragStart
    ]
    [ div
      [ style "margin" "auto"
      , style "width" "1px"
      , style "height" "100%"
      , style "background-color" "black"
      ]
      []
    ]

hsplit : Model -> Html Msg
hsplit m =
  div
    [ id "right-split"
    , style "top" (String.fromFloat (100 * toFraction m.dragStateY) ++ "%")
    , onMouseDown DragStartY
    ]
    [ div
      [ style "margin-top" "5px"
      , style "margin-left" "-5px"
      , style "width" "calc(100% + 5px)"
      , style "height" "1px"
      , style "background-color" "black"
      ]
      []
    ]

view : Model -> Html Msg
view m =
  div
    [ id "view"
    , preventDefaultOn "keydown" shortcuts
    ]
    [ div
      [ id "left-pane"
      , style "pointer-events" (toPointerEvents m.dragState)
      , style "user-select" (toPointerEvents m.dragState)
      , style "width" (String.fromFloat (100 * toFraction m.dragState) ++ "%")
      ]
      [ window m
      , div [ id "left-split" ] []
      , buttonBar m
      ]
    , vsplit m
    , div
      [ id "right-pane"
      , style "pointer-events" (toPointerEvents m.dragState)
      , style "user-select" (toPointerEvents m.dragState)
      , style "width" (String.fromFloat (100 * (1.0 - toFraction m.dragState)) ++ "%")
      ]
      [ viz m
      , hsplit m
      , console m
      ]
    ]

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions=subscriptions
    }
