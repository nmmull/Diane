port module Update exposing (..)

import Json.Decode as D
import Browser.Events exposing (onMouseUp, onMouseMove, onMouseUp)
import Html.Events exposing (keyCode)
import Dict exposing (Dict)
import Model exposing (..)

type Msg
    = Step
    | Eval
    | Undo
    | Reset
    | Save
    | ClearConsole
    | ClearData
    | Change String
    | DragStartX
    | DragMoveX Bool Float
    | DragStopX Float
    | DragStartY
    | DragMoveY Bool Float
    | DragStopY Float

noCmd : a -> ( a, Cmd Msg )
noCmd x = ( x, Cmd.none )

update_ : Msg -> Model -> Model
update_ msg =
    case msg of
        Step -> evalStep True
        Eval -> eval
        Undo -> undo
        Reset -> reset
        Save ->
            save
            >> clearHistory
            >> trace "Program saved (and history cleared)"
        Change newProgram -> changeProgram newProgram
        ClearConsole -> clearTrace
        ClearData -> updateHistory >> clearStack >> clearEnv
        DragStartX -> \m ->
            { m | dragX = Moving (fracX m) }
        DragMoveX isDown frac -> \m ->
            { m | dragX = if isDown then Moving frac else Static (fracX m) }
        DragStopX frac -> \m ->
            { m | dragX = Static frac }
        DragStartY -> \m ->
            { m | dragY = Moving (fracY m) }
        DragMoveY isDown frac -> \m ->
            { m | dragY = if isDown then Moving frac else Static (fracY m) }
        DragStopY frac -> \m ->
            { m | dragY = Static frac }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg = update_ msg >> noCmd

port messageReceiver : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions m =
    let
        draggedX =
            case m.dragX of
                Static _ -> []
                Moving _ ->
                    [ onMouseMove (D.map2 DragMoveX decodeButtons decodeFracX)
                    , onMouseUp (D.map DragStopX decodeFracX)
                    ]
    in
    let
        draggedY =
            case m.dragY of
              Static _ -> []
              Moving _ ->
                  [ onMouseMove (D.map2 DragMoveY decodeButtons decodeFracY)
                  , onMouseUp (D.map DragStopY decodeFracY)
                  ]
    in
    Sub.batch (messageReceiver Change :: draggedX ++ draggedY)

decodeFracX : D.Decoder Float
decodeFracX =
  D.map2 (/)
    (D.field "pageX" D.float)
    (D.at ["currentTarget","defaultView","innerWidth"] D.float)

decodeFracY : D.Decoder Float
decodeFracY =
  D.map2 (/)
    (D.field "pageY" D.float)
    (D.at ["currentTarget","defaultView","innerHeight"] D.float)

decodeButtons : D.Decoder Bool
decodeButtons =
  D.field "buttons" (D.map (\buttons -> buttons == 1) D.int)

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

shortcuts : D.Decoder ( Msg, Bool )
shortcuts =
  let mk m = D.succeed ( m, True ) in
  let
    go key =
      case Dict.get key keycodes of
        Just Backslash -> mk Step
        Just Option -> mk Eval
        Just Backtick -> mk Undo
        _ -> D.fail "unknown-shortcut"
  in D.andThen go keyCode

-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg m =
--     case msg of
--         Step -> noCmd (evalStep True m)
--         Eval -> noCmd (eval m)
--         Undo -> noCmd (undo m)
--         Reset -> noCmd (reset m)
--         Save ->
--             noCmd (m |> { m | savedProgram = m.config.program, history = []} -- trace: History has been reset
--         Change p ->
--           let c = m.config in
--           noCmd { m | config = { c | program = p } }
--         Tick _ -> if m.going then noCmd (evalStep False m) else noCmd m
--         ClearConsole ->
--           noCmd { m | trace = [] }
--         ClearData ->
--           let c = m.config in
--           let newC = { c | stack = [], env = emptyEnv } in
--           noCmd { m | config = newC, history = c :: m.history }
--         DragStart ->
--           noCmd { m | dragX = Moving (toFraction m.dragX) }
--         DragMove isDown frac ->
--           noCmd { m | dragX = if isDown then Moving frac else Static (toFraction m.dragX) }
--         DragStop frac ->
--           noCmd { m | dragX = Static frac }
--         DragStartY ->
--           noCmd { m | dragY = Moving (toFraction m.dragY) }
--         DragMoveY isDown frac ->
--           noCmd { m | dragY = if isDown then Moving frac else Static (toFraction m.dragY) }
--         DragStopY frac ->
--           noCmd { m | dragY = Static frac }



{-


subscriptions : Model -> Sub Msg
subscriptions m =
  let
    dragSubs =
      case m.dragX of
        Static _ -> Sub.none
        Moving _ ->
          Sub.batch
            [ E.onMouseMove (Decode.map2 DragMove decodeButtons decodeFraction)
            , E.onMouseUp (Decode.map DragStop decodeFraction)
            ]
  in
  let
    dragSubsY =
      case m.dragY of
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

-}
