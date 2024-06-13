module Model exposing (..)

import Diane exposing (..)
import MyParser exposing (..)

type alias Model =
    { config : Config
    , programCopy : Prog
    , savedProgram : Prog
    , history : List Config
    , trace : List String
    , going : Bool
    , dragX : DragState
    , dragY : DragState
    , hasTrace : Bool
    , adjustable : Bool
    }

type DragState
    = Static Float
    | Moving Float

ifThenElse : (Model -> Bool) -> (Model -> Model) -> (Model -> Model) -> Model -> Model
ifThenElse f yes no m = if f m then yes m else no m

updateConfig : Bool -> Config -> Model -> Model
updateConfig withHistory config model =
    let c = model.config in
    let
        go m =
            let
                newHistory =
                    if c.program == m.programCopy
                    then c :: m.history
                    else c :: { c | program = m.programCopy } :: m.history
            in { m | history = newHistory }
    in
    let
        m =
            if withHistory
            then go model
            else model
    in { m | config = config, programCopy = config.program }

maybeTrace : Maybe String -> Model -> Model
maybeTrace maybeMsg =
    case maybeMsg of
        Just msg -> trace msg
        Nothing -> identity

clearTrace m = { m | trace = [] }
clearStack m =
    let c = m.config in
    { m | config = { c | stack = [] } }
clearEnv m =
    let c = m.config in
    { m | config = { c | env = emptyEnv } }

panic : String -> Model -> Model
panic msg = maybeTrace (Just msg) >> stop

parseAndThen : (ParserOutput -> Model -> Model) -> Model -> Model
parseAndThen go model =
    case parse model.config.program of
        Ok out -> go out model
        Err _ -> panic (mkErrMsg "Parse Error") model

evalAndThen : (( Config, Maybe String ) -> Model -> Model) -> ParserOutput -> Model -> Model
evalAndThen go { command, unconsumed } ({ config } as model ) =
    case evalCommand command { config | program = unconsumed } of
        Ok out -> go out model
        Err e -> panic (errMsg e) model

popHistoryAndThen : (Config -> Model -> Model) -> Model -> Model
popHistoryAndThen go m =
    case m.history of
        [] -> m
        last :: rest -> go last { m | history = rest }

reloadProgram m = updateConfig False (initConfig m.savedProgram) m
changeProgram newProgram m =
    let c = m.config in
    { m | config = { c | program = newProgram } }

clearHistory m = { m | history = [] }
updateHistory m = updateConfig True m.config m

stop m = { m | going = False }
start m = { m | going = True }
save m = { m | savedProgram = m.config.program }
trace msg m = { m | trace = msg :: m.trace }
reset = stop >> reloadProgram >> clearHistory
undo = stop >> popHistoryAndThen (updateConfig False)

step : Bool -> Model -> Model
step withHistory =
    let
        go ( nextConfig, maybeMsg ) =
            updateConfig withHistory nextConfig
            >> maybeTrace maybeMsg
    in
    ifThenElse
        (.config >> done)
        stop
        (parseAndThen (evalAndThen go))

eval : Model -> Model
eval =
    let
        go n m =
            if m.going && n > 0
            then go (n - 1) (step False m)
            else
                if n <= 0
                then m |> stop |> trace (mkErrMsg "time out")
                else m
    in
    start >> updateHistory >> go 100000

initConfig prog =
    { stack = []
    , program = prog
    , env = emptyEnv
    }

initModel flags =
    { config = initConfig flags.program
    , programCopy = flags.program
    , going = False
    , savedProgram = flags.program
    , trace = []
    , history = []
    , dragX = Static 0.5
    , dragY = Static 0.7
    , hasTrace = flags.hasTrace
    , adjustable = flags.adjustable
    }

fracX : Model -> Float
fracX m =
    case m.dragX of
        Static f -> f
        Moving f -> f

fracY : Model -> Float
fracY m =
    case m.dragY of
        Static f -> f
        Moving f -> f
