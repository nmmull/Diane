module Diane exposing (..)

import Dict exposing (Dict)

stackStr : List Int -> String
stackStr s =
  case s of
    [] -> "⊥"
    _ -> String.join " :: " (List.map String.fromInt s) ++ " :: ⊥"

-- PROGRAMS

type alias Prog = String
type alias Ident = String

type Command
  = Push Int | Trace
  | Drop | Swap | Dup | Over | Rot
  | Drop2 | Swap2 | Nip | Tuck
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | If Prog Prog | While Prog Prog
  | Fun Ident Prog | Call Ident
  | Lookup Ident | Assign Ident | Unassign Ident
  | OpenLocal | CloseLocal

commandString : Command -> String
commandString c =
    case c of
        Push i -> "(push) " ++ String.fromInt i
        Trace -> "print"
        Drop -> "drop"
        Swap -> "swap"
        Dup -> "dup"
        Over -> "over"
        Rot -> "rot"
        Drop2 -> "drop2"
        Swap2 -> "swap2"
        Nip -> "nip"
        Tuck -> "tuck"
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
        Eq -> "="
        Neq -> "<>"
        Lt -> "<"
        Lte -> "<="
        Gt -> ">"
        Gte -> ">="
        If _ _ -> "? {_} else {_}"
        While _ _ -> "while {_} do {_}"
        Fun name _ -> "def " ++ name ++ " {_}"
        Call name -> "#" ++ name
        Lookup name -> "(lookup) " ++ name
        Assign name -> "@" ++ name
        Unassign name -> "!" ++ name
        OpenLocal -> "["
        CloseLocal -> "]"

-- ERRORS

type Error
  = StackUnderflow Command
  | UnknownVariable Ident
  | InvalidCall Ident
  | InvalidLookup Ident
  | DivByZero
  | CloseGlobal

mkErrMsg : String -> String
mkErrMsg s = "ERROR: " ++ s ++ "."

errMsg : Error -> String
errMsg e =
  let
    m =
      case e of
        StackUnderflow com -> "Stack underflow on '" ++ commandString com ++ "'"
        UnknownVariable ident -> "Unknown variable '" ++ ident ++ "'"
        InvalidCall ident -> "Invalid call, '" ++ ident ++ "' is not a subroutine"
        InvalidLookup ident -> "Invalid lookup, '" ++ ident ++ "' is not an integer"
        DivByZero -> "Division by 0"
        CloseGlobal -> "No block to close"
  in mkErrMsg m

-- VALUES

type Value
  = Number Int
  | Subroutine Prog

valString : Value -> String
valString v =
  case v of
    Number n -> String.fromInt n
    Subroutine _ -> "<function>"

-- ENVIRONMENTS

type alias Bindings = Dict Ident Value

assignBindings = Dict.insert
unassignBindings = Dict.remove
lookupBindings = Dict.get
emptyBindings = Dict.empty

type Env
    = Global Bindings
    | Local Bindings Env

mapBindings f e =
    case e of
        Global bs -> Global (f bs)
        Local bs env -> Local (f bs) env

assign x v e = mapBindings (assignBindings x v) e
unassign x e = mapBindings (unassignBindings x) e
lookup x e =
    case e of
        Global bs -> lookupBindings x bs
        Local bs rest -> case lookupBindings x bs of
            Nothing -> lookup x rest
            Just v -> Just v

emptyEnv = Global emptyBindings
emptyLocal = Local emptyBindings

bindingsString : Bindings -> String
bindingsString e =
  let
    go bs =
      case bs of
        [] -> []
        (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
  in
  String.join "\n" (go (Dict.toList e))

envToList : Env -> List Bindings
envToList e =
    case e of
        Global bs -> [bs]
        Local bs rest -> bs :: envToList rest

-- CONFIGURATIONS

type alias Config =
  { stack : List Int
  , program : Prog
  , env : Env
  }

-- EVALUATION

indent : Int -> String -> String
indent k s =
    let ls = List.map (\x -> (String.repeat k " ") ++ x) (String.lines s) in
    String.join "\n" ls

evalCommand : Command -> Config -> Result Error ( Config, Maybe String )
evalCommand com ({stack, program, env} as config) =
  let mkr s e p = Ok ( { config | stack = s, env = e, program = p }, Nothing ) in
  let mk s = mkr s env program in
  let mkBool b s = mk (if b then 1 :: s else 0 :: s) in
  let mkTrace s msg = Ok ( { config | stack = s }, Just msg ) in
  let mkProg s p = mkr s env (p ++ if String.isEmpty program then "" else "\n" ++ program) in
  let mkEnv s e = mkr s e program in
  case ( com, stack ) of
    ( Push n, s ) -> mk (n :: s)
    ( Drop, _ :: s ) -> mk s
    ( Swap, x :: y :: s ) -> mk (y :: x :: s)
    ( Dup, x :: s ) -> mk (x :: x :: s)
    ( Over, x :: y :: s ) -> mk (y :: x :: y :: s)
    ( Rot, x :: y :: z :: s ) -> mk (z :: x :: y :: s)
    ( Drop2, x :: y :: s ) -> mk s
    ( Swap2, x :: y :: z :: w :: s ) -> mk (z :: w :: x :: y :: s)
    ( Nip, x :: y :: s ) -> mk (x :: s)
    ( Tuck, x :: y :: s ) -> mk (x :: y :: x :: s)
    ( Trace, x :: s ) -> mkTrace s (String.fromInt x)
    ( Add, x :: y :: s ) -> mk (x + y :: s)
    ( Sub, x :: y :: s ) -> mk (x - y :: s)
    ( Mul, x :: y :: s ) -> mk (x * y :: s)
    ( Div, x :: 0 :: s ) -> Err DivByZero
    ( Div, x :: y :: s ) -> mk (x // y :: s)
    ( Mod, x :: 0 :: s ) -> Err DivByZero
    ( Mod, x :: y :: s ) -> mk (remainderBy y x :: s)
    ( Eq, x :: y :: s ) -> mkBool (x == y) s
    ( Neq, x :: y :: s ) -> mkBool (x /= y) s
    ( Lt, x :: y :: s ) -> mkBool (x < y) s
    ( Lte, x :: y :: s ) -> mkBool (x <= y) s
    ( Gt, x :: y :: s ) -> mkBool (x > y) s
    ( Gte, x :: y :: s ) -> mkBool (x >= y) s
    ( If p1 p2, x :: s ) ->
        if x == 0
        then mkProg s p2
        else mkProg s p1
    ( While p1 p2, s ) ->
        let
            while =
                p1
                ++ " ? {\n"
                ++ indent 2 p2
                ++ "\n while {\n"
                ++ indent 4 p1
                ++ "\n } do {\n"
                ++ indent 4 p2
                ++ "\n }\n} else { }"
        in
        mkProg s while
    ( Fun name body, s ) ->
      mkEnv s (assign name (Subroutine body) env)
    ( Call id, s ) ->
        case lookup id config.env of
            Nothing -> Err (UnknownVariable id)
            Just (Subroutine body) -> mkProg s body
            Just _ -> Err (InvalidCall id)
    ( Lookup id, s ) ->
        case lookup id config.env of
            Nothing -> Err (UnknownVariable id)
            Just (Number n) -> mk (n :: s)
            Just _ -> Err (InvalidLookup id)
    ( Assign id, x :: s ) ->
      mkEnv s (assign id (Number x) env)
    ( Unassign id, s ) ->
        case lookup id config.env of
            Nothing -> Err (UnknownVariable id)
            _ -> mkEnv s (unassign id env)
    ( OpenLocal, s ) ->
        mkEnv s (emptyLocal env)
    ( CloseLocal, s ) ->
        case env of
          Global _ -> Err CloseGlobal
          Local _ e -> mkEnv s e
    _ -> Err (StackUnderflow com)

done : Config -> Bool
done c = String.isEmpty (String.trim c.program)
