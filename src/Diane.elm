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

-- ERRORS

type Error
  = StackUnderflow
  | UnknownVariable
  | InvalidCall
  | InvalidLookup
  | DivByZero

mkErrMsg : String -> String
mkErrMsg s = "ERROR: " ++ s ++ "."

errMsg : Error -> String
errMsg e =
  let
    m =
      case e of
        StackUnderflow -> "Stack underflow"
        UnknownVariable -> "Unknown variable"
        InvalidCall -> "Invalid call"
        InvalidLookup -> "Invalid lookup"
        DivByZero -> "Division by 0"
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

type alias Env = Dict Ident Value

assign = Dict.insert
unassign = Dict.remove
lookup = Dict.get
emptyEnv = Dict.empty

envString : Env -> String
envString e =
  let
    go bs =
      case bs of
        [] -> []
        (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
  in
  String.join "\n" (go (Dict.toList e))

-- CONFIGURATIONS

type alias Config =
  { stack : List Int
  , program : Prog
  , env : Env
  }

-- EVALUATION

evalCommand : Command -> Config -> Result Error ( Config, Maybe String )
evalCommand com ({stack, program, env} as config) =
  let mkr s e p = Ok ( { config | stack = s, env = e, program = p }, Nothing ) in
  let mk s = mkr s env program in
  let mkBool b s = mk (if b then 1 :: s else 0 :: s) in
  let mkTrace s msg = Ok ( { config | stack = s }, Just msg ) in
  let mkProg s p = mkr s env (p ++ "\n" ++ program) in
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
          ++ " ? { "
          ++ p2
          ++ " while { "
          ++ p1
          ++ " } do { "
          ++ p2
          ++ " } } else { }"
      in
      mkProg s while
    ( Fun name body, s ) ->
      mkEnv s (assign name (Subroutine body) env)
    ( Call id, s ) ->
      case lookup id config.env of
        Nothing -> Err UnknownVariable
        Just (Subroutine body) -> mkProg s body
        Just _ -> Err InvalidCall
    ( Lookup id, s ) ->
      case lookup id config.env of
        Nothing -> Err UnknownVariable
        Just (Number n) -> mk (n :: s)
        Just _ -> Err InvalidLookup
    ( Assign id, x :: s ) ->
      mkEnv s (assign id (Number x) env)
    ( Unassign id, s ) ->
      mkEnv s (unassign id env)
    _ -> Err StackUnderflow

done : Config -> Bool
done c = String.isEmpty c.program
