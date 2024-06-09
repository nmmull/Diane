module Diane exposing (..)

import Dict exposing (Dict)

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

type Error
  = StackUnderflow
  | UnknownVariable
  | InvalidCall
  | InvalidLookup

type Value
  = Number Int
  | Subroutine Prog

type alias Env = Dict Ident Value

assign = Dict.insert
unassign = Dict.remove
lookup = Dict.get
initEnv = Dict.empty

type alias Config =
  { stack : List Int
  , program : Prog
  , env : Env
  , trace : List String
  }

evalCommand : Command -> Config -> Result Error Config
evalCommand com ({stack, program, env, trace} as config) =
  let mk s = Ok { config | stack = s } in
  let mkBool b s = mk (if b then 1 :: s else 0 :: s) in
  let mkTrace s msg = Ok { config | stack = s, trace = msg :: trace } in
  let mkProg s p = Ok { config | stack = s, program = p ++ "\n" ++ program } in
  let mkEnv s e = Ok { config | env = e, stack = s } in
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
    ( Div, x :: y :: s ) -> mk (x // y :: s)
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
