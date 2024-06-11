module MyParser exposing (parse, Output)

import Diane exposing (..)
import Parser exposing (..)
import Set

unconsumed : Parser String
unconsumed = getChompedString (chompWhile (\_ -> True))

parseIdent : Parser Ident
parseIdent = variable
  { start = Char.isUpper
  , inner = Char.isUpper
  , reserved = Set.fromList ["TOP"]
  }

isSpace c = List.member c [' ', '\n', '\t']
notAllSpace = String.any (\c -> not (isSpace c))

clean : String -> String
clean s =
  let
    go ls =
      if not (List.isEmpty ls) && List.all (String.startsWith " ") ls
      then go (List.map (String.dropLeft 1) ls)
      else ls
  in
  String.trim
    (String.join
      "\n"
      (go (List.filter notAllSpace (String.lines s))))

parseBody : Parser Prog
parseBody =
  map
    (\s -> clean (String.slice 1 -1 s))
      (getChompedString
      (multiComment "{" "}" Nestable))

parseIf : Parser Command
parseIf = succeed If
  |. keyword "?"
  |. spaces
  |= parseBody
  |. spaces
  |. keyword "else"
  |. spaces
  |= parseBody

parseWhile : Parser Command
parseWhile = succeed While
  |. keyword "while"
  |. spaces
  |= parseBody
  |. spaces
  |. keyword "do"
  |. spaces
  |= parseBody

parseFun : Parser Command
parseFun = succeed Fun
  |. keyword "def"
  |. spaces
  |= parseIdent
  |. spaces
  |= parseBody

parsePre : String -> (Ident -> Command) -> Parser Command
parsePre symb com = succeed com
  |. symbol symb
  |. spaces
  |= parseIdent

type alias Output =
  { command : Command
  , unconsumed : Prog
  }

parseCommand : Parser Command
parseCommand = oneOf
  [ map Push int
  , succeed Drop2 |. keyword "drop2"
  , succeed Swap2 |. keyword "swap2"
  , succeed Dup   |. keyword "dup"
  , succeed Drop  |. keyword "drop"
  , succeed Swap  |. keyword "swap"
  , succeed Over  |. keyword "over"
  , succeed Rot   |. keyword "rot"
  , succeed Nip   |. keyword "nip"
  , succeed Tuck  |. keyword "tuck"
  , succeed Trace |. keyword "print"
  , succeed Add   |. keyword "+"
  , succeed Sub   |. keyword "-"
  , succeed Mul   |. keyword "*"
  , succeed Div   |. keyword "/"
  , succeed Mod   |. keyword "%"
  , succeed Eq    |. keyword "="
  , backtrackable (succeed Gte   |. keyword ">=")
  , succeed Gt    |. keyword ">"
  , backtrackable (succeed Neq   |. keyword "<>")
  , backtrackable (succeed Lte   |. keyword "<=")
  , succeed Lt    |. keyword "<"
  , parseIf, parseWhile, parseFun
  , parsePre "!" Unassign
  , parsePre "@" Assign
  , parsePre "#" Call
  , map Lookup parseIdent
  ]

parse =
  let
    go = succeed Output
      |. spaces
      |= parseCommand
      |. spaces
      |= unconsumed
  in
  Parser.run go
