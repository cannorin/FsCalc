module Parser
open Lexer

type Expr =
  | ENumber of int
  | EOp2 of string * Expr * Expr
  | EOp1 of string * Expr

[<Struct>]
type Associativity = ALeft | ARight | AUnary

let ops = 
  Map.ofSeq [
    ("!", 1), (5, AUnary)
    ("+", 1), (5, AUnary)
    ("^", 2), (4, ALeft)
    ("-", 1), (3, AUnary)

    ("*", 2), (2, ALeft)
    ("/", 2), (2, ALeft)
    ("%", 2), (2, ALeft)
    ("+", 2), (1, ALeft)
    ("-", 2), (1, ALeft)
  ]

let (|OperatorDefined|_|) argCount = function
  | TOperator op ->
    ops |> Map.tryFind (op, argCount)
        |> Option.map (fun (prec, assoc) -> op, prec, assoc)
        |> Option.defaultWith (fun () -> failwithf "unknown operator '%s'" op)
        |> Some
  | _ -> None

let rec nextValue = function
  | TLParen      :: rest ->
    let sexp, srest = parseWithState [] [] rest
    match srest with
      | TRParen  :: rest -> sexp, rest
      | _                -> failwith "paren mismatch"
  | TNumber num  :: rest -> ENumber num, rest
  | _                    -> failwith "syntax error"

and parseWithState lhs stack = function
  | OperatorDefined 1 (op, prec, assoc) :: rest ->
      if not <| List.isEmpty stack then
        let (_, prec', _) = stack |> List.head
        if prec' > prec then
          failwith "invalid operator"
      parseWithState lhs ((op, prec, assoc) :: stack) rest
  | tokens ->
    let inline construct a o e l =
      if a = AUnary then
        EOp1 (o, e), l
      else
        match l with
          | lhs :: l -> EOp2 (o, lhs, e), l
          | _        -> failwith "missing argument for binary operator"

    let lval, rest = nextValue tokens
    match rest with
      | OperatorDefined 2 (op, prec, assoc) :: rest ->
        let (expr, lhs), count, _ =
          List.fold (fun ((e, l), c, skip) (o, p, a) ->
            if p < prec || (a <> ALeft && p = prec) || skip then
              (e, l), c, true
            else
              construct a o e l, c + 1, skip
          ) ((lval, lhs), 0, false) stack
        parseWithState (expr :: lhs) ((op, prec, assoc) :: List.skip count stack) rest
      | _ ->
        let expr, _ =
          stack |> List.fold (fun (e, l) (o, _, a) -> construct a o e l) (lval, lhs)
        expr, rest

let inline parse tokens = parseWithState [] [] tokens
