module Eval
open Parser

let rec fact = function 0 | 1 -> 1 | n -> n * fact (n-1)

let rec eval = function
  | ENumber num -> num
  | EOp1 (op, e) ->
    let v = eval e
    match op with
      | "+" -> +v
      | "-" -> -v
      | "!" -> fact v
      | _   -> failwithf "unknown unary operator '%s'" op
  | EOp2 (op, e, f) ->
    let v, u = eval e, eval f
    match op with
      | "+" -> v + u
      | "-" -> v - u
      | "*" -> v * u
      | "/" -> v / u
      | "%" -> v % u
      | "^" -> pown v u
      | _   -> failwithf "unknown binary operator '%s'" op
