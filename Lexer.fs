module Lexer
open System

[<Struct>]
type Token = 
  | TNumber of num:int 
  | TOperator of op:string 
  | TLParen | TRParen
  | TEOF

let rec tkn current str =
  seq {
    match str with
      | [] ->
        yield! Option.toList current
        yield TEOF
      | c :: rest ->
        let inline cont cur =
          tkn (Some cur) rest
        let inline next cur =
          seq {
            yield! Option.toList current
            yield! cont cur
          }
        
        if Char.IsDigit c then
          match current with
            | Some (TNumber n) ->
              yield! cont <| TNumber (n * 10 + int c - int '0')
            | _ ->
              yield! next <| TNumber (int c - int '0')
        else if c = '(' then
          yield! next <| TLParen
        else if c = ')' then
          yield! next <| TRParen
        else if Char.IsWhiteSpace c  then
          yield! tkn current rest
        else
          yield! next <| TOperator (string c)
  }

let inline tokenize (str: string) =
  str |> List.ofSeq |> tkn None |> List.ofSeq
