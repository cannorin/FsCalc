module Main
open System
open Parser
open Lexer
open Eval

[<EntryPoint>]
let main argv =
  while true do
    try
      printf "> "
      Console.ReadLine()
      |> tokenize
      |> parse
      |> fst
      |> eval
      |> printfn "= %i"
    with
      | e -> printfn "error: %s" e.Message
  0
