open Ast
open Typecheck
open Eval

let parse_string (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.token lexbuf

let interpreter (s : string) : string =
  try
    let e = parse_string s in
    let t = typecheck e in
    let (v, sigma) = eval e in
    string_of_value v ^ " : " ^ string_of_tp t ^ "   | sigma = " ^ string_of_mem sigma
  with
  | Lexer.LexError msg -> "Erro lexico - " ^ msg
  | Parser.Error       -> "Erro sintatico"
  | TypeError msg      -> "Erro de tipo - " ^ msg
  | RunTimeError msg   -> "Erro de execucao - " ^ msg

let () =
  let examples = [
    ("Soma e If", "if (10 + 5) < 20 then { 100 } else { 200 }");
    ("Let simples", "let x : int = 10 in { x + 32 }");
    ("Ref / Deref / Asg", "let x : ref int = new 5 in { x := !x + 10; !x }");
    ("Somatorio 1..5 com while",
     "let i : ref int = new 1 in { let soma : ref int = new 0 in { while !i < 6 do { soma := !soma + !i; i := !i + 1 }; !soma } }");
    ("Fatorial de 5 com while",
     "let n : ref int = new 5 in { let acc : ref int = new 1 in { while 1 < !n do { acc := !acc * !n; n := !n - 1 }; !acc } }");
  ] in
  List.iter (fun (nome, code) ->
    Printf.printf "=== %s ===\nResultado: %s\n\n" nome (interpreter code)
  ) examples
