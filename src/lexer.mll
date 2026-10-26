{
  open Parser
  exception LexError of string
}

let digit = ['0'-'9']
let int_lit = digit+
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit | '\'')*
let whitespace = [' ' '\t' '\r']

rule token = parse
  | whitespace+   { token lexbuf }
  | '\n'          { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | "(*"          { comment 1 lexbuf }
  | "()"          { UNIT_VAL }
  | ":="          { ASG }
  | "=="          { EQEQ }
  | "&&"          { AND }
  | "||"          { OR }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '<'           { LT }
  | '>'           { GT }
  | '='           { EQ }
  | '!'           { DEREF }
  | '~'           { NOT }
  | ':'           { COLON }
  | ';'           { SEMI }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | int_lit as n  { NUM (int_of_string n) }
  | ident as id   {
      match id with
      | "true"  -> BOOL true
      | "false" -> BOOL false
      | "not"   -> NOT
      | "and"   -> AND
      | "or"    -> OR
      | "if"    -> IF
      | "then"  -> THEN
      | "else"  -> ELSE
      | "let"   -> LET
      | "in"    -> IN
      | "while" -> WHILE
      | "do"    -> DO
      | "new"   -> NEW
      | "int"   -> TINT
      | "bool"  -> TBOOL
      | "unit"  -> TUNIT
      | "ref"   -> TREF
      | _       -> ID id
    }
  | eof           { EOF }
  | _ as c        {
      let pos = Lexing.lexeme_start_p lexbuf in
      raise (LexError (Printf.sprintf "Caractere inesperado '%c' na linha %d, coluna %d"
        c pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)))
    }

and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof  { raise (LexError "Comentario nao fechado antes do fim do arquivo") }
  | _    { comment depth lexbuf }
