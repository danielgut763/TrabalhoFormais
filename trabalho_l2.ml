(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(*  TRABALHO L2 - Linguagem com Construções Imperativas                       *)
(*  - Analisador Léxico (Scanner) e Analisador Sintático (Parser)             *)
(*  - Inferência / Verificação de Tipos (Sistema de Tipos de L2)              *)
(*  - Avaliador Big-Step                                                      *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ========================================================================== *)
(* 1. DATATYPES PARA ASTs E FUNÇÕES AUXILIARES                                *)
(* ========================================================================== *)

type tp =
  | TInt
  | TBool
  | TUnit
  | TRef of tp


type bop = 
  | Or
  | And
  | Eq
  | Lt
  | Gt
  | Sum
  | Sub
  | Mul
  | Div


type ast =
  | Num of int
  | Id of string
  | Bool of bool
  | Not of ast 
  | Binop of bop * ast * ast
  | Let of string * tp * ast * ast 
  | If of ast * ast * ast
  (* construçoes imperativas *)
  | Unit
  | Deref of ast
  | New of ast
  | Asg of string * ast (* x := e *)
  | Seq of ast * ast 
  | Whi of ast * ast 
  | Loc of int


let is_value (e : ast) : bool =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> true
  | _ -> false


let string_of_value (e : ast) : string =
  match e with
  | Num n  -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit   -> "()"
  | Loc l  -> "loc(" ^ string_of_int l ^ ")"
  | _      -> failwith "expressao nao e um valor"


let rec string_of_type (t : tp) : string =
  match t with
  | TInt    -> "int"
  | TBool   -> "bool"
  | TUnit   -> "unit"
  | TRef t1 -> "ref " ^ string_of_type t1


let rec expr_str (e : ast) : string =
  match e with
  | Num n  -> string_of_int n
  | Id x   -> x
  | Bool b -> string_of_bool b
  | Not e1 -> "(not " ^ expr_str e1 ^ ")"
  | Binop (o, e1, e2) ->
      let s = (match o with
        | Or  -> "||"
        | And -> "&&"
        | Eq  -> "="
        | Lt  -> "<"
        | Gt  -> ">"
        | Sum -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/") in
      "(" ^ expr_str e1 ^ " " ^ s ^ " " ^ expr_str e2 ^ ")"
  | Let (x, t, e1, e2) ->
      "(let " ^ x ^ " : " ^ string_of_type t ^ " = " ^ expr_str e1 ^
      " in " ^ expr_str e2 ^ ")"
  | If (e1, e2, e3) ->
      "(if " ^ expr_str e1 ^ " then " ^ expr_str e2 ^
      " else " ^ expr_str e3 ^ ")"
  | Unit     -> "()"
  | Deref e1 -> "(!" ^ expr_str e1 ^ ")"
  | New e1   -> "(new " ^ expr_str e1 ^ ")"
  | Asg (x, e2) -> "(" ^ x ^ " := " ^ expr_str e2 ^ ")"
  | Seq (e1, e2) -> "(" ^ expr_str e1 ^ "; " ^ expr_str e2 ^ ")"
  | Whi (e1, e2) -> "(while " ^ expr_str e1 ^ " do " ^ expr_str e2 ^ ")"
  | Loc l    -> "loc(" ^ string_of_int l ^ ")"


(* ========================================================================== *)
(* 2. ANALISADOR LÉXICO (SCANNER)                                             *)
(* ========================================================================== *)

type token =
  | IDENT of string
  | NUM of int
  | TRUE | FALSE
  | UNIT_TOK
  | IF | THEN | ELSE
  | LET | IN
  | WHILE | DO
  | NEW | DEREF | NOT
  | TINT | TBOOL | TUNIT | TREF
  | PLUS | MINUS | TIMES | DIV
  | LT | GT | EQ
  | AND | OR
  | ASG | COLON | SEMI
  | LPAREN | RPAREN
  | LBRACE | RBRACE
  | EOF


let to_id_or_kw s =
  match s with
  | "true"  -> TRUE
  | "false" -> FALSE
  | "if"    -> IF
  | "then"  -> THEN
  | "else"  -> ELSE
  | "let"   -> LET
  | "in"    -> IN
  | "while" -> WHILE
  | "do"    -> DO
  | "new"   -> NEW
  | "not"   -> NOT
  | "and"   -> AND
  | "or"    -> OR
  | "int"   -> TINT
  | "bool"  -> TBOOL
  | "unit"  -> TUNIT
  | "ref"   -> TREF
  | _       -> IDENT s


exception Erro_Lexico of int

let is_letter c = c >= 'a' && c <= 'z'
let is_digit  c = c >= '0' && c <= '9'


(* Consome o sufixo de um identificador. *)
let rec scan_id input pos =
  if pos < String.length input &&
     (is_letter input.[pos] || is_digit input.[pos])
  then scan_id input (pos + 1)
  else pos


(* Consome o sufixo de um numero. *)
let rec scan_nro input pos =
  if pos < String.length input && is_digit input.[pos]
  then scan_nro input (pos + 1)
  else pos


(* Consome um comentario iniciado em "(*".
   Devolve a posicao logo depois de "*)".
   Comentarios aninhados e comentarios sem fechamento sao erros. *)
let rec skip_comment input inicio pos =
  let n = String.length input in
  if pos + 1 >= n then raise (Erro_Lexico inicio)
  else
    match input.[pos], input.[pos + 1] with
    | '*', ')' -> pos + 2
    | '(', '*' -> raise (Erro_Lexico pos)
    | _        -> skip_comment input inicio (pos + 1)


(* Reconhece o proximo token, ignorando espacos e comentarios. *)
let rec next_token input pos =
  let n = String.length input in
  if pos >= n then (EOF, pos)
  else
    match input.[pos] with
    | ' ' | '\t' | '\n' | '\r' ->
        next_token input (pos + 1) (* despreza caracteres "espaço em branco" *)

    | 'a'..'z' ->
        let stop = scan_id input (pos + 1) in
        let lexeme = String.sub input pos (stop - pos) in
        (to_id_or_kw lexeme, stop)

    | '0'..'9' ->
        let stop = scan_nro input (pos + 1) in
        if stop < n && is_letter input.[stop] then (* rejeita "123abc" por exemplo *)
          raise (Erro_Lexico stop)
        else
          let lexeme = String.sub input pos (stop - pos) in
          let value =
            try int_of_string lexeme
            with Failure _ -> raise (Erro_Lexico pos)
          in
          (NUM value, stop)

    | '(' when pos + 1 < n && input.[pos + 1] = '*' ->
        let next = skip_comment input pos (pos + 2) in
        next_token input next
    | '(' when pos + 1 < n && input.[pos + 1] = ')' ->
        (UNIT_TOK, pos + 2)
    | '(' -> (LPAREN, pos + 1)
    | ')' -> (RPAREN, pos + 1)

    | '{' -> (LBRACE, pos + 1)
    | '}' -> (RBRACE, pos + 1)

    | ':' when pos + 1 < n && input.[pos + 1] = '=' ->
        (ASG, pos + 2)
    | ':' -> (COLON, pos + 1)

    | ';' -> (SEMI, pos + 1)
    | '!' -> (DEREF, pos + 1)
    | '~' -> (NOT, pos + 1)

    | '&' when pos + 1 < n && input.[pos + 1] = '&' ->
        (AND, pos + 2)
    | '|' when pos + 1 < n && input.[pos + 1] = '|' ->
        (OR, pos + 2)

    | '=' when pos + 1 < n && input.[pos + 1] = '=' ->
        (EQ, pos + 2)
    | '=' -> (EQ, pos + 1)

    | '<' -> (LT, pos + 1)
    | '>' -> (GT, pos + 1)

    | '+' -> (PLUS, pos + 1)
    | '-' -> (MINUS, pos + 1)
    | '*' -> (TIMES, pos + 1)
    | '/' -> (DIV, pos + 1)

    | _ -> raise (Erro_Lexico pos)


(* Percorre a entrada produzindo tokens. *)
let rec scan_from input pos =
  let (tok, next) = next_token input pos in
  match tok with
  | EOF -> [EOF]
  | _   -> tok :: (scan_from input next)

let scan input = scan_from input 0


(* ========================================================================== *)
(* 3. ANALISADOR SINTÁTICO (PARSER RECURSIVO DESCENDENTE)                     *)
(* ========================================================================== *)

exception Erro_Sintatico of string

let consume (expected : token) (toks : token list) (msg : string) : token list =
  match toks with
  | t :: rest when t = expected -> rest
  | _ -> raise (Erro_Sintatico msg)


(* Gramática de Tipos:
   T ::= int | bool | unit | ref T | ( T ) *)
let rec parse_tp (toks : token list) : (tp * token list) =
  match toks with
  | TINT  :: rest -> (TInt, rest)
  | TBOOL :: rest -> (TBool, rest)
  | TUNIT :: rest -> (TUnit, rest)
  | TREF  :: rest ->
      let (t, rest') = parse_tp rest in
      (TRef t, rest')
  | LPAREN :: rest ->
      let (t, rest') = parse_tp rest in
      let rest'' = consume RPAREN rest' "Esperado ')' apos tipo" in
      (t, rest'')
  | _ -> raise (Erro_Sintatico "Tipo invalido (esperado int, bool, unit ou ref T)")


(* Gramática de Expressões de L2 (organizada por precedência):
   expr   ::= stmt ( ';' expr )?
   stmt   ::= IF expr THEN block ELSE block
            | LET IDENT ':' tp '=' expr IN expr
            | WHILE expr DO block
            | IDENT ':=' stmt
            | logic_or
   block  ::= '{' expr '}' | stmt
   logic_or  ::= logic_and ( '||' logic_and )*
   logic_and ::= rel ( '&&' rel )*
   rel       ::= add ( ('=' | '<' | '>') add )*
   add       ::= mul ( ('+' | '-') mul )*
   mul       ::= unary ( ('*' | '/') unary )*
   unary     ::= 'not' unary | '!' unary | 'new' unary | '-' unary | atom
   atom      ::= NUM | TRUE | FALSE | '()' | IDENT | '(' expr ')' | '{' expr '}'
*)

let rec parse_expr (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_stmt toks in
  match rest with
  | SEMI :: rest' ->
      let (e2, rest'') = parse_expr rest' in
      (Seq (e1, e2), rest'')
  | _ -> (e1, rest)

and parse_block (toks : token list) : (ast * token list) =
  match toks with
  | LBRACE :: rest ->
      let (e, rest') = parse_expr rest in
      let rest'' = consume RBRACE rest' "Esperado '}' para fechar bloco" in
      (e, rest'')
  | _ -> parse_stmt toks

and parse_stmt (toks : token list) : (ast * token list) =
  match toks with
  | IF :: rest ->
      let (e1, rest1) = parse_expr rest in
      let rest2 = consume THEN rest1 "Esperado 'then' apos condicao do if" in
      let (e2, rest3) =
        match rest2 with
        | LBRACE :: r ->
            let (eb, r') = parse_expr r in
            let r'' = consume RBRACE r' "Esperado '}' para fechar bloco then" in
            (eb, r'')
        | _ -> parse_expr rest2
      in
      let rest4 = consume ELSE rest3 "Esperado 'else' apos ramo then" in
      let (e3, rest5) = parse_block rest4 in
      (If (e1, e2, e3), rest5)

  | LET :: IDENT x :: COLON :: rest ->
      let (t, rest1) = parse_tp rest in
      let rest2 = consume EQ rest1 "Esperado '=' na declaracao let" in
      let (e1, rest3) = parse_expr rest2 in
      let rest4 = consume IN rest3 "Esperado 'in' na declaracao let" in
      let (e2, rest5) =
        match rest4 with
        | LBRACE :: r ->
            let (eb, r') = parse_expr r in
            let r'' = consume RBRACE r' "Esperado '}' ao fechar corpo do let" in
            (eb, r'')
        | _ -> parse_expr rest4
      in
      (Let (x, t, e1, e2), rest5)

  | WHILE :: rest ->
      let (e1, rest1) = parse_expr rest in
      let rest2 = consume DO rest1 "Esperado 'do' apos condicao do while" in
      let (e2, rest3) = parse_block rest2 in
      (Whi (e1, e2), rest3)

  | IDENT x :: ASG :: rest ->
      let (e2, rest1) = parse_stmt rest in
      (Asg (x, e2), rest1)

  | _ -> parse_or toks

and parse_or (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_and toks in
  let rec loop acc r =
    match r with
    | OR :: r' ->
        let (e2, r'') = parse_and r' in
        loop (Binop (Or, acc, e2)) r''
    | _ -> (acc, r)
  in
  loop e1 rest

and parse_and (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_rel toks in
  let rec loop acc r =
    match r with
    | AND :: r' ->
        let (e2, r'') = parse_rel r' in
        loop (Binop (And, acc, e2)) r''
    | _ -> (acc, r)
  in
  loop e1 rest

and parse_rel (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_add toks in
  let rec loop acc r =
    match r with
    | EQ :: r' ->
        let (e2, r'') = parse_add r' in
        loop (Binop (Eq, acc, e2)) r''
    | LT :: r' ->
        let (e2, r'') = parse_add r' in
        loop (Binop (Lt, acc, e2)) r''
    | GT :: r' ->
        let (e2, r'') = parse_add r' in
        loop (Binop (Gt, acc, e2)) r''
    | _ -> (acc, r)
  in
  loop e1 rest

and parse_add (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_mul toks in
  let rec loop acc r =
    match r with
    | PLUS :: r' ->
        let (e2, r'') = parse_mul r' in
        loop (Binop (Sum, acc, e2)) r''
    | MINUS :: r' ->
        let (e2, r'') = parse_mul r' in
        loop (Binop (Sub, acc, e2)) r''
    | _ -> (acc, r)
  in
  loop e1 rest

and parse_mul (toks : token list) : (ast * token list) =
  let (e1, rest) = parse_unary toks in
  let rec loop acc r =
    match r with
    | TIMES :: r' ->
        let (e2, r'') = parse_unary r' in
        loop (Binop (Mul, acc, e2)) r''
    | DIV :: r' ->
        let (e2, r'') = parse_unary r' in
        loop (Binop (Div, acc, e2)) r''
    | _ -> (acc, r)
  in
  loop e1 rest

and parse_unary (toks : token list) : (ast * token list) =
  match toks with
  | NOT :: rest ->
      let (e, rest') = parse_unary rest in
      (Not e, rest')
  | DEREF :: rest ->
      let (e, rest') = parse_unary rest in
      (Deref e, rest')
  | NEW :: rest ->
      let (e, rest') = parse_unary rest in
      (New e, rest')
  | MINUS :: rest ->
      let (e, rest') = parse_unary rest in
      (Binop (Sub, Num 0, e), rest')
  | _ -> parse_atom toks

and parse_atom (toks : token list) : (ast * token list) =
  match toks with
  | NUM n :: rest      -> (Num n, rest)
  | TRUE :: rest       -> (Bool true, rest)
  | FALSE :: rest      -> (Bool false, rest)
  | UNIT_TOK :: rest   -> (Unit, rest)
  | LPAREN :: RPAREN :: rest -> (Unit, rest)
  | IDENT x :: rest    -> (Id x, rest)
  | LPAREN :: rest ->
      let (e, rest') = parse_expr rest in
      let rest'' = consume RPAREN rest' "Esperado ')' apos expressao" in
      (e, rest'')
  | LBRACE :: rest ->
      let (e, rest') = parse_expr rest in
      let rest'' = consume RBRACE rest' "Esperado '}' apos expressao" in
      (e, rest'')
  | _ -> raise (Erro_Sintatico "Expressao inesperada")


(* Função principal do Parser: recebe string e devolve a AST *)
let parser (input : string) : ast =
  let toks = scan input in
  let (e, rest) = parse_expr toks in
  match rest with
  | [EOF] -> e
  | _     -> raise (Erro_Sintatico "Tokens extras apos o fim da expressao")


(* ========================================================================== *)
(* 4. ANÁLISE SEMÂNTICA / INFERÊNCIA DE TIPOS                                 *)
(* ========================================================================== *)

type tyenv = (string * tp) list

let insert k v lst = (k, v) :: lst

let rec lookup k lst =
  match lst with
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

exception TypeError of string (* erro a ser reportado para o usuário do interpretador *)


(* type_of implementa as regras do Sistema de Tipos de L2 *)
let rec type_of (g : tyenv) (e : ast) : tp =
  match e with
  (* (T-INT) *)
  | Num _ -> TInt

  (* (T-BOOL) *)
  | Bool _ -> TBool

  (* (T-UNIT) *)
  | Unit -> TUnit

  (* (T-VAR) *)
  | Id x ->
      (match lookup x g with
       | Some t -> t
       | None -> raise (TypeError ("Identificador nao declarado: " ^ x)))

  (* Operador lógico unário Not *)
  | Not e1 ->
      if type_of g e1 = TBool then TBool
      else raise (TypeError "Operando de not deve ser booleano")

  (* Operações binárias: (T-OP+), (T-OP<) e demais de bop *)
  | Binop (bop, e1, e2) ->
      let t1 = type_of g e1 in
      let t2 = type_of g e2 in
      (match bop with
       | Sum | Sub | Mul | Div ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError "Operadores aritmeticos requerem operandos inteiros")
       | Lt | Gt ->
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError "Operadores relacionais (<, >) requerem operandos inteiros")
       | Eq ->
           if t1 = t2 && (t1 = TInt || t1 = TBool || t1 = TUnit) then TBool
           else raise (TypeError "Operador de igualdade requer operandos do mesmo tipo basico")
       | And | Or ->
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError "Operadores logicos requerem operandos booleanos"))

  (* (T-IF) *)
  | If (e1, e2, e3) ->
      if type_of g e1 <> TBool then
        raise (TypeError "Condicao de if-then-else deve ser do tipo bool")
      else
        let t2 = type_of g e2 in
        let t3 = type_of g e3 in
        if t2 <> t3 then
          raise (TypeError "Expressoes do then e do else devem ser do mesmo tipo")
        else t2

  (* (T-LET) *)
  | Let (x, t1, e1, e2) ->
      let te1 = type_of g e1 in
      if te1 <> t1 then
        raise (TypeError ("Tipo da expressao em let (" ^ string_of_type te1 ^
                          ") difere da anotacao (" ^ string_of_type t1 ^ ")"))
      else
        type_of (insert x t1 g) e2

  (* (T-ATR) *)
  | Asg (x, e2) ->
      (match lookup x g with
       | Some (TRef t) ->
           let t2 = type_of g e2 in
           if t = t2 then TUnit
           else raise (TypeError ("Atribuicao em '" ^ x ^ "' esperava tipo " ^
                                  string_of_type t ^ ", mas recebeu " ^ string_of_type t2))
       | Some t ->
           raise (TypeError ("Variavel '" ^ x ^ "' nao e uma referencia (tipo: " ^
                             string_of_type t ^ ")"))
       | None ->
           raise (TypeError ("Identificador nao declarado na atribuicao: " ^ x)))

  (* (T-DEREF) *)
  | Deref e1 ->
      (match type_of g e1 with
       | TRef t -> t
       | t -> raise (TypeError ("Operador '!' requer tipo ref T, mas recebeu " ^ string_of_type t)))

  (* (T-NEW) *)
  | New e1 ->
      let t1 = type_of g e1 in
      TRef t1

  (* (T-WHILE) *)
  | Whi (e1, e2) ->
      let t1 = type_of g e1 in
      let t2 = type_of g e2 in
      if t1 <> TBool then
        raise (TypeError "Condicao do while deve ser do tipo bool")
      else if t2 <> TUnit then
        raise (TypeError "Corpo do while deve ser do tipo unit")
      else
        TUnit

  (* (T-SEQ) *)
  | Seq (e1, e2) ->
      let t1 = type_of g e1 in
      if t1 <> TUnit then
        raise (TypeError "Primeira expressao da sequencia (;) deve ser do tipo unit")
      else
        type_of g e2

  | Loc _ ->
      raise (TypeError "Localizacao de memoria (Loc) nao deve ocorrer estaticamente")


(* ========================================================================== *)
(* 5. AVALIADOR BIG-STEP                                                      *)
(* ========================================================================== *)

exception RunTimeError of string (* erro a ser reportado para o usuário do interpretador *)

(* Ambiente de variáveis em tempo de execução (associa identificadores a valores) *)
type runEnv = (string * ast) list

(* Memória sigma: mapeia endereços (int) para valores (ast) *)
type mem = (int * ast) list


(* Gera um novo endereço l não pertencente a Dom(sigma) *)
let rec new_loc (sigma : mem) : int =
  let rec max_loc s acc =
    match s with
    | [] -> acc
    | (l, _) :: tl -> max_loc tl (if l > acc then l else acc)
  in
  (max_loc sigma (-1)) + 1


(* Atualiza sigma[l |-> v] para l pertencente a Dom(sigma) *)
let rec update_mem (l : int) (v : ast) (sigma : mem) : mem =
  match sigma with
  | [] -> raise (RunTimeError ("Endereco de memoria invalido: loc(" ^ string_of_int l ^ ")"))
  | (l', v') :: tl ->
      if l = l' then (l, v) :: tl
      else (l', v') :: update_mem l v tl


(* Substituição {v/x}e (conforme regra E-LET2 da especificação) *)
let rec subs (x : string) (v : ast) (e : ast) : ast =
  match e with
  | Num n  -> Num n
  | Bool b -> Bool b
  | Unit   -> Unit
  | Loc l  -> Loc l
  | Id y   -> if x = y then v else Id y
  | Not e1 -> Not (subs x v e1)
  | Binop (op, e1, e2) -> Binop (op, subs x v e1, subs x v e2)
  | If (e1, e2, e3)    -> If (subs x v e1, subs x v e2, subs x v e3)
  | Let (y, t, e1, e2) ->
      let e1' = subs x v e1 in
      if x = y then Let (y, t, e1', e2)
      else Let (y, t, e1', subs x v e2)
  | Deref e1   -> Deref (subs x v e1)
  | New e1     -> New (subs x v e1)
  | Asg (y, e2) -> Asg (y, subs x v e2)
  | Seq (e1, e2) -> Seq (subs x v e1, subs x v e2)
  | Whi (e1, e2) -> Whi (subs x v e1, subs x v e2)


(* Formatação legível da memória sigma *)
let rec string_of_mem (sigma : mem) : string =
  let rec aux s =
    match s with
    | [] -> ""
    | [(l, v)] -> "loc(" ^ string_of_int l ^ ") |-> " ^ string_of_value v
    | (l, v) :: tl -> "loc(" ^ string_of_int l ^ ") |-> " ^ string_of_value v ^ ", " ^ aux tl
  in
  "{ " ^ aux (List.rev sigma) ^ " }"


(* Avaliador Big-Step: (e, sigma) ==> (v, sigma') *)
let rec eval_bigstep (r : runEnv) (sigma : mem) (e : ast) : (ast * mem) =
  match e with
  (* Valores já estão avaliados *)
  | Num n  -> (Num n, sigma)
  | Bool b -> (Bool b, sigma)
  | Unit   -> (Unit, sigma)
  | Loc l  -> (Loc l, sigma)

  (* Variáveis *)
  | Id x ->
      (match lookup x r with
       | Some v -> (v, sigma)
       | None -> raise (RunTimeError ("Identificador nao declarado: " ^ x)))

  (* Operador unário Not *)
  | Not e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Bool b -> (Bool (not b), sigma1)
       | _ -> raise (RunTimeError "Operando de not nao e booleano"))

  (* Operadores binários (OP1, OP2, OP+, OP<, etc.) *)
  | Binop (bop, e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      let (v2, sigma2) = eval_bigstep r sigma1 e2 in
      (match (bop, v1, v2) with
       | (Sum, Num n1, Num n2) -> (Num (n1 + n2), sigma2)
       | (Sub, Num n1, Num n2) -> (Num (n1 - n2), sigma2)
       | (Mul, Num n1, Num n2) -> (Num (n1 * n2), sigma2)
       | (Div, Num n1, Num n2) ->
           if n2 = 0 then raise (RunTimeError "Divisao por zero")
           else (Num (n1 / n2), sigma2)
       | (Lt, Num n1, Num n2)   -> (Bool (n1 < n2), sigma2)
       | (Gt, Num n1, Num n2)   -> (Bool (n1 > n2), sigma2)
       | (Eq, Num n1, Num n2)   -> (Bool (n1 = n2), sigma2)
       | (Eq, Bool b1, Bool b2) -> (Bool (b1 = b2), sigma2)
       | (Eq, Unit, Unit)       -> (Bool true, sigma2)
       | (And, Bool b1, Bool b2) -> (Bool (b1 && b2), sigma2)
       | (Or, Bool b1, Bool b2)  -> (Bool (b1 || b2), sigma2)
       | _ -> raise (RunTimeError "Erro de execucao em operacao binaria"))

  (* Condicional (IF1, IF2, IF3) *)
  | If (e1, e2, e3) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Bool true  -> eval_bigstep r sigma1 e2
       | Bool false -> eval_bigstep r sigma1 e3
       | _ -> raise (RunTimeError "Erro de execucao em if-then-else"))

  (* Declaração Let (E-LET1, E-LET2) *)
  | Let (x, _, e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      let e2' = subs x v1 e2 in
      eval_bigstep (insert x v1 r) sigma1 e2'

  (* Alocação de memória (NEW, NEW1): l nao pertence a Dom(sigma) *)
  | New e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      let l = new_loc sigma1 in
      (Loc l, (l, v1) :: sigma1)

  (* Desreferência (DEREF, DEREF1): l pertence a Dom(sigma) e sigma(l) = v *)
  | Deref e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Loc l ->
           (match lookup l sigma1 with
            | Some v -> (v, sigma1)
            | None -> raise (RunTimeError ("Endereco nao encontrado na memoria: loc(" ^ string_of_int l ^ ")")))
       | _ -> raise (RunTimeError "Erro de execucao em deref (!): nao e uma localizacao"))

  (* Atribuição (ATR1, ATR2): l := v, sigma --> (), sigma[l |-> v] *)
  | Asg (x, e2) ->
      let l =
        match lookup x r with
        | Some (Loc addr) -> addr
        | Some _ -> raise (RunTimeError ("Variavel '" ^ x ^ "' nao contem uma localizacao de memoria"))
        | None   -> raise (RunTimeError ("Identificador nao declarado na atribuicao: " ^ x))
      in
      let (v2, sigma1) = eval_bigstep r sigma e2 in
      let sigma2 = update_mem l v2 sigma1 in
      (Unit, sigma2)

  (* Sequência (SEQ, SEQ1): (); e2, sigma --> e2, sigma *)
  | Seq (e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Unit -> eval_bigstep r sigma1 e2
       | _ -> raise (RunTimeError "Primeira expressao de ';' nao avaliou para ()"))

  (* Laço While (E-WHILE):
     while e1 do e2, sigma --> if e1 then (e2; while e1 do e2) else (), sigma *)
  | Whi (e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Bool false -> (Unit, sigma1)
       | Bool true  ->
           let (v2, sigma2) = eval_bigstep r sigma1 e2 in
           (match v2 with
            | Unit -> eval_bigstep r sigma2 (Whi (e1, e2))
            | _ -> raise (RunTimeError "Corpo do while nao avaliou para ()"))
       | _ -> raise (RunTimeError "Condicao do while nao avaliou para booleano"))


(* Função auxiliar eval iniciando com ambiente e memória vazios *)
let eval (e : ast) : (ast * mem) =
  eval_bigstep [] [] e


(* ========================================================================== *)
(* 6. INTERPRETADOR PRINCIPAL                                                 *)
(* ========================================================================== *)

(* Recebe uma AST, verifica o tipo e avalia via semântica big-step *)
let interpreter_ast (e : ast) : string =
  try
    let t = type_of [] e in
    let (v, sigma) = eval e in
    (string_of_value v) ^ " : " ^ (string_of_type t) ^ "   | sigma = " ^ (string_of_mem sigma)
  with
  | TypeError msg    -> "Erro de tipo - " ^ msg
  | RunTimeError msg -> "Erro de execucao - " ^ msg


(* Recebe uma string com código L2, faz o parse, verifica o tipo e avalia *)
let interpreter (source : string) : string =
  try
    let e = parser source in
    interpreter_ast e
  with
  | Erro_Lexico pos    -> "Erro lexico na posicao " ^ string_of_int pos
  | Erro_Sintatico msg -> "Erro sintatico - " ^ msg
  | TypeError msg      -> "Erro de tipo - " ^ msg
  | RunTimeError msg   -> "Erro de execucao - " ^ msg


(* ========================================================================== *)
(* 7. EXEMPLOS E CASOS DE TESTE                                               *)
(* ========================================================================== *)

(* Teste 1: Operações aritméticas e condicional simples *)
let src1 = "if (10 + 5) < 20 then { 100 } else { 200 }"
let tst1 = parser src1
let res1 = interpreter src1

(* Teste 2: Let simples *)
let src2 = "let x : int = 10 in { x + 32 }"
let tst2 = parser src2
let res2 = interpreter src2

(* Teste 3: Alocação (new), desreferência (!) e atribuição (:=) *)
let src3 = "
  let x : ref int = new 5 in {
    x := !x + 10;
    !x
  }
"
let tst3 = parser src3
let res3 = interpreter src3

(* Teste 4: Laço while — calcula o somatório de 1 até 5 (resultado esperado: 15) *)
let src4 = "
  let i : ref int = new 1 in {
    let soma : ref int = new 0 in {
      while !i < 6 do {
        soma := !soma + !i;
        i := !i + 1
      };
      !soma
    }
  }
"
let tst4 = parser src4
let res4 = interpreter src4

(* Teste 5: Laço while — calcula fatorial de 5 (resultado esperado: 120) *)
let src5 = "
  let n : ref int = new 5 in {
    let acc : ref int = new 1 in {
      while 1 < !n do {
        acc := !acc * !n;
        n := !n - 1
      };
      !acc
    }
  }
"
let tst5 = parser src5
let res5 = interpreter src5

(* Teste 6: Referência para referência (ref (ref int)) *)
let src6 = "
  let r : ref int = new 42 in {
    let p : ref ref int = new r in {
      r := 99;
      !(!p)
    }
  }
"
let tst6 = parser src6
let res6 = interpreter src6

(* Teste 7: Erro de tipo — atribuindo bool em variável ref int *)
let src_err1 = "let x : ref int = new 0 in { x := true }"
let res_err1 = interpreter src_err1

(* Teste 8: Erro de tipo — corpo do while não tem tipo unit *)
let src_err2 = "let x : ref int = new 0 in { while !x < 5 do { !x + 1 } }"
let res_err2 = interpreter src_err2

(* Teste 9: Erro de tipo — primeiro termo de ';' não tem tipo unit *)
let src_err3 = "10; 20"
let res_err3 = interpreter src_err3
