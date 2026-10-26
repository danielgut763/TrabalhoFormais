(* ========================================================================== *)
(* Sistema de Tipos / Inferência de Tipos para L2                             *)
(* ========================================================================== *)

open Ast

exception TypeError of string

(* Ambiente de tipos Gamma: mapeia identificadores (string) para tipos (tp) *)
type gamma = (string * tp) list

let lookup_tp (x : string) (g : gamma) : tp =
  match List.assoc_opt x g with
  | Some t -> t
  | None -> raise (TypeError ("T-VAR: Variavel '" ^ x ^ "' nao declarada no contexto Gamma"))

(* Inferencia de tipos: Gamma |- e : T *)
let rec typeof (g : gamma) (e : ast) : tp =
  match e with
  (* (T-INT) *)
  | Num _ ->
      TInt

  (* (T-BOOL) *)
  | Bool _ ->
      TBool

  (* (T-UNIT) *)
  | Unit ->
      TUnit

  (* (T-VAR) *)
  | Id x ->
      lookup_tp x g

  (* Operador unario Not *)
  | Not e1 ->
      let t1 = typeof g e1 in
      if t1 = TBool then TBool
      else raise (TypeError ("Not: esperado bool, mas encontrado " ^ string_of_tp t1))

  (* Operacoes binarias: (T-OP+), (T-OP<) e demais operadores de bop *)
  | Binop (op, e1, e2) ->
      let t1 = typeof g e1 in
      let t2 = typeof g e2 in
      (match op with
       | Sum | Sub | Mul | Div ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError (
             "T-OPArit (" ^ string_of_bop op ^ "): esperado int e int, mas encontrado " ^
             string_of_tp t1 ^ " e " ^ string_of_tp t2))
       | Lt | Gt ->
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError (
             "T-OPRel (" ^ string_of_bop op ^ "): esperado int e int, mas encontrado " ^
             string_of_tp t1 ^ " e " ^ string_of_tp t2))
       | Eq ->
           if t1 = t2 then TBool
           else raise (TypeError (
             "T-OPEq (==): tipos incompativeis " ^
             string_of_tp t1 ^ " e " ^ string_of_tp t2))
       | And | Or ->
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError (
             "T-OPLog (" ^ string_of_bop op ^ "): esperado bool e bool, mas encontrado " ^
             string_of_tp t1 ^ " e " ^ string_of_tp t2)))

  (* (T-IF) *)
  | If (e1, e2, e3) ->
      let t1 = typeof g e1 in
      let t2 = typeof g e2 in
      let t3 = typeof g e3 in
      if t1 <> TBool then
        raise (TypeError ("T-IF: condicao deve ser bool, mas tem tipo " ^ string_of_tp t1))
      else if t2 <> t3 then
        raise (TypeError (
          "T-IF: ramos then e else devem ter o mesmo tipo, mas encontrados " ^
          string_of_tp t2 ^ " e " ^ string_of_tp t3))
      else
        t2

  (* (T-LET) *)
  | Let (x, t, e1, e2) ->
      let t1 = typeof g e1 in
      if t1 <> t then
        raise (TypeError (
          "T-LET: variavel '" ^ x ^ "' anotada com tipo " ^ string_of_tp t ^
          ", mas expressao tem tipo " ^ string_of_tp t1))
      else
        typeof ((x, t) :: g) e2

  (* (T-ATR) *)
  | Asg (x, e2) ->
      let tx = lookup_tp x g in
      let t2 = typeof g e2 in
      (match tx with
       | TRef t ->
           if t = t2 then TUnit
           else raise (TypeError (
             "T-ATR: atribuicao em '" ^ x ^ "' esperava " ^ string_of_tp t ^
             ", mas recebeu " ^ string_of_tp t2))
       | _ ->
           raise (TypeError (
             "T-ATR: variavel '" ^ x ^ "' nao e uma referencia (tipo encontrado: " ^
             string_of_tp tx ^ ")")))

  (* (T-DEREF) *)
  | Deref e1 ->
      let t1 = typeof g e1 in
      (match t1 with
       | TRef t -> t
       | _ ->
           raise (TypeError (
             "T-DEREF: operador '!' exige tipo ref T, mas recebeu " ^ string_of_tp t1)))

  (* (T-NEW) *)
  | New e1 ->
      let t1 = typeof g e1 in
      TRef t1

  (* (T-WHILE) *)
  | Whi (e1, e2) ->
      let t1 = typeof g e1 in
      let t2 = typeof g e2 in
      if t1 <> TBool then
        raise (TypeError ("T-WHILE: condicao do while deve ser bool, mas encontrado " ^ string_of_tp t1))
      else if t2 <> TUnit then
        raise (TypeError ("T-WHILE: corpo do while deve ser unit, mas encontrado " ^ string_of_tp t2))
      else
        TUnit

  (* (T-SEQ) *)
  | Seq (e1, e2) ->
      let t1 = typeof g e1 in
      if t1 <> TUnit then
        raise (TypeError ("T-SEQ: primeira expressao de ';' deve ter tipo unit, mas encontrado " ^ string_of_tp t1))
      else
        typeof g e2

  (* Localizacoes de memoria sao valores internos de tempo de execucao *)
  | Loc l ->
      raise (TypeError ("Loc(" ^ string_of_int l ^ ") nao deve aparecer estaticamente no codigo fonte"))

(* Funcao principal de inferencia de tipos com contexto inicial vazio *)
let typecheck (e : ast) : tp =
  typeof [] e
