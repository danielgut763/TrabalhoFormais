(* ========================================================================== *)
(* Sintaxe Abstrata da Linguagem L2                                           *)
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

(* ========================================================================== *)
(* Funções Auxiliares de Formatação (Pretty-Printing)                         *)
(* ========================================================================== *)

let rec string_of_tp (t : tp) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TRef t' -> "ref " ^ string_of_tp t'

let string_of_bop (op : bop) : string =
  match op with
  | Or  -> "||"
  | And -> "&&"
  | Eq  -> "=="
  | Lt  -> "<"
  | Gt  -> ">"
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let rec string_of_ast (e : ast) : string =
  match e with
  | Num n -> string_of_int n
  | Id x -> x
  | Bool b -> string_of_bool b
  | Not e1 -> "not (" ^ string_of_ast e1 ^ ")"
  | Binop (op, e1, e2) ->
      "(" ^ string_of_ast e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_ast e2 ^ ")"
  | Let (x, t, e1, e2) ->
      "let " ^ x ^ " : " ^ string_of_tp t ^ " = " ^ string_of_ast e1 ^
      " in { " ^ string_of_ast e2 ^ " }"
  | If (e1, e2, e3) ->
      "if " ^ string_of_ast e1 ^
      " then { " ^ string_of_ast e2 ^ " }" ^
      " else { " ^ string_of_ast e3 ^ " }"
  | Unit -> "()"
  | Deref e1 -> "!" ^ string_of_ast e1
  | New e1 -> "new (" ^ string_of_ast e1 ^ ")"
  | Asg (x, e2) -> x ^ " := " ^ string_of_ast e2
  | Seq (e1, e2) -> string_of_ast e1 ^ "; " ^ string_of_ast e2
  | Whi (e1, e2) ->
      "while " ^ string_of_ast e1 ^ " do { " ^ string_of_ast e2 ^ " }"
  | Loc l -> "loc(" ^ string_of_int l ^ ")"

let is_value (e : ast) : bool =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> true
  | _ -> false
