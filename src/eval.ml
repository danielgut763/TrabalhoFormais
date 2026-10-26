(* ========================================================================== *)
(* Avaliador Big-Step para L2                                                 *)
(* ========================================================================== *)

open Ast

exception RunTimeError of string

type runEnv = (string * ast) list
type mem = (int * ast) list

let insert k v lst = (k, v) :: lst

let rec lookup k lst =
  match lst with
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let new_loc (sigma : mem) : int =
  let rec max_loc s acc =
    match s with
    | [] -> acc
    | (l, _) :: tl -> max_loc tl (if l > acc then l else acc)
  in
  (max_loc sigma (-1)) + 1

let rec update_mem (l : int) (v : ast) (sigma : mem) : mem =
  match sigma with
  | [] -> raise (RunTimeError ("Endereco de memoria invalido: loc(" ^ string_of_int l ^ ")"))
  | (l', v') :: tl ->
      if l = l' then (l, v) :: tl
      else (l', v') :: update_mem l v tl

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

let string_of_value (e : ast) : string =
  match e with
  | Num n  -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit   -> "()"
  | Loc l  -> "loc(" ^ string_of_int l ^ ")"
  | _      -> failwith "expressao nao e um valor"

let string_of_mem (sigma : mem) : string =
  let rec aux s =
    match s with
    | [] -> ""
    | [(l, v)] -> "loc(" ^ string_of_int l ^ ") |-> " ^ string_of_value v
    | (l, v) :: tl -> "loc(" ^ string_of_int l ^ ") |-> " ^ string_of_value v ^ ", " ^ aux tl
  in
  "{ " ^ aux (List.rev sigma) ^ " }"

let rec eval_bigstep (r : runEnv) (sigma : mem) (e : ast) : (ast * mem) =
  match e with
  | Num n  -> (Num n, sigma)
  | Bool b -> (Bool b, sigma)
  | Unit   -> (Unit, sigma)
  | Loc l  -> (Loc l, sigma)
  | Id x ->
      (match lookup x r with
       | Some v -> (v, sigma)
       | None -> raise (RunTimeError ("Identificador nao declarado: " ^ x)))
  | Not e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Bool b -> (Bool (not b), sigma1)
       | _ -> raise (RunTimeError "Operando de not nao e booleano"))
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
  | If (e1, e2, e3) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Bool true  -> eval_bigstep r sigma1 e2
       | Bool false -> eval_bigstep r sigma1 e3
       | _ -> raise (RunTimeError "Erro de execucao em if-then-else"))
  | Let (x, _, e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      let e2' = subs x v1 e2 in
      eval_bigstep (insert x v1 r) sigma1 e2'
  | New e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      let l = new_loc sigma1 in
      (Loc l, (l, v1) :: sigma1)
  | Deref e1 ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Loc l ->
           (match lookup l sigma1 with
            | Some v -> (v, sigma1)
            | None -> raise (RunTimeError ("Endereco nao encontrado na memoria: loc(" ^ string_of_int l ^ ")")))
       | _ -> raise (RunTimeError "Erro de execucao em deref (!): nao e uma localizacao"))
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
  | Seq (e1, e2) ->
      let (v1, sigma1) = eval_bigstep r sigma e1 in
      (match v1 with
       | Unit -> eval_bigstep r sigma1 e2
       | _ -> raise (RunTimeError "Primeira expressao de ';' nao avaliou para ()"))
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

let eval (e : ast) : (ast * mem) =
  eval_bigstep [] [] e
