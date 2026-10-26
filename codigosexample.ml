(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(* Linguagem de tipos *)

type tipo = 
    TyInt 
  | TyBool
  | TyFn   of tipo * tipo
  | TyPair of tipo * tipo
  | TyList of tipo          
  | TyVar  of int   (* variáveis de tipo -- números *) 

            
(* para impressao legível de tipos  *)

let rec tipo_str (tp:tipo) : string =
  match tp with
    TyInt           -> "int"
  |  TyBool          -> "bool"       
  |  TyFn   (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "->" ^ (tipo_str t2) ^ ")"
  |  TyPair (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "*" ^ (tipo_str t2) ^ ")" 
  |  TyVar  x        -> (string_of_int x)
  |  TyList t1       -> "(" ^ (tipo_str t1) ^ " list)"

            

  
type bop = Sum | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq

          
type ident = string

(* Expressões de L1  - sem anotação de tipo  *)
                                                
type expr  = 
    Num of int  
  | Var of ident 
  | True
  | False
  | Binop of bop * expr * expr
  | Pair of expr * expr 
  | Fst of expr
  | Snd of expr 
  | If of expr * expr * expr
  | Fn of ident * expr                    (* sem anotação *)
  | App of expr * expr
  | Let of ident * expr * expr            (* sem anotação *)
  | LetRec of ident * ident * expr * expr (* sem anotação *)
  | Nil                                   (* sem anotação *)
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | Raise
  | Try of expr * expr



(* para impressão legível de expressões *)
     
let rec expr_str (e:expr) : string  =
  match e with
    Var x -> x            
  | Num n -> string_of_int n;
  | True  -> "true"
  | False -> "false"
  | Binop (o,e1,e2) ->  
      let s = (match o with
            Sum  -> "+"
          | Sub  -> "-"
          | Mult -> "*"
          | Div -> "/"
          | Leq  -> "<="
          | Geq  -> ">="
          | Eq   -> "="
          | Lt   -> "<"
          | Gt   -> ">") in
      "( " ^ (expr_str e1) ^ " " ^ s ^ " " ^ (expr_str e2) ^ " )"
  | Pair (e1,e2) ->  "(" ^ (expr_str e1) ^ "," ^ (expr_str e2) ^ ")"  
  | Fst e1 -> "fst " ^ (expr_str e1) 
  | Snd e1 -> "snd " ^ (expr_str e1) 
  | If (e1,e2,e3) -> "(if " ^ (expr_str e1) ^ " then "
                     ^ (expr_str e2) ^ " else "
                     ^ (expr_str e3) ^ " )" 
  | Fn (x,e1) -> "(fn " ^ x ^ " => " ^ (expr_str e1) ^ " )"
  | App (e1,e2) -> "(" ^ (expr_str e1) ^ " " ^ (expr_str e2) ^ ")"
  | Let (x,e1,e2) -> "(let " ^ x ^ "=" ^ (expr_str e1) ^ "\nin " ^ (expr_str e2) ^ " )"
  | LetRec (f,x,e1,e2) -> "(let rec" ^ f ^ "= fn " ^ x ^ " => " ^ (expr_str e1) ^ "\nin " ^ (expr_str e2) ^ " )"
  | Nil -> "[]"
  | Cons (e1,e2) -> (expr_str e1) ^ "::" ^ (expr_str e2)
  | Head e1 -> "hd " ^ (expr_str e1)
  | Tail e1 -> "tl " ^ (expr_str e1)
  | Raise   -> "raise"
  | Try (e1,e2) -> "try " ^ (expr_str e1) ^ " with " ^ (expr_str e2)




         
(* ambientes *)
            
type tyenv = (ident * tipo) list 
    
let empty_amb : tyenv = []
    
let rec lookup g x : tipo option = 
  match g with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some t else lookup tl x
  
let update (g: tyenv) (x:ident) (t:tipo) : tyenv = 
  (x,t) :: g

  
               
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)
                 

(* imprime restrições *)

let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (t1,t2)::c' -> 
      print_string (tipo_str t1);
      print_string " = ";
      print_string (tipo_str t2);
      print_string "\n";
      print_constr c'

                 

(* código para geração de novas variáveis de tipo *)
                 
let lastvar : int ref = ref 0

let newvar (u:unit) : int =
  let x:int = !lastvar
  in lastvar := (x+1) ; x


     
(*  coleta de restrições *)

exception CollectFail of string

       
let rec collect (g:tyenv) (e:expr) : (constraints * tipo)  =

  match e with 
    Var x   ->
      (match lookup g x with
         None    -> raise (CollectFail x)
       | Some tp -> ([],tp))                    

  | Num n -> ([],TyInt)

  | True  -> ([],TyBool)

  | False -> ([],TyBool)

  | Binop (o,e1,e2) ->  
      if List.mem o [Sum;Sub;Mult;Div]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyInt)
      else   
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyBool)    

  | Pair (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2, TyPair(tp1,tp2))    
        
  | Fst e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in
      (c1@[(tp1,TyPair(TyVar tA, TyVar tB))], TyVar tA)

  | Snd e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in         
      (c1@[(tp1,TyPair(TyVar tA,TyVar tB))], TyVar tB)         

  | If (e1,e2,e3) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      let (c3,tp3) = collect g e3 in         
      (c1@c2@c3@[(tp1,TyBool);(tp2,tp3)], tp2)

  | Fn (x,e1) ->
      let tA       = newvar()        in
      let g'       = (x,TyVar tA)::g in
      let (c1,tp1) = collect g' e1   in
      (c1, TyFn(TyVar tA,tp1))
         
  | App (e1,e2) -> 
      let (c1,tp1) = collect  g e1  in
      let (c2,tp2) = collect  g e2  in
      let tX       = newvar()       in 
      (c1@c2@[(tp1,TyFn(tp2, TyVar tX))], TyVar tX) 
  
  | Let (x,e1,e2) ->
      let (c1,tp1) = collect  g e1   in 
      let g'       = (x,tp1)::g       in
      let (c2,tp2) = collect  g' e2  in
      (c1@c2, tp2)

         
  | LetRec (f,x,e1,e2) ->
      let tX = newvar() in
      let tY = newvar() in
      let g2 = (f,TyVar tX)::g in
      let g1 = (x,TyVar tY)::g2               in
      let (c1,tp1) = collect g1 e1            in
      let (c2,tp2) = collect g2 e2            in
      (c1@c2@[(TyVar tX,TyFn(TyVar tY,tp1))],   tp2)
     
  | Nil ->
      let tA = newvar() in
      ([], TyList (TyVar tA))
     
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)
             
  | Head e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyVar tA)
         
  | Tail e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], tp1)

  | Raise ->
      let tA = newvar() in
      ([],TyVar tA)
          
  | Try (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp1,tp2)],tp1)      




(* substituições *)
                      
type subst = (int*tipo) list


(* para imprimir substituições  *)
                
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::s' -> 
      print_int a;
      print_string " |-> ";
      print_string (tipo_str b);
      print_string "\n";
      print_subst s'


           
(* aplicação de substituição a tipo *)
           
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  |  TyBool          -> TyBool       
  |  TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  |  TyPair (t1,t2)  -> TyPair (appsubs s t1, appsubs s t2)
  |  TyVar  x        -> (match lookup s x with
        None     -> TyVar x
      | Some tp' -> tp')
  |  TyList t1       -> TyList (appsubs s t1)                      

                
(* composição de substituições: s1 o s2  *)
let rec compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,tp) -> (x, appsubs s1 tp))    s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,y) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
                                                              
(* s [ (t11,t12), ....(tn1,tn2]) = [(s t11,s t12), ....(s tn1, s tn2] *) 

let rec appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
                 
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
    TyInt           -> false
  |  TyBool          -> false       
  |  TyFn   (tp1,tp2)  -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyPair (tp1,tp2)  -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyVar  x        -> v=x
  |  TyList tp1       -> var_in_tipo v tp1                      


  
(* unificação *)
              
exception UnifyFail of (tipo*tipo)
           
let rec unify (c:constraints) : subst =
  match c with
    [] -> []
  | (TyInt,    TyInt )   :: c' -> unify c'
  | (TyBool,   TyBool)   :: c' -> unify c'
  | (TyVar x1, TyVar x2) :: c' when x1=x2 -> unify c'
                             
  | (TyFn(tp1,tp2),  TyFn(tp3,tp4)  ) :: c' -> unify ((tp1,tp3)::(tp2,tp4)::c')
  | (TyPair(tp1,tp2),TyPair(tp3,tp4)) :: c' -> unify ((tp1,tp3)::(tp2,tp4)::c') 
  | (TyList tp1,     TyList tp2)      :: c' -> unify ((tp1,tp2)::c')
  
  | (TyVar x, tp):: c' 
  | (tp,TyVar x) :: c' -> 
      if var_in_tipo x tp
      then raise (UnifyFail(TyVar x, tp)) (*  Ex:  X = X list   ou  X list = X   *)
      else compose
          (unify (appconstr [(x,tp)] c')) (* unify c' modificada pela subs X |--> tp *)
          [(x,tp)]  (* gera substituição    X |--> tp  *)

  | (tp1,tp2)::c' -> raise (UnifyFail(tp1,tp2))


(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
       
let type_infer (e:expr) : unit =
  lastvar := 0;  (* para resetar nome de var de tipo cada chamada de type_infer *) 
  print_string "\nexpr:\n";
  print_string (expr_str e);
  print_string "\n\n"; 
  try
    let (c,tp) = collect [] e  in
    (print_string "\nEquações coletadas:\n";
     print_constr c;
     print_string "Tipo inferido: ";    
     print_string ((tipo_str tp) ^ "\n\n"));
    let s      = unify c       in
    let tf     = appsubs s tp  in 
    print_string "\n\nSubstituição:\n";
    print_subst s;
    print_string "Tipo inferido (após subs): ";
    print_string ((tipo_str tf) ^ "\n\n"); 
  with 
  | CollectFail x   -> print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n"
                     
  | UnifyFail (tp1,tp2) -> print_string "Erro: impossível unificar os tipos\n* ";
      print_string (tipo_str tp1);
      print_string "\n* ";
      print_string (tipo_str tp2);
      print_string "\n\n"
                           

                   
(*********   EXEMPLOS DE CÓDIGO  ***********)
            
                                                  
(*   exemplo de código 1

   let rec pow = 
        fn x => 
          fn y => 
            if y = 0 
              then 1 
              else x * (pow x (y-1))  
   in 
       pow 3 4 
         
  type ifer deve retorna int 
*)

                

(*   (y-1)              *)     
let ys1 = Binop(Sub, Var "y", Num 1)  

(*   pow x (y-1)        *)        
let powapp  =   App(App(Var "pow", Var "x"), ys1)   

(*   x*(pow x (y-1))    *)           
let xp =   Binop(Mult, Var "x", powapp)
   

(*   if y=0 then 1 else x*(pow x (y-1))    *)
let ebdy =  Fn("y", If(Binop(Eq, Var "y", Num 0) , Num 1, xp)) 


(*  let rec pow = fn x => ... in (pow 3) 4 *)
                          
let ex1 = 
  LetRec("pow", "x", ebdy, App(App(Var "pow", Num 3),Num 4)) 



(*   exemplo de código 2

  if raise 
    then (1 :: nil)
    else nil

  deve ter tipo "int list"

*)

let ex2 = If(Raise, Cons(Num 1,Nil), Nil)

  
(*   exemplo de código 3

  if (tl raise) 
    then (true :: nil)
    else raise

  deve falhar pois "tail" sempre retorna um tipo de lista

*)

let ex3 = If(Tail(Raise), Cons(Num 1,Nil), Nil)


(* exemplo de código 4


   let id = fn x => x
   in if (id true) then (id 0) else 1

 deve falhar pois o tipo id não consegue ser unificado
 ao mesmo tempo com bool->bool e int->int

*)
        

let ex4 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), App(Var "id",Num 0), Num 1))

        
(*   exemplo de código 5

  let double = fn f => fn x => f (f x) 
  in  if true
        then double (fn y => y) 3
        else double (fn y => false) true
 
  deve falhar pois double não consegue unificar simultaneamente
  com (int->int)->int->int e (bool->bool)->bool->bool

*)

let doublefn = Fn("f",Fn("x",App(Var "f",App(Var "f",Var "x"))))

let doubleif = If(
    True,
    App(App(Var "double",Fn("y",Var "y")),Num 3),
    App(App(Var "double",Fn("y",False)),True)
  )
        
let ex5 = Let("double",doublefn,doubleif)



(* exemplo de código 6

   let id = fn x => x
   in if (id true) then id else id

deve ser do tipo (bool -> bool)

*)
        

let ex6 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), Var "id", Var "id"))
    

(* exemplo de código 7

    if raise then nil else nil

deve ser do tipo X list

*)

let ex7 = If (Raise, Nil, Nil)
    
    
let ex8 = Let("idnil", Nil, Var "idnil")



(* Linguagem de tipos *)

type tipo = 
    TyInt 
  | TyBool
  | TyFn   of tipo * tipo
  | TyPair of tipo * tipo
  | TyList of tipo          
  | TyVar  of int   (* variáveis de tipo -- números *) 


(* free type variables em um tipo *)
            
let rec ftv (tp:tipo) : int list =
  match tp with
    TyInt         -> []
  | TyBool        -> []
  | TyFn(t1,t2)   -> (ftv t1) @ (ftv t2)
  | TyPair(t1,t2) -> (ftv t1) @ (ftv t2)
  | TyList(t1)    -> (ftv t1)
  | TyVar(n)      -> [n] 


                   
(* impressao legível de tipos  *)

let rec tipo_str (tp:tipo) : string =
  match tp with
    TyInt           -> "int"
  |  TyBool          -> "bool"       
  |  TyFn   (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "->" ^ (tipo_str t2) ^ ")"
  |  TyPair (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "*" ^ (tipo_str t2) ^ ")"
                      
  |  TyVar  x        -> (string_of_int x)
  |  TyList t1       -> "(" ^ (tipo_str t1) ^ " list)"

            
type ident = string
  
type op = Sum | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq


(* Linguagem de termos *)
                                                
type expr  = 
    Num of int  
  | Var of ident 
  | True
  | False
  | Binop of op * expr * expr
  | Pair of expr * expr 
  | Fst of expr
  | Snd of expr 
  | If of expr * expr * expr
  | Fn of ident * expr                    (* sem anotação *)
  | App of expr * expr
  | Let of ident * expr * expr            (* sem anotação *)
  | LetRec of ident * ident * expr * expr (* sem anotação *)
  | Nil                                   (* sem anotação *)
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | IsEmpty of expr        
  | Raise
  | Try of expr * expr



(* impressão legível de termo *)
     
let rec termo_str (e:expr) : string  =
  match e with
    Var x -> x            
  | Num n -> string_of_int n;
  | True  -> "true"
  | False -> "false"
  | Binop (o,e1,e2) ->  
      let s = (match o with
            Sum  -> "+"
          | Sub  -> "-"
          | Mult -> "*"
          | Div -> "/"
          | Leq  -> "<="
          | Geq  -> ">="
          | Eq   -> "="
          | Lt   -> "<"
          | Gt   -> ">") in
      "( " ^ (termo_str e1) ^ " " ^ s ^ " " ^ (termo_str e2) ^ " )"
  | Pair (e1,e2) ->  "(" ^ (termo_str e1) ^ "," ^ (termo_str e2) ^ ")"  
  | Fst e1 -> "fst " ^ (termo_str e1) 
  | Snd e1 -> "snd " ^ (termo_str e1) 
  | If (e1,e2,e3) -> "(if " ^ (termo_str e1) ^ " then "
                     ^ (termo_str e2) ^ " else "
                     ^ (termo_str e3) ^ " )" 
  | Fn (x,e1) -> "(fn " ^ x ^ " => " ^ (termo_str e1) ^ " )"
  | App (e1,e2) -> "(" ^ (termo_str e1) ^ " " ^ (termo_str e2) ^ ")"
  | Let (x,e1,e2) -> "(let " ^ x ^ "=" ^ (termo_str e1) ^ "\nin "
                     ^ (termo_str e2) ^ " )"
  | LetRec (f,x,e1,e2) -> "(let rec " ^ f ^ "= fn " ^ x ^ " => "
                          ^ (termo_str e1) ^ "\nin " ^ (termo_str e2) ^ " )"
  | Nil -> "[]"
  | Cons (e1,e2) -> (termo_str e1) ^ "::" ^ (termo_str e2)
  | Head e1 -> "hd " ^ (termo_str e1)
  | Tail e1 -> "tl " ^ (termo_str e1)
  | IsEmpty e1 -> "isempty " ^ (termo_str e1)           
  | Raise   -> "raise"
  | Try (e1,e2) -> "try " ^ (termo_str e1) ^ " with " ^ (termo_str e2)







         
(* ambientes - modificados para polimorfismo *)
            
type amb = (ident * (int list) * tipo) list 
    
let empty_amb : amb = [] 
    
let rec lookup amb x : (int list * tipo) option = 
  match amb with
    [] -> None
  | (y,v,t) :: tl -> if (y=x) then Some (v,t) else lookup tl x
                   
let rec update (amb: amb) (x:ident) (v:int list) (t:tipo) : amb = 
  (x,v,t) :: amb


(* remove elementos identicos *)
let nub l =
  let ucons h t = if List.mem h t then t else h::t in
  List.fold_right ucons l []
 

(* calcula l1 - l2 (como sets) *)
let diff (l1:'a list) (l2:'a list) : 'a list =
  List.filter (fun a -> not (List.mem a l2)) l1

  
(* calcula todas as variáveis de tipo livres 
   do ambiente *)           
let rec ftv_amb' (g:amb) : int list =
  match g with
    []           -> []
  | (x,v,tp)::t  -> (diff (ftv tp) v)
                    @(ftv_amb' t)
let ftv_amb (g:amb) : int list = nub (ftv_amb' g)



  
               
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)
                 

(* imprime restrições *)

let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (a,b)::t -> print_string (tipo_str a);
      print_string " = ";
      print_string (tipo_str b);
      print_string "\n";
      print_constr t

                 

(* código para geração de novas variáveis de tipo *)
                 
let lastvar = ref 0

let newvar (u:unit) : int =
  let x = !lastvar
  in lastvar := (x+1) ; x




(* substituições *)
                      
type subst = (int*tipo) list


(* consulta a substituição *)
           
let rec lookupsubs amb x : (tipo) option = 
  match amb with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some (t) else lookupsubs tl x


(* imprime substituições  *)
                
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::t -> print_int a;
      print_string " |-> ";
      print_string (tipo_str b);
      print_string "\n";
      print_subst t


           
(* aplicação de substituição a tipo *)
           
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  |  TyBool          -> TyBool       
  |  TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  |  TyPair (t1,t2)  -> TyPair (appsubs s t1, appsubs s t2)
  |  TyVar  x        -> (match lookupsubs s x with
        None        -> TyVar x
      | Some tp'    -> tp')
  |  TyList t1       -> TyList (appsubs s t1)                      

                          
let appsubs_amb (s:subst) (g:amb) : amb =
  List.map
    (fun (x, quantificadas, tp) ->
       let s' = List.filter (fun (a, _) -> not (List.mem a quantificadas)) s
       in (x, quantificadas, appsubs s' tp)
    )
    g
  
                      
(* composição de substituições: s1 o s2  *)
let rec compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,y) -> (x, appsubs s1 y))      s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,y) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
let rec appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
                 
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
    TyInt           -> false
  |  TyBool          -> false       
  |  TyFn   (t1,t2)  -> (var_in_tipo v t1)||(var_in_tipo v t2)
  |  TyPair (t1,t2)  -> (var_in_tipo v t1)||(var_in_tipo v t2)
  |  TyVar  x        -> v=x
  |  TyList t1       -> var_in_tipo v t1                      




(* cria novas variáveis para politipos durante a consulta dos mesmos *) 
                       
let rec renamevars (v:int list) (tp:tipo) : tipo =
  match v with
    []      -> tp
  | (h::v') -> let h' = newvar() in
      appsubs [(h,TyVar h')] (renamevars v' tp)

  
(* coleta de restrições e unificação definidas via recursão mútua *)
              
exception UnifyFail of (tipo*tipo)

exception CollectFail of string

               
let rec collect (g:amb) (e:expr) : (constraints*tipo)  =

  match e with

 (* caso alterado para polimorfismo:
    ambientes registram variáveis de tipo
    quantificadas, que recebem nomes
    novos a cada consulta ao ambiente. 
  *)
   
    Var x   ->
      (match lookup g x with
         None        -> raise (CollectFail x)
       | Some (v,tp) -> ([],renamevars v tp))

  | Num n -> ([],TyInt)

  | True  -> ([],TyBool)

  | False -> ([],TyBool)

  | Binop (o,e1,e2) ->  
      if List.mem o [Sum;Sub;Mult;Div]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyInt)
      else   
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyBool)    

  | Pair (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2, TyPair(tp1,tp2))    
        
  | Fst e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in
      (c1@[(tp1,TyPair(TyVar tA, TyVar tB))], TyVar tA)

  | Snd e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in         
      (c1@[(tp1,TyPair(TyVar tA,TyVar tB))], TyVar tB)         

  | If (e1,e2,e3) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      let (c3,tp3) = collect g e3 in         
      (c1@c2@c3@[(tp1,TyBool);(tp2,tp3)], tp2)

  | Fn (x,e1) ->
      let tA       = newvar()           in
      let g'       = (x,[],TyVar tA)::g in
      let (c1,tp1) = collect g' e1      in
      (c1, TyFn(TyVar tA,tp1))
         
  | App (e1,e2) -> 
      let (c1,tp1) = collect  g e1 in
      let (c2,tp2) = collect  g e2  in
      let tA       = newvar()       in
      let tB       = newvar()       in
      (c1@c2@[(tp1,TyFn(TyVar tA,TyVar tB));(tp2,TyVar tA)]
      , TyVar tB) 

  (* let alterado com suporte a polimorfismo Hindley-Milner 

  Versão monomórfica (para comparação):       

| Let (x,e1,e2) ->
  let (c1,tp1) = collect  g e1   in
  let g'       = (x,tp1)::g      in
  let (c2,tp2) = collect  g' e2  in
  (c1@c2, tp2)  
*)

        
  | Let (x,e1,e2) ->
      let (c1,tp1) = collect  g e1 in 
      let sigma    = unify c1 in
      let stp1     = appsubs sigma tp1 in
      let sg       = appsubs_amb sigma g in 
      let polivars = nub (diff (ftv stp1) (ftv_amb sg)) in
      let g'       = (x,polivars,stp1) :: sg in 
      let (c2,tp2) = collect  g' e2 in                     
      (c1@c2, tp2)

  (* LetRec alterado com suporte a polimorfismo Hindley-Milner 
                     
    Versão monomórfica para comparação:
         
| LetRec (f,x,e1,e2) ->
  let tA = newvar() in
  let tB = newvar() in
  let g2 = (f,TyFn(TyVar tA,TyVar tB))::g in
  let g1 = (x,TyVar tA)::g2               in
let (c1,tp1) = collect g1 e1            in
let (c2,tp2) = collect g2 e2            in
(c1@[(tp1,TyVar tB)]@c2, tp2)
*)     

      
  | LetRec (f,x,e1,e2) ->
      let tA = newvar() in
      let tB = newvar() in
      let tf = TyFn(TyVar tA, TyVar tB) in

      let g2 = (f,[],tf) :: g in
      let g1 = (x,[],TyVar tA) :: g2 in
      let (c1,tp1) = collect g1 e1 in

      let cr  = c1 @ [(tp1,TyVar tB)] in
      let s1  = unify cr in
      let tf1 = appsubs s1 tf in
      let sg  = appsubs_amb s1 g in

      let polivars =
        nub (diff (ftv tf1) (ftv_amb sg))
      in
      let g' = (f,polivars,tf1) :: sg in
      let (c2,tp2) = collect g' e2 in
      (cr @ c2, tp2)


  | Nil ->
      let tA = newvar() in
      ([], TyList (TyVar tA))
     
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)
             
  | Head e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyVar tA)

  | IsEmpty e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyBool)

  | Tail e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], tp1)

  | Raise ->
      let tA = newvar() in
      ([],TyVar tA)
          
  | Try (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp1,tp2)],tp1)      

and  unify (c:constraints) : subst =
  match c with
    []                                   -> []
  | (TyInt,TyInt)::c'                    -> unify c'
  | (TyBool,TyBool)::c'                  -> unify c'
  | (TyFn(x1,y1),TyFn(x2,y2))::c'        -> unify ((x1,x2)::(y1,y2)::c')
  | (TyPair(x1,y1),TyPair(x2,y2))::c'    -> unify ((x1,x2)::(y1,y2)::c')
  | (TyList x1,TyList x2)::c'            -> unify ((x1,x2)::c')
  | (TyVar x1, TyVar x2)::c' when x1=x2 -> unify c'

  | (TyVar x1, tp2)::c'                  -> if var_in_tipo x1 tp2
      then raise (UnifyFail(TyVar x1, tp2))
      else compose
          (unify (appconstr [(x1,tp2)] c'))
          [(x1,tp2)]  

  | (tp1,TyVar x2)::c'                  -> if var_in_tipo x2 tp1
      then raise (UnifyFail(tp1,TyVar x2))
      else compose
          (unify (appconstr [(x2,tp1)] c'))
          [(x2,tp1)] 

  | (tp1,tp2)::c' -> raise (UnifyFail(tp1,tp2))




                   

(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
       
let type_infer (e:expr) : unit =
  lastvar := 0; 
  print_string "\nTermo:\n";
  print_string (termo_str e);
  print_string "\n\n";
  try
    let (c,tp) = collect [] e  in
    let s      = unify c       in
    let tf     = appsubs s tp  in
    print_string "\nRestrições:\n";
    print_constr c;
    print_string "Tipo inferido: ";    
    print_string (tipo_str tp);
    print_string "\n\nSubstituição:\n";
    print_subst s;
    print_string "Tipo inferido (após subs): ";
    print_string (tipo_str tf);
    print_string "\n\n"

  with
    
  | CollectFail x   ->
      print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n"
                     
  | UnifyFail (tp1,tp2) ->
      print_string "Erro: impossível unificar os tipos\n* ";
      print_string (tipo_str tp1);
      print_string "\n* ";
      print_string (tipo_str tp2);
      print_string "\n\n"
                           

                   
(*********   EXEMPLOS DE CÓDIGO  ***********)
            
                                                  
(*   exemplo de código 1

   let rec pow = 
        fn x => 
          fn y => 
            if y = 0 
              then 1 
else x * (pow x (y-1))  
in 
(pow 3) 4 


  deve ter tipo "int"

*)

                

(*   (y-1)              *)     
let ys1 = Binop(Sub, Var "y", Num 1)  

(*   pow x (y-1)        *)        
let powapp  =   App(App(Var "pow", Var "x"), ys1)   

(*   x*(pow x (y-1))    *)           
let xp =   Binop(Mult, Var "x", powapp)
   

(*   if y=0 then 1 else x*(pow x (y-1))    *)
let ebdy =  Fn("y", If(Binop(Eq, Var "y", Num 0) , Num 1, xp)) 


(*  let rec pow = fn x => ... in (pow 3) 4 *)
                          
let ex1 = 
  LetRec("pow", "x", ebdy, App(App(Var "pow", Num 3),Num 4)) 



(*   exemplo de código 2

  if raise 
    then (cons 1 empty)
    else empty

deve ter tipo "int list"

*)

let ex2 = If(Raise, Cons(Num 1,Nil), Nil)

  
(*   exemplo de código 3

  if tail raise 
    then (cons true empty)
else raise

    deve falhar pois "tail" sempre retorna um tipo de lista

*)

let ex3 = If(Tail(Raise), Cons(Num 1,Nil), Nil)


(* exemplo de código 4


let id = fn x => x
in if (id true) then (id 0) else 1

    deve retornar "int" em um sistema
    polimórfico, mas falharia em um sistema
                                   monomórfico.

*)
        

let ex4 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), App(Var "id",Num 0), Num 1))

        
(*   exemplo de código 5

  let double = fn f => fn x => f (f x) 
  in  if true
        then double (fn y => y) 3
        else double (fn y => false) true
 
  deve falhar o tipo da parte then e da parte else sao diferentes 

*)

let doublefn = Fn("f",Fn("x",App(Var "f",App(Var "f",Var "x"))))

let doubleif = If(
    True,
    App(App(Var "double",Fn("y",Var "y")),Num 3),
    App(App(Var "double",Fn("y",False)),True)
  )
        
let ex5 = Let("double",doublefn,doubleif)



(* exemplo de código 6

let id = fn x => x
in if (id true) then id else id

    em um sistema polimórfico deve retornar o 
    tipo forall X.(X -> X) 

    em um sistema monomórfico retornaria
    bool -> bool

*)

let ex6 = Let("id",Fn("x",Var "x"),
              If(App(Var "id",True), Var "id", Var "id"));;





(* exemplo de código 7

   let rec f = fn y => (fn z=>z) y
   in f

em um sistema polimórfico deve retornar o 
tipo forall X.(X -> X) 

*)

let ex7 = LetRec("f","y",
                 App(Fn("z",Var "z"),Var "y"),
                 Var "f");;


(* exemplo de código 8

   let rec f = fn y => (f y)
   in f 

em um sistema polimórfico deve retornar o 
tipo "forall X. forall Y. X -> Y" 

*)

let ex8 = LetRec("f","y",
                 App(Var "f",Var "y"),
                 Var "f");;


(* exemplo de código 9

   let rec f = fn y => (fn z => f z) 2
   in f 0

em um sistema polimórfico deve retornar o 
tipo "forall X.X" 

*)

let ex9 = LetRec("f","y",
                 App(Fn("z",App(Var "f",Var "z")),Num 2),
                 App(Var "f",Num 0));;


(* exemplo de código 10

let rec map = fn f => (fn l => if isempty l
                       then nil
                       else cons (f (head l)) (map f (tail l)))
                                        
in map

  em um sistema polimórfico deve retornar o 
  tipo "forall A. forall B. (A->B) -> A list -> B list" 

*)

let ex10 = LetRec ("map","f",
                   Fn("l", If(IsEmpty(Var "l"),
                              Nil,
                              Cons (App(Var "f",Head(Var "l")),
                                    App(App(Var "map",Var "f"),Tail(Var "l"))))),
                   Var "map");;

let negfn = Fn("x", If(Var "x", False, True))
let incr = Fn("x", Binop(Sum, Var "x", Num 1))

let l1 = Cons(Num 10,Cons(Num 20,Cons(Num 30,Nil))) 
let l2 = Cons(True,Cons(False, Cons(True,Nil))) 

let ex10 = LetRec ("map","f",
                   Fn("l", If(IsEmpty(Var "l"),
                              Nil,
                              Cons (App(Var "f",Head(Var "l")),
                                    App(App(Var "map",Var "f"),Tail(Var "l"))))),
                   Pair(App(App(Var "map", incr ), l1), 
                        App(App(Var "map", negfn), l2)
                       )
                  )
    
    
(* fn x =>
     let rec f = fn y => x y
     in (f 1, f true) *)
let ex11 =
  Fn ("x",
      LetRec ("f", "y",
              App (Var "x", Var "y"),
              Pair (App (Var "f", Num 1),
                    App (Var "f", True))))


(* interpretador com avaliador no estilo big step para o core de L1 *)


(* ===== Datatypes para ASTs e funções auxiliares sobre ASTs =====  *) 

type typ =
  | TInt
  | TBool
  | TFn of typ * typ 
  

type bop = 
  | Sum | Sub | Mul | Div  (* ariméticos *)
  | Eq  | Lt | Gt          (* relacionais *)
  | Or  | And              (* lógicos *)
  
  
type uop = 
  | Not   
  | Minus

type ast =
  | Num of int 
  | Bool of bool 
  | Binop of bop * ast * ast 
  | If of ast * ast * ast
  | Unop of uop * ast
  | Id of string 
  | Fn of string * typ * ast
  | App of ast * ast
  | Let    of string * typ         * ast                   * ast
  | LetRec of string * (typ * typ) * (string * typ * ast)  * ast 
              
type value = 
  | VNum of int
  | VBool of bool
  | VClos of string * ast * runEnv
  | VRecClos of string * string * ast * runEnv
and runEnv = 
  (string * value) list
            
    (*let is_value (e:ast) : bool = 
       match e with
       | Num _ | Bool _ | Fn _ -> true
       | _ -> false *)
          
let string_of_value (v: value) : string =
  match v with 
  | VNum n  -> string_of_int n 
  | VBool b -> string_of_bool b 
  | VClos _ -> "<fun>"
  | VRecClos _ -> "<fun>"
    
  
  
let rec string_of_type (t : typ) : string =
  match t with
  | TInt  -> "int"
  | TBool -> "bool"
  | TFn (t1, t2) ->
      let s1 = 
        match t1 with
        | TFn _ -> "(" ^ string_of_type t1 ^ ")"
        | _    -> string_of_type t1
      in
      s1 ^ " -> " ^ string_of_type t2


(* ==== Análise Semântica ===== *)


type tyenv = (string * typ)  list
    
    

let insert k v lst = (k, v) :: lst

let rec lookup k lst = 
  match lst with 
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

exception TypeError of string  (* erro a ser reportado para o usuário do interpretador *)


(* type_of pode ativar exceçao TypeError *) 
let rec type_of (g:tyenv) (e:ast) : typ =
  match e with
  | Num _  -> TInt
    
  | Bool _ -> TBool 

  | Binop(bop, e1,e2) -> 
      let t1 = type_of g e1 in
      let t2 = type_of g e2 in
      (match bop with
       | Sum | Sub | Mul | Div ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError "Operadores aritméticos requerem operandos inteiros")
       | Eq | Lt | Gt ->
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError "Operadores relacionais requerem operandos inteiros")
       | And | Or  ->
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError "Operadores lógicos requerem operandos booleanos"))


  | If(e1,e2,e3) ->
      if type_of g e1 <> TBool then raise (TypeError "Condiçao de if-then-else deve ser do tipo bool")
      else 
        let t2 = type_of g e2 in
        let t3 = type_of g e3 in 
        if t2 <> t3 then raise (TypeError "Expressoes do then e do else devem ser do mesmo tipo")
        else t2
          
  |  Unop(Not, e1) -> 
      if type_of g e1 = TBool then TBool else raise (TypeError "Operanod de not deve ser booleano")
          
  | Unop(Minus, e1) ->
      if type_of g e1 = TInt then TInt else raise (TypeError "Operando de - deve ser inteiro")
      
  | Id x ->  
      (match lookup x g with
       | Some t -> t
       | None -> raise (TypeError ("Identifcador nao declarado: " ^ x)))
                   
  | Fn(x,t,e1) -> failwith "nao implementado"
  | App(e1,e2) -> failwith "nao implementado"
  | Let(x,t1,e1,e2) -> failwith "nao implementado"
  | LetRec(f,(tin,tout), (x,t,e1), e2) -> failwith "nao implementado"
  
  
(* === Avaliador ==== *)

  
exception RunTimeError of string (* erro a ser reportado para o usuário do interpretador *)
                                 
                        
      (* implementa semantica big step *)                        
let rec eval (r: runEnv) (e:ast) : value  = 
  match e with
  | Num n   -> VNum n
  | Bool b  -> VBool b
    
  | Binop(bop, e1,e2)  -> 
      let v1 = eval r e1 in
      let v2 = eval r e2 in
      (match (bop, v1, v2) with
       | (Sum, VNum n1, VNum n2)  -> VNum(n1 + n2)
       | (Sub, VNum n1, VNum n2)  -> VNum(n1 - n2)
       | (And, VBool b1, VBool b2)  -> VBool(b1 && b2)
       | (Eq, VNum n1, VNum n2)  -> VBool(n1 = n2)
       | _ -> failwith "nao implementado" )
  
  
  | If(e1,e2,e3) ->  
      let v1 = eval r e1 in
      (match v1 with
       | VBool true -> eval r e2
       | VBool false -> eval r e3
       | _ -> raise (RunTimeError ("erro de execuçãoo de if-then-else")))
                                                                                                      
  | Unop(uop,e1)  -> failwith "unop nao implememntado" 
        
  | Id x ->  
      (match lookup x r with
       | Some v -> v
       | None -> raise (RunTimeError ("Identificador nao declarado:" ^ x)))
               
  | Fn(x,t,e1) -> VClos(x,e1,r)
                    
                    
  | App(e1,e2) ->
      let v1 = eval r e1 in
      let v2 = eval r e2 in
      (match v1 with 
       | VClos(x,e', r')              ->  eval ((x,v2) ::             r') e'
       | VRecClos (f,x,e',r') as rcls ->  eval ((x,v2) :: (f,rcls) :: r') e'
       | _ ->  raise (RunTimeError ("lado esquerdo da aplicaçao nao e funçao"))
      )
      
      
  | Let(x,t1,e1,e2)                  -> failwith "let nao implementado" 
                                          
  | LetRec(f,(tin,tout), (x,t,e1), e2) -> failwith "let rec nao implementado" 
        
  

    


(* ==== Interpretador =====  *)
          
let interpreter (e:ast) : string = 
  try
    let t = type_of [] e in
    let v = eval    [] e  in
    (string_of_value v) ^ " : "  ^ (string_of_type t)
  with 
  | TypeError msg    -> "Erro de tipo - " ^ msg 
  | RunTimeError msg -> "Erro de execuçao - " ^ msg 

(* ====== TESTES ===== *)

let tst1 = Binop(Sum, Num 10, Num 5)

let tst2 = If(Binop(Eq, tst1, Num 15), Num 5,  Num 50)
    
let tst3 = If(Binop(Sum, tst1, Num 15), Num 5,  Num 50)
    
let tst4 = If(Binop(Eq, tst1, Bool true), Num 5,  Num 50)
    
let tst5 = If(Bool true, Num 5, Bool false)
    
let tst6 = Binop(Div, Num 6, Num 0)
  
let tst7 = Binop(Mul, Num 10, Bool true)
    
let tst8 = Unop( Not, Bool false) 
    
let tst9 = Unop(Minus, tst2)
    
let tst10 = Fn("x",TInt, Binop(Sum, Num 10, Id "x"))
    
let tst11 = App(tst10, Num 50)
    
let corpofat = If(Binop(Eq,Id "x", Num 0),
                  Num 1,
                  Binop(Mul,Id "x",App(Id "x", Binop(Sub,Id "x", Num 1))))
    
let tstFat = LetRec("fat", (TInt,TInt),
                    ("x", TInt, corpofat),
                    App(Id "fat", Num 5))
  
  
        (*++++++++++++++++++++++++++++++++++++++*)
(*  Compilador para L1                  *)
(*   - inferência de tipos              *)
(*   - traduação para SSM1              *)
(*  Simulador  para L1                  *)
(*++++++++++++++++++++++++++++++++++++++*)



(* ===== Datatypes para ASTs e funções auxiliares sobre ASTs =====  *) 

type typ =
    TInt
  | TBool
  | TFn of typ * typ 
           
type bop = 
  | Sum | Sub | Mul | Div  (* ariméticos *)
  | Eq  | Lt | Gt          (* relacionais *)
  | Or  | And              (* lógicos *)
  

type uop = 
  | Not   
  | Minus

type ast =
  | Num of int 
  | Bool of bool 
  | Binop of bop * ast * ast 
  | If of ast * ast * ast
  | Unop of uop * ast
  | Id of string 
  | Fn of string * typ * ast
  | App of ast * ast
  | Let    of string * typ         * ast                   * ast
  | LetRec of string * (typ * typ) * (string * typ * ast)  * ast 


let is_value (e:ast) : bool = 
  match e with
  | Num _ | Bool _ | Fn _ -> true
  | _ -> false
          
let string_of_value (e: ast) : string =
  match e with 
  | Num n  -> string_of_int n 
  | Bool b -> string_of_bool b 
  | Fn _ -> "<fun>"
  | _ -> failwith "expressão nao é um valor"
  
  
let rec string_of_type (t : typ) : string =
  match t with
  | TInt  -> "int"
  | TBool -> "bool"
  | TFn (t1, t2) ->
      let s1 = 
        match t1 with
        | TFn _ -> "(" ^ string_of_type t1 ^ ")"
        | _    -> string_of_type t1
      in
      s1 ^ " -> " ^ string_of_type t2
 
 (* ==== Análise Semântica ===== *)


type tyenv = (string * typ)  list
  

let insert k v lst = (k, v) :: lst

let rec lookup k lst = 
  match lst with 
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

exception TypeError of string  (* erro a ser reportado para o usuário do interpretador *)       

  

(* exceções que não devem ocorrer  *)


  
(* type_of pode ativar exceçao TypeError *) 
let rec type_of (g:tyenv) (e:ast) : typ =
  match e with
  | Num _  -> TInt
    
  | Bool _ -> TBool 

  | Binop(bop, e1,e2) -> 
      let t1 = type_of g e1 in
      let t2 = type_of g e2 in
      (match bop with
       | Sum | Sub | Mul | Div ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError "Operadores aritméticos requerem operandos inteiros")
       | Eq | Lt | Gt ->
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError "Operadores relacionais requerem operandos inteiros")
       | And | Or  ->
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError "Operadores lógicos requerem operandos booleanos"))


  | If(e1,e2,e3) ->
      if type_of g e1 <> TBool then raise (TypeError "Condiçao de if-then-else deve ser do tipo bool")
      else 
        let t2 = type_of g e2 in
        let t3 = type_of g e3 in 
        if t2 <> t3 then raise (TypeError "Expressoes do then e do else devem ser do mesmo tipo")
        else t2
          
  |  Unop(Not, e1) -> 
      if type_of g e1 = TBool then TBool else raise (TypeError "Operanod de not deve ser booleano")
          
  | Unop(Minus, e1) ->
      if type_of g e1 = TInt then TInt else raise (TypeError "Operando de - deve ser inteiro")
      
  | Id x ->  
      (match lookup x g with
       | Some t -> t
       | None -> raise (TypeError ("Identifcador nao declarado: " ^ x)))
                   
  | Fn(x,t,e1) -> failwith "nao implementado"
  | App(e1,e2) -> failwith "nao implementado"
  | Let(x,t1,e1,e2) -> failwith "nao implementado"
  | LetRec(f,(tin,tout), (x,t,e1), e2) -> failwith "nao implementado"





  
      

(*++++++++++++++++++++++++++++++++++++++*++++++*)
(*        SINTAXE DA LINGUAGEM SSM1            *)
(*+++++++++++++++++++++++++++++++++++++++++++++*) 

type instr =
    INT of int 
  | BOOL of bool
  | POP
  | COPY
  | ADD 
  | MULT 
  | INV
  | EQ
  | GT
  | AND
  | NOT 
  | JMP of int       (* deveria ser nat ... *)
  | JMPIFTRUE of int (* deveria ser nat ... *)
  | VAR of string
  | FUN of string * instr list
  | RFUN of string * string * instr list
  | APPLY
    
type code = instr list 


(*++++++++++++++++++++++++++++++++++++++*++++++*)
(*         TRADUÇÃO DE L1 PARA SSM1            *)
(*+++++++++++++++++++++++++++++++++++++++++++++*)


(* operações : op ::= Sum | Sub | Mul | Eq | Gt | Lt | Geq | Leq *)   
let tradop (oper:bop) (c1:code) (c2:code) : code = 
  match oper with 
    Sum -> c1 @ c2 @ [ADD] 
  | Sub -> c1 @ c2 @ [INV;ADD]
  | Mul -> c1 @ c2 @ [MULT]
  | Eq  ->  c1 @ c2 @ [EQ]
  | _ -> failwith "tradução de operador binario nao implementado" 


let rec trad (e:ast) : code = 
  match e with 
    Num n -> 
      [INT n]

  | Bool b -> 
      [BOOL b]

  | Binop(oper,e1,e2) ->
      let c1 = trad e1 in
      let c2 = trad e2 in
      tradop oper c1 c2

  | If(e1,e2,e3) -> 
      let c2 = trad e2 in
      let c3 = trad e3 in 
      let n2 = List.length c2 in
      let n3 = List.length c3 in
      (trad e1) @ [JMPIFTRUE (n3+1)] @ c3 @ [JMP n2] @ c2 

  | Id x ->
      [VAR x] 

  | App(e1,e2) -> 
      (trad e1) @ (trad e2) @ [APPLY] 

  | Fn(x,_,e') -> 
      [FUN(x, trad e')]

    (* let x = e1 in e2    equivalente a    (fn x => e2) e1    *)
  | Let(x,_,e1,e2) ->
      [FUN(x,trad e2)] @ (trad e1) @ [APPLY] 

        
  | LetRec(f,(_,_), (y,_,e1), e2) -> 
      [FUN(f,trad e2); RFUN(f,y,trad e1); APPLY]
      
  | Unop(uop,e1) ->  failwith "traduçao de operador unário nao implementado" 

           
(*+++++++++++++++++++++++++++++++++++++++++++++++++*)        
(*      PRINCIPAL DO COMPILADOR                    *) 
(*+++++++++++++++++++++++++++++++++++++++++++++++++*)   


(* principal do compilador *)

let compila (e:ast) :  code  =
  try 
    let t = type_of [] e in
    let c = trad e
    in  print_string ("programa do tipo: "^ (string_of_type t));  c
  with 
    TypeError msg -> print_string ("Erro de tipo - " ^ msg); []
      
    



(*++++++++++++++++++++++++++++++++++++++*++++++*)
(*        SIMULADOR PARA SSM1                  *)
(*+++++++++++++++++++++++++++++++++++++++++++++*)


(* valores armazenáveis e            *)
(* componentes dos estados de SSM1   *)
    
type strvalue = 
    SINT of int
  | SBOOL of bool
  | CLOS of  string * code * environment
  | RCLOS of string * string * code * environment 
and 
  environment = (string * strvalue) list  
      
type stack =  strvalue list
    
type dump = (code * stack * environment) list 
    
type state = code * stack * environment * dump


(* função auxiliar drop para operações de desvio *)
exception InvalidArg 

let rec drop n lst  =
  if (n <0) then raise InvalidArg else
    match (n,lst) with
      (0,_) ->  Some lst 
    | (n, _::tl) -> drop (n-1) tl
    | (n, []) -> None 



exception NoRuleApplies
  
let step (state: state) : state =
  match state with 
  
  | ([], sv :: [],e, (c', s', e') :: d) -> (c', sv :: s', e', d)

(* operações que colocam na pilha  - não falham *)
  | ((INT z) :: c,       s,e,d) -> (c, (SINT z):: s,e,d)                        
  | ((BOOL b) :: c,      s,e,d) -> (c, (SBOOL b):: s,e,d)
  | ((FUN(x,c')) :: c,   s,e,d) -> (c, (CLOS(x,c',e)):: s,e,d)
  | ((RFUN(f,x,c')) :: c,s,e,d) -> (c, (RCLOS(f,x,c',e)):: s,e,d) 
                                                                  
(* operações que precisam de pilha não vazia *) 
  | (POP :: c,    sv :: s,e,d) -> (c,s,e,d) 
  | (COPY :: c,   sv :: s,e,d) -> (c, sv :: sv ::s, e,d) 
  

(* precisam de pilha com pelo menos 1 elto especifico no topo *)

  | (INV :: c, SINT z :: s,e,d) -> (c, (SINT (- z)) :: s,e,d) 
  | (NOT ::c, SBOOL b :: s,e,d) -> (c, (SBOOL (not b)) :: s,e,d)
  | ((JMPIFTRUE n) :: c, (SBOOL false) :: s,e,d) -> (c,s,e,d)
                                                    
(* precisa pilha c/ pelo menos 1 elto especifico no topo e restri. adicional *)                                                    
  | ((JMPIFTRUE n)::c,(SBOOL true)::s,e,d) when (n>=0 && n<= List.length c) -> 
      (Option.get (drop n c),s,e,d)
  
(* ops que precisam de pilha com pelo menos 2 eltos especificos no topo *) 
  | (ADD ::c, SINT z1 :: SINT z2 :: s,e,d) -> (c, (SINT (z1 + z2)) :: s,e,d) 
  | (MULT ::c, SINT z1 :: SINT z2 :: s,e,d) -> (c, (SINT (z1 * z2)) :: s,e,d)                   
  | (EQ :: c, SINT z1 :: SINT z2 :: s,e,d) -> (c, (SBOOL (z1=z2)) :: s,e,d)                                     
  | (GT :: c, SINT z1 :: SINT z2 :: s,e,d) -> (c, (SBOOL (z1>z2)) :: s,e,d)                                       
  | (AND::c, SBOOL b1 :: SBOOL b2 :: s,e,d) -> (c,(SBOOL (b1 && b2)):: s,e,d) 
  | (APPLY :: c, sv::(CLOS(x,c',e')):: s,e,d) ->
      (c',[], (x,sv) :: e', (c,s,e) :: d)
  | (APPLY :: c, sv::(RCLOS(f,x,c',e')) :: s,e,d) ->
      (c',[], (f,RCLOS(f,x,c',e')) :: (x,sv) :: e', (c,s,e) :: d)
  
(*  nao operam com pilha *)
  | ((JMP n):: c,s,e,d) when (n>=0) && (n<= List.length c) -> 
      (Option.get (drop n c) ,s,e,d) 
      
  | ((VAR x) :: c, s,e,d) ->
      (match lookup x e with
         Some v -> (c, v::s, e,d)
       | None -> raise NoRuleApplies)
  
  | _ -> raise NoRuleApplies
      
  
(* avaliador para SSM1 *)

let rec eval (st:state) : state = 
  try
    let st' = step st 
    in eval st' 
  with NoRuleApplies -> st 
                            
let simula c = eval (c, [],[],[])
  

 (* +++++++++++++++++++++++++++++++++++++++*)
 (*                TESTES                  *)
 (*++++++++++++++++++++++++++++++++++++++++*)

(*
     let x:int = 2 
     in let foo: int --> int = fn y:int => x + y 
        in let x: int = 5
           in foo 10 
*)

let e'' = Let("x", TInt, Num 5, App(Id "foo", Num 10))

let e'  = Let("foo", TFn(TInt,TInt), Fn("y", TInt, Binop(Sum, Id "x", Id "y")), e'')

let tst = Let("x", TInt, Num(2), e') 
    
  

(* programa pow x y = xˆy   *)  
    
let ys1 = Binop(Sub, Id "y", Num 1)     (* y - 1  *)
  
let powapp  =   App(App(Id "pow", Id "x"), ys1)   (* pow *)
                                           
let xp =   Binop(Mul, Id "x", powapp)
      
    (* fn y:int => if y=0 then 1 else x * (pow x (y-1))    *) 
let ebdy = Fn("y",TInt,If(Binop(Eq, Id "y", Num 0) , Num 1, xp))  
  
let pw = 
  LetRec("pow", (TInt, TFn(TInt,TInt)), (* pow: int --> (int --> int) *)
         ("x", TInt, ebdy), (* fn x: int => ebdy  *)
         App(App(Id "pow", Num 3), Num 4)) (* in  (pow 3) 4    *)
                                            
                                            
  
                                           
    
    