(* helper functions *)
let rec rev xs =
  let rec rev' xs a = match xs with
    | [] -> a
    | x::xr -> rev' xr (x::a)
  in rev' xs []

let rec map f l = match l with
 | [] -> []
 | x :: l -> (f x) :: map f l

(* environments *)
let rec lookup k l = match l with
 | [] -> None
 | (k', v) :: l -> if k = k' then Some v else lookup k l
 
let rec update k v l = match l with
 | [] -> [(k, v)]
 | (k', v') :: l -> if k = k' then (k, v) :: l
                    else (k', v') :: update k v l
                    
type 'a env = (string * 'a) list

(* split string into a list of characters *)
let explode s =
  let rec explode' i l =
    if i = 0 then l 
             else explode' (i - 1) (String.get s (i - 1) :: l) in
  explode' (String.length s) []
  
(* construct string from a list of characters *)
let implode cs =
  let rec implode' cs s = match cs with
    | [] -> s
    | c::cr -> implode' cr (s ^ (String.make 1 c))
  in implode' cs ""
  

(* types *)
type ty = Int | Bool | Arrow of ty * ty

(* binary operators (for AST) *)
type op = Add | Mul | Sub | Leq | Geq | Lt | Gt | Eq | Neq | LazyAnd | LazyOr

(* unary operators *)
type uop = Not | Neg

(* MiniOCaml expressions (for AST) *)
type exp =
 | Icon of int               (* integer constant *)
 | Bcon of bool              (* Boolean constant *)
 | Var of string             (* identifier *)
 | If of exp * exp * exp     (* conditional: if e1 then e2 else e3 *)
 | Oapp of exp * op * exp    (* binary operator: e1 op e2 *)
 | Uapp of uop * exp         (* unary operator: op e*)
 | Let of string * exp * exp (* local declaration: let X = e1 in e2 *)
 | Lam of string * ty * exp  (* lambda function: fun (X: t) -> e1*)
 | Fapp of exp * exp         (* function application: e1 e2*)
 | Letrec of string * string * ty * ty * exp * exp
                             (* recursive let expression: let rec F (X: t1) : t2 = e1 in e2 *)
      
(* language tokens (lexer output) *)
type const = BCON of bool | ICON of int
type token = LP | RP | EQ | COL | ARR | ADD | SUB | MUL | LEQ
           | IF | THEN | ELSE | LAM | LET | IN | REC
           | CON of const | VAR of string | BOOL | INT
           | NEQ | LT | GT | GEQ | NOT | NEG
 
 
(* Mini-OCaml lexer *)
let lex s : token list =
  let is_digit c = '0' <= c && c <= '9' in
  let is_lower c = 'a' <= c && c <= 'z' in
  let is_upper c = 'A' <= c && c <= 'Z' in
  let is_whitespace c = match c with ' ' | '\t' | '\n' | '\r' -> true 
                                   | _ -> false in
  let digit_val c = Char.code c - Char.code '0' in
  let rec lex' cs acc = match cs with
    | [] -> rev acc
    | '('::cr -> lex' cr (LP::acc)
    | ')'::cr -> lex' cr (RP::acc)
    | ':'::cr -> lex' cr (COL::acc)
    | '-'::'>'::cr -> lex' cr (ARR::acc)
    | '+'::cr -> lex' cr (ADD::acc)
    | '-'::cr -> lex' cr (SUB::acc)
    | '*'::cr -> lex' cr (MUL::acc)
    | '<'::'='::cr -> lex' cr (LEQ::acc)
    | '='::'>'::cr -> lex' cr (GEQ::acc)
    | '<'::'>'::cr -> lex' cr (NEQ::acc)
    | '<'::cr -> lex' cr (LT::acc)
    | '>'::cr -> lex' cr (GT::acc)
    | '='::cr -> lex' cr (EQ::acc)
    | '~'::cr -> lex' cr (NEG::acc)
    | c::cr when is_digit c -> lex_num (c::cr) acc 0
    | c::cr when is_lower c -> lex_id (c::cr) acc []
    | c::cr when is_whitespace c -> lex' cr acc
    | _ -> failwith "lex: illegal character"
  and lex_num cs acc n = match cs with (* lex a numeric constant *)
    | c::cr when is_digit c -> lex_num cr acc (n * 10 + digit_val c)
    | _ -> lex' cs (CON(ICON(n))::acc)
  and lex_id cs acc id = match cs with (* lex an identifier or keyword *)
    | c::cr when is_lower c -> lex_id cr acc (c::id)
    | c::cr when is_upper c -> lex_id cr acc (c::id)
    | c::cr when is_digit c -> lex_id cr acc (c::id)
    | '_'::cr -> lex_id cr acc ('_'::id)
    | '\''::cr -> lex_id cr acc ('\''::id)
    | _ -> lex_id' cs (implode (rev id)) acc
  and lex_id' cs id acc = match id with (* lex keywords *)
    | "true" -> lex' cs (CON(BCON(true))::acc)
    | "false" -> lex' cs (CON(BCON(false))::acc)
    | "if" -> lex' cs (IF::acc)
    | "then" -> lex' cs (THEN::acc)
    | "else" -> lex' cs (ELSE::acc)
    | "fun" -> lex' cs (LAM::acc)
    | "let" -> lex' cs (LET::acc)
    | "rec" -> lex' cs (REC::acc)
    | "in" -> lex' cs (IN::acc)
    | "int" -> lex' cs (INT::acc)
    | "bool" -> lex' cs (BOOL::acc)
    | "not" -> lex' cs (NOT::acc)
    | _ -> lex' cs (VAR(id)::acc)
  in lex' (explode s) []



(* Mini-OCaml parser *)
exception ExpectedVar of token list
exception ExpectedToken of token * token list

(* check if next token in ts is identifier *)
let expect_var ts = match ts with
  | VAR s :: ts -> s, ts
  | _ -> raise (ExpectedVar ts)

(* check if next token in ts is t *)
let expect t ts = match ts with
  | t' :: ts when t = t' -> ts
  | _ -> raise (ExpectedToken (t, ts))

let parse_exp ts =
  let rec parse_simple ts = match ts with 
    (* parse high-level structures using recursive descent *)
    | VAR s :: tr -> Var s, tr
    | CON (ICON c) :: tr -> Icon c, tr
    | CON (BCON c) :: tr -> Bcon c, tr
    | LP :: tr  -> let e, tr = parse_binary tr in  (* parse parentheses: (e1) *)
                   let    tr = expect RP tr in
                   e, tr
    | IF :: tr  -> let e1, tr = parse_binary tr in (* parse conditional: if e1 then e2 else e3 *)
                   let     tr = expect THEN tr in
                   let e2, tr = parse_binary tr in
                   let     tr = expect ELSE tr in
                   let e3, tr = parse_binary tr in
                   If (e1, e2, e3), tr
    | LET :: REC :: tr -> let  f, tr = expect_var tr in  (* parse recursive let expression:   *)
                          let     tr = expect LP tr in   (* let rec F (X: t1) : t2 = e1 in e2 *)
                          let  x, tr = expect_var tr in
                          let     tr = expect COL tr in
                          let t1, tr = parse_ty tr in
                          let     tr = expect RP tr in
                          let     tr = expect COL tr in
                          let t2, tr = parse_ty tr in
                          let     tr = expect EQ tr in
                          let e1, tr = parse_binary tr in
                          let     tr = expect IN tr in
                          let e2, tr = parse_binary tr in
                          Letrec (f, x, t1, t2, e1, e2), tr
    | LET :: tr -> let  x, tr = expect_var tr in (* parse local declaration: let X = e1 in e2 *)
                   let     tr = expect EQ tr in
                   let e1, tr = parse_binary tr in
                   let     tr = expect IN tr in
                   let e2, tr = parse_binary tr in
                   Let (x, e1, e2), tr
    | LAM :: tr -> let     tr = expect LP tr in (* parse lambda expressions: fun (X: t) -> e *)
                   let  x, tr = expect_var tr in
                   let     tr = expect COL tr in
                   let  t, tr = parse_ty tr in
                   let     tr = expect RP tr in
                   let     tr = expect ARR tr in
                   let  e, tr = parse_binary tr in
                   Lam (x, t, e), tr
    | NOT :: tr -> let  e, tr = parse_binary tr in
                   Uapp (Not, e), tr
    | NEG :: tr -> let  e, tr = parse_binary tr in
                   Uapp (Neg, e), tr
    | _ -> failwith "parse: illegal tokens"
  and parse_binary ts = 
    (* parse binary (infix) operators using operator precedence parsing *)
    let rec parse_prec p (l, ts) = match parse_op ts with
      | None -> l, ts
      | Some (op, lp, rp, ts') -> 
          if lp < p then (l, ts)
                    else let r, ts = parse_prec rp (parse_simple ts')
                         in parse_prec p (op l r, ts)
    in parse_prec 0 (parse_simple ts)
  and parse_op ts =
    let create_oapp op l r = Oapp (l, op, r) in 
    let create_Fapp l r = Fapp (l, r) in 
    match ts with
      | LEQ :: tr   -> Some (create_oapp Leq, 1, 1, tr)
      | GEQ :: tr   -> Some (create_oapp Geq, 1, 1, tr)
      | LT  :: tr   -> Some (create_oapp  Lt, 1, 1, tr)
      | GT  :: tr   -> Some (create_oapp  Gt, 1, 1, tr)
      | EQ  :: tr   -> Some (create_oapp  Eq, 1, 1, tr)
      | NEQ :: tr   -> Some (create_oapp Neq, 1, 1, tr)
      | ADD :: tr   -> Some (create_oapp Add, 2, 3, tr)
      | SUB :: tr   -> Some (create_oapp Sub, 2, 3, tr)
      | MUL :: tr   -> Some (create_oapp Mul, 4, 5, tr)
      | LP  :: tr   -> Some (create_Fapp, 6, 7, ts)
      | CON _ :: tr -> Some (create_Fapp, 6, 7, ts)
      | VAR _ :: tr -> Some (create_Fapp, 6, 7, ts)
      | _ -> None
  and parse_ty ts = 
    let rec parse_ty' ts lt = (match ts with
      | ARR :: tr -> let rt, tr = parse_ty tr in
                     Arrow (lt, rt), tr
      | _         -> lt, ts)
    in match ts with
      | INT :: tr  -> parse_ty' tr Int
      | BOOL :: tr -> parse_ty' tr Bool
      | LP :: tr   -> let t, tr = parse_ty tr in
                      let    tr = expect RP tr in
                      parse_ty' tr t
      | _          -> failwith "parse_ty: expected type token(s)"
    
  in match parse_binary ts with 
    (* check that all tokens were eaten by parser*)
    | t, [] -> t
    | _ -> failwith "parse: junk at end"
    
 
 
(* Mini-OCaml type checking *)
type tenv = ty env

let rec check_ty (tyenv: tenv) (e: exp) = match e with
 | Icon _ -> Some Int
 | Bcon _ -> Some Bool
 | Var x -> lookup x tyenv
 | If (e1, e2, e3) -> let t1 = check_ty tyenv e1 in
                      let t2 = check_ty tyenv e2 in
                      let t3 = check_ty tyenv e3 in
                      (match t1, t2, t3 with
                       | (Some Bool, Some x, Some y) ->
                         if x = y then Some x else None
                       | _ -> None)
 | Let (x, e1, e2) -> let t1 = check_ty tyenv e1 in
                      (match t1 with
                       | None -> None
                       | Some tx -> let tyenv' = update x tx tyenv in check_ty tyenv' e2)
 | Oapp (e1, o, e2) -> let t1 = check_ty tyenv e1 in
                       let t2 = check_ty tyenv e2 in
                       (match t1, o, t2 with
                        | (Some Int, Add, Some Int) -> Some Int
                        | (Some Int, Mul, Some Int) -> Some Int
                        | (Some Int, Sub, Some Int) -> Some Int
                        | (Some Int, Leq, Some Int) -> Some Bool
                        | (Some Int, Lt,  Some Int) -> Some Bool
                        | (Some Int, Eq,  Some Int) -> Some Bool
                        | (Some Int, Neq, Some Int) -> Some Bool
                        | (Some Bool, LazyOr,  Some Bool) -> Some Bool
                        | (Some Bool, LazyAnd, Some Bool) -> Some Bool
                        | _ -> None)
 | Uapp (o, e) -> let t = check_ty tyenv e in
                  (match t, o with
                   | Some Bool, Not -> Some Bool
                   | Some Int, Neg -> Some Int
                   | _ -> None)
 | Lam (x, t, e) -> (match check_ty (update x t tyenv) e with
                     | Some t' -> Some (Arrow (t, t'))
                     | _ -> None)
 | Fapp (e1, e2) -> let t1 = check_ty tyenv e1 in
                    let t2 = check_ty tyenv e2 in
                    (match t1, t2 with
                     | Some (Arrow (t1, t2)), Some t3 when t1 = t3 -> Some t2
                     | _ -> None)
 | Letrec (f, x, t1, t2, e1, e2) -> let tlam = check_ty (update x t1 (update f (Arrow (t1, t2)) tyenv)) e1 in
                                    let tres = check_ty (update f (Arrow (t1, t2)) tyenv) e2 in 
                                    if tlam = Some t2 then tres else None



(* Mini-OCaml evaluator *)
type va = Ival of int | Bval of bool
  | Cl of string * exp * venv
  | Cr of string * string * exp * venv
and venv = va env

let rec eval (env: venv) (e: exp) : va = match e with
  | Icon c -> Ival c
  | Bcon b -> Bval b
  | Var x -> (match lookup x env with Some v -> v
             | None -> failwith "Missing value")
  | If (e1, e2, e3) -> (match eval env e1 with
                        | Bval true -> eval env e2
                        | Bval false -> eval env e3
                        | _ -> failwith "Type not correct")
  | Let (x, e1, e2) -> (let vx = eval env e1 in
                        let env' = update x vx env in
                        eval env' e2)
  | Oapp (e1, LazyOr, e2) -> (match eval env e1 with
                              | Bval true -> Bval true
                              | Bval false -> eval env e2
                              | _ -> failwith "Illegal value") 
  | Oapp (e1, LazyAnd, e2) -> (match eval env e1 with
                              | Bval false -> Bval false
                              | Bval true -> eval env e2
                              | _ -> failwith "Illegal value") 
  | Oapp (e1, o, e2) -> let v1 = eval env e1 in
                        let v2 = eval env e2 in
                        (match o, v1, v2 with
                         | Add, Ival x, Ival y -> Ival (x + y)
                         | Mul, Ival x, Ival y -> Ival (x * y)
                         | Sub, Ival x, Ival y -> Ival (x - y)
                         | Leq, Ival x, Ival y -> Bval (x <= y)
                         | Lt,  Ival x, Ival y -> Bval (x < y)
                         | Eq,  Ival x, Ival y -> Bval (x = y)
                         | Neq, Ival x, Ival y -> Bval (x <> y)
                         | _ -> failwith "Illegal value")
  | Uapp (o, e) -> let v = eval env e in
                   (match o, v with 
                    | Not, Bval x -> Bval (not x)
                    | Neg, Ival x -> Ival (-x)
                    | _ -> failwith "Illegal value")
  | Lam (x, _, e) -> Cl (x, e, env)
  | Fapp (e1, e2) -> let v1 = eval env e1 in
                     let v2 = eval env e2 in
                     (match v1, v2 with
                      | Cl (x, e, env), v2 -> eval (update x v2 env) e
                      | Cr (f, x, e, env), v2 -> eval (update x v2 (update f v1 env)) e
                      | _ -> failwith "Illegal value")
  | Letrec (f, x, _, _, e1, e2) -> let cls = Cr (f, x, e1, env) in
                                   eval (update f cls env) e2



(* The full interpreter *)
let interpret s =
  let tokens = lex s in
  let ast    = parse_exp tokens in
  match check_ty [] ast with
    | Some _ -> eval [] ast
    | None   -> failwith "check_ty failed"

;;
interpret "let rec f (n: int) : bool = if n = 0 then true else not f (n-1) in f 42"