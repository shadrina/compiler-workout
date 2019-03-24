open GT       
open Language
open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let instrEval (stack, (s, i, o)) instruction = match instruction with
  | BINOP op -> (match stack with
    | y :: x :: tail -> ((Language.Expr.binopEval op x y) :: tail, (s, i, o))
    | _              -> failwith "Not enough elements in stack")
  | CONST z  -> (z :: stack, (s, i, o))
  | READ     -> (match i with
    | z :: tail -> (z :: stack, (s, tail, o))
    | _         -> failwith "Not enough elements in input")
  | WRITE    -> (match stack with
    | z :: tail -> (tail, (s, i, o @ [z]))
    | _         -> failwith "Not enough elements in stack")
  | LD x     -> ((s x) :: stack, (s, i, o))
  | ST x     -> (match stack with
    | z :: tail -> (tail, (Language.Expr.update x z s, i, o))
    | _         -> failwith "Not enough elements in stack")
  | LABEL l  ->  (stack, (s, i, o))

let rec eval env cfg p = match p with
  | instr::tail -> (match instr with
    | LABEL l        -> eval env cfg tail
    | JMP l          -> eval env cfg (env#labeled l)
    | CJMP (znz, l)  -> (let (st, rem) = cfg in match znz with
                          | "z"  -> (match st with
                                    | z::st' -> if z <> 0 then (eval env (st', rem) tail) else (eval env (st', rem) (env#labeled l))
                                    | []     -> failwith "CJMP with empty stack")
                          | "nz" -> (match st with
                                    | z::st' -> if z <> 0 then (eval env (st', rem) (env#labeled l)) else (eval env (st', rem) tail)
                                    | []     -> failwith "CJMP with empty stack"))
    | _              -> eval env (instrEval cfg instr) tail)
  | []          -> cfg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
   *)

let labelGen = object 
   val mutable freeLabel = 0
   method get = freeLabel <- freeLabel + 1; "L" ^ string_of_int freeLabel 
end

(*let lastAndRem l =  match (List.rev l) with 
  | last::revRem -> (last, List.rev revRem)
  | last::[]     -> (last, [])

let rec compileIfStmt stmt finalLabel =
  let (sLast, rem) = lastAndRem stmt in
  let compiled = (match sLast with
    | Stmt.If (e', s1', s2') -> 
        let lElse = labelGen#get in
        let compiledRem = (match rem with
          | [] -> []
	  | _  -> compile rem) in
        compiledRem @ expr e' @ [CJMP ("z", lElse)] 
          @ compileIfStmt s1' finalLabel @ [JMP finalLabel] 
          @ [LABEL lElse] @ compileIfStmt s2' finalLabel @ [LABEL finalLabel]
    | _ -> compile stmt) in
    compiled*)

let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
  | Stmt.If (e, s1, s2) ->
    let lElse = labelGen#get in
    let lFi = labelGen#get in
    expr e @ [CJMP ("z", lElse)] @ compile s1 @ [JMP lFi] @ [LABEL lElse] @ compile s2 @ [LABEL lFi]
  | Stmt.While (e, body) ->
    let lCheck = labelGen#get in
    let lLoop = labelGen#get in
    [JMP lCheck; LABEL lLoop] @ compile body @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]
  | Stmt.RepeatUntil (body, e) ->
    let lLoop = labelGen#get in
    [LABEL lLoop] @ compile body @ expr e @ [CJMP ("z", lLoop)]
  | Stmt.Skip -> []
  | _ -> failwith "Not impl"

