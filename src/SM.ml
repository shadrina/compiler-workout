open GT       
open Language
       
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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

let instrEval (controlSt, st, (s, i, o)) instruction = match instruction with
  | BINOP op -> (match st with
    | y :: x :: tail -> (controlSt, (Expr.to_func op x y) :: tail, (s, i, o))
    | _              -> failwith "Not enough elements in stack")
  | CONST z  -> (controlSt, z :: st, (s, i, o))
  | READ     -> (match i with
    | z :: tail -> (controlSt, z :: st, (s, tail, o))
    | _         -> failwith "Not enough elements in input")
  | WRITE    -> (match st with
    | z :: tail -> (controlSt, tail, (s, i, o @ [z]))
    | _         -> failwith "Not enough elements in stack")
  | LD x     -> (controlSt, (State.eval s x) :: st, (s, i, o))
  | ST x     -> (match st with
    | z :: tail -> (controlSt, tail, (State.update x z s, i, o))
    | _         -> failwith "Not enough elements in stack")
  | LABEL l  ->  (controlSt, st, (s, i, o))

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let rec eval env cfg p = 
  match p with
    | instr::tail -> (match instr with
      | JMP l          -> eval env cfg (env#labeled l)
      | CJMP (znz, l)  -> (let (controlSt, st, rem) = cfg in match znz with
                            | "z"  -> (match st with
                                      | z::st' -> if z <> 0 then (eval env (controlSt, st', rem) tail) else (eval env (controlSt, st', rem) (env#labeled l))
                                      | []     -> failwith "CJMP with empty stack")
                            | "nz" -> (match st with
                                      | z::st' -> if z <> 0 then (eval env (controlSt, st', rem) (env#labeled l)) else (eval env (controlSt, st', rem) tail)
                                      | []     -> failwith "CJMP with empty stack"))
      | CALL (l, _, _) -> let (controlSt, st, (s, i, o)) = cfg in eval env ((tail, s) :: controlSt, st, (s, i, o)) (env#labeled l)
      | RET _ | END    -> let (controlSt, st, (s, i, o)) = cfg in (match controlSt with
                                                                    | (p', s')::cStTail -> let s'' = State.leave s s' in eval env (cStTail, st, (s'', i, o)) p'
                                                                    | _                 -> cfg
                                                                  )
      | BEGIN (_, params, locals) -> let (controlSt, st, (s, i, o)) = cfg in
                                  let s' = State.enter s (params @ locals) in 
                                  let calcParams = List.map (fun p -> ST p) params in
                                  let cfg' = eval env (controlSt, st, (s', i, o)) calcParams in
                                  eval env cfg' tail
      | _                      -> eval env (instrEval cfg instr) tail)
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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let labelGen = object 
   val mutable freeLabel = 0
   method get = freeLabel <- freeLabel + 1; "L" ^ string_of_int freeLabel
end

let rec compileWithLabels p lastL =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (fName, argsE) -> let compiledArgs = List.flatten (List.map (expr) (List.rev argsE)) in
                                compiledArgs @ [CALL (fName, List.length argsE, true)]
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = labelGen#get in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Read x        -> [READ; ST x], false
  | Stmt.Write e       -> (expr e @ [WRITE]), false
  | Stmt.Assign (x, e) -> (expr e @ [ST x]), false
  | Stmt.If (e, s1, s2) ->
    let lElse = labelGen#get in
    let (compiledS1, used1) = compileWithLabels s1 lastL in
    let (compiledS2, used2) = compileWithLabels s2 lastL in 
    (expr e @ [CJMP ("z", lElse)] 
    @ compiledS1 @ (if used1 then [] else [JMP lastL]) @ [LABEL lElse]
    @ compiledS2 @ (if used2 then [] else [JMP lastL])), true
  | Stmt.While (e, body) ->
    let lCheck = labelGen#get in
    let lLoop = labelGen#get in
    let (doBody, _) = compileWithLabels body lCheck in
    ([JMP lCheck; LABEL lLoop] @ doBody @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]), false
  | Stmt.Repeat (body, e) ->
    let lLoop = labelGen#get in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false
  | Stmt.Call (fName, argsE) -> let compiledArgs = List.flatten (List.map (expr) (List.rev argsE)) in
                                compiledArgs @ [CALL (fName, List.length argsE, false)], false
  | Stmt.Return e -> (match e with
                       | Some x -> (expr x) @ [RET true]
                       | None   -> [RET false]), false

let compileP p =
  let label = labelGen#get in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else [])

let compileDefs defs =
  let compileDef (name, (params, locals, body)) = 
    (let compiledBody = compileP body in
    [LABEL name; BEGIN (name, params, locals)] @ compiledBody @ [END]) in
  List.flatten (List.map compileDef defs)

let rec compile (defs, main) =
  let compiledMain = compileP main in
  let compiledDefs = compileDefs defs in
  compiledMain @ [END] @ compiledDefs
