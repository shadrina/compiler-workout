open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string
(* create an S-expression          *) | SEXP    of string * int
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool
(* drops the top element off       *) | DROP
(* duplicates the top element      *) | DUP
(* swaps two top elements          *) | SWAP
(* checks the tag of S-expression  *) | TAG     of string
(* enters a scope                  *) | ENTER   of string list
(* leaves a scope                  *) | LEAVE
(* drop values from stack and jmp  *) | ZJMPDROP of string * int
with show
                                                   
(* The type for the stack machine program *)
type prg = insn list

let print_prg p = List.iter (fun i -> Printf.printf "%s\n" (show(insn) i)) p
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n
          
let rec eval env (controlSt, st, ((s, i, o) as config)) p =
  let instrEval instr = match instr with
    | BINOP op -> (match st with
      | (y::x::xs) -> (Value.of_int (MyUtils.toFunc op (Value.to_int x) (Value.to_int y))::xs, config) 
      | _ -> failwith "Not enough elements in stack")
    | CONST n  -> ((Value.of_int n)::st, config)
    | STRING str -> (Value.of_string str::st, config)
    | SEXP (t, n)-> let vs, st' = split n st in ((Value.sexp t @@ (List.rev vs))::st', config)
    | LD x       -> ((State.eval s x)::st, config)
    | ST x       -> (match st with
                      | z::tail -> (tail, ((State.update x z s), i, o))
                      | _       -> failwith "Not enough elements in stack")
    | STA (x, n) -> let ((value::ids), tail) = split (n + 1) st in
                    let s' = Stmt.update s x value (List.rev ids) in
                    (tail, (s', i, o))
    | BEGIN (_, params, locals) -> let vs, st' = split (List.length params) st in
                                   let s' = List.combine params @@ List.rev vs in
                                   (st', (List.fold_left (fun s (x, v) -> State.update x v s) (State.enter s (params @ locals)) s', i, o))
    | LABEL _                   -> (st, config)
    | DROP                      -> (List.tl st, config)
    | DUP                       -> (List.hd st :: st, config)
    | SWAP                      -> let x::y::st' = st in (y::x::st', config)
    | TAG t                     -> let x::st' = st in
                                   let res = match x with
                                     | Value.Sexp (t', _) -> MyUtils.bti (t = t') 
                                     | _                  -> 0 in
                                   ((Value.of_int res)::st', config)
    | ENTER xs                  -> let vs, st' = split (List.length xs) st in
                                   (st', (State.push s (List.fold_left (fun s' (x, v) -> State.bind x v s') State.undefined (List.combine xs vs)) xs, i, o))
    | LEAVE                     -> (st, (State.drop s, i, o)) 
    in match p with
       | x::xs -> (match x with 
        | JMP l -> eval env (controlSt, st, config) (env#labeled l)
        | CJMP (znz, l) -> (match st with
                            | head::tail -> let v = (Value.to_int head) in
                                              if (znz = "z" && v == 0) || (znz = "nz" && v <> 0) then eval env (controlSt, tail, config) (env#labeled l)
                                              else eval env (controlSt, tail, config) xs 
                              | _          -> failwith "CJMP failed with empty stack"
                           )
        | ZJMPDROP (l, d) -> let z::st' = st in
                             if Value.to_int z = 0 then let (_,  st'') = split d st' in
                                                        eval env (controlSt, st'', config) (env#labeled l)
                             else eval env (controlSt, st', config) xs
        | CALL (f, n, fl) -> if env#is_label f then eval env ((xs, s)::controlSt, st, config) (env#labeled f)
                             else eval env (env#builtin (controlSt, st, config) f n fl) xs
        | RET _ | END     -> (match controlSt with
                                | (p, oldS)::controlSt' -> let s' = State.leave s oldS in
                                                       eval env (controlSt', st, (s', i, o)) p
                                | _                     -> (controlSt, st, config)
                             )
        | _               -> let (st, config) = instrEval x in eval env (controlSt, st, config) xs)
      | _ -> (controlSt, st, config)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           (*Printf.printf "Builtin:\n";*)
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let labelGen = object 
   val mutable freeLabel = 0
   method get = freeLabel <- freeLabel + 1; "L" ^ string_of_int freeLabel
  end

(* Assume that on the top of the stack there is already a duplicated version of element *)
let rec matchPattern lFalse depth = function
  | Stmt.Pattern.Wildcard | Stmt.Pattern.Ident _ -> false, [DROP]
  | Stmt.Pattern.Sexp (t, ps) -> true, [TAG t] @ [ZJMPDROP (lFalse, depth)] @ (
                                   List.flatten @@
                                     List.mapi (fun i p -> [DUP; CONST i; CALL (".elem", 2, false)] @
                                                             (match p with
                                                              | Stmt.Pattern.Sexp (_, ps') -> (if List.length ps' > 0 then [DUP] @ snd (matchPattern lFalse (depth + 1) p) @ [DROP]
                                                                                               else snd (matchPattern lFalse depth p))
                                                              | _ -> snd (matchPattern lFalse depth p))
                                               ) ps)

(* Idea taken from #513 *)
let rec makeBindings p =
  let rec inner p' = match p' with
    | Stmt.Pattern.Wildcard -> []
    | Stmt.Pattern.Ident var -> [[]]
    | Stmt.Pattern.Sexp (_, xs) ->
       let next i x = List.map (fun arr -> i::arr) (inner x) in
       List.flatten (List.mapi next xs) in
  let topElem i = [CONST i; CALL (".elem", 2, false)] in
  let extractBindValue path = [DUP] @ (List.flatten (List.map topElem path)) @ [SWAP] in
  List.flatten (List.map extractBindValue (List.rev (inner p)))
  
let rec compileWithLabels p lastL =
  let rec call f args fl =
    let argsCode = List.flatten @@ List.map expr args in
    argsCode @ [CALL (f, List.length args, fl)]
  and expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.String s         -> [STRING s]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Array xs         -> let compiledXs = List.flatten (List.map expr xs) in
                             compiledXs @ [CALL (".array", (List.length compiledXs), false)]
  | Expr.Sexp (t, xs)     -> let compiledXs = List.flatten (List.map expr xs) in
                             compiledXs @ [SEXP (t, List.length xs)]
  | Expr.Elem (a, i)      -> expr a @ expr i @ [CALL (".elem", 2, false)]
  | Expr.Length n         -> expr n @ [CALL (".length", 1, false)]
  | Expr.Call (fName, argsE) -> call fName argsE false
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = labelGen#get in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Assign (x, [], e) -> (expr e @ [ST x]), false
  | Stmt.Assign (x, is, e) -> List.flatten (List.map expr (is @ [e])) @ [STA (x, List.length is)], false
  | Stmt.If (e, s1, s2)    -> let lElse = labelGen#get in
                              let (compiledS1, used1) = compileWithLabels s1 lastL in
                              let (compiledS2, used2) = compileWithLabels s2 lastL in 
                              (expr e @ [CJMP ("z", lElse)] 
                                @ compiledS1 @ (if used1 then [] else [JMP lastL]) @ [LABEL lElse]
                                @ compiledS2 @ (if used2 then [] else [JMP lastL])), true
  | Stmt.While (e, body)   -> let lCheck = labelGen#get in
                              let lLoop = labelGen#get in
                              let (doBody, _) = compileWithLabels body lCheck in
                              ([JMP lCheck; LABEL lLoop] @ doBody @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]), false
  | Stmt.Repeat (body, e)  -> let lLoop = labelGen#get in
                              let (repeatBody, _) = compileWithLabels body lastL in
                              ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Call (fName, argsE) -> call fName argsE true, false
  | Stmt.Return e            -> (match e with
                                   | Some x -> (expr x) @ [RET true]
                                   | None   -> [RET false]
                                ), false
  | Stmt.Leave               -> [LEAVE], false
  | Stmt.Case (e, [p, s])    -> (* TODO: Reverse matchPattern return tuple *)
                                let pUsed, pCode = matchPattern lastL 0 p in
                                let sBody, sUsed = compileWithLabels (Stmt.Seq (s, Stmt.Leave)) lastL in
                                expr e @ [DUP] @ pCode @ makeBindings p @ [DROP; ENTER (List.rev (Stmt.Pattern.vars p))] @ sBody, pUsed || sUsed
  | Stmt.Case (e, bs)        -> let n = List.length bs - 1 in
                                let _, _, code = List.fold_left
                                                   (fun (l, i, code) (p, s) ->
                                                     let lFalse, jmp = if i = n then lastL, []
                                                                       else labelGen#get, [JMP lastL] in
                                                     let _, pCode = matchPattern lFalse 0 p in
                                                     let sBody, _ = compileWithLabels (Stmt.Seq (s, Stmt.Leave)) lastL in
                                                     let amLabel = match l with Some x -> [LABEL x; DUP] | None -> [] in
                                                     (Some lFalse, i + 1, (amLabel @ pCode @ makeBindings p @ [DROP; ENTER (List.rev (Stmt.Pattern.vars p))] @ sBody @ jmp) :: code)
                                                   ) (None, 0, []) bs in
                                expr e @ [DUP] @ List.flatten @@ List.rev code, true
  | Stmt.Skip                -> [], false

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
