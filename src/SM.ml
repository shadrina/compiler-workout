open GT
open Syntax
open List

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)

let instrEval (stack, (s, i, o)) instruction = match instruction with
    | BINOP op -> (match stack with
        | y :: x :: tail -> ((Syntax.Expr.binopEval op x y) :: tail, (s, i, o))
        | _              -> failwith "Not enough elements in stack")
    | CONST z -> (z :: stack, (s, i, o))
    | READ    -> (match i with
        | z :: tail -> (z :: stack, (s, tail, o))
        | _         -> failwith "Not enough elements in input")
    | WRITE   -> (match stack with
        | z :: tail -> (tail, (s, i, o @ [z]))
        | _         -> failwith "Not enough elements in stack")
    | LD x    -> ((s x) :: stack, (s, i, o))
    | ST x    -> (match stack with
        | z :: tail -> (tail, (Syntax.Expr.update x z s, i, o))
        | _         -> failwith "Not enough elements in stack")

let eval cfg p = List.fold_left instrEval cfg p 

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt =
    let rec compileExpr expr = match expr with
        | Syntax.Expr.Const c            -> [CONST c]
        | Syntax.Expr.Var x              -> [LD x]
        | Syntax.Expr.Binop (o, x, y)    -> (compileExpr x) @ (compileExpr y) @ [BINOP o]
    in match stmt with
        | Syntax.Stmt.Read x             -> [READ; ST x]
        | Syntax.Stmt.Write e            -> (compileExpr e) @ [WRITE]
        | Syntax.Stmt.Assign (x, e)      -> (compileExpr e) @ [ST x]
        | Syntax.Stmt.Seq (stmt1, stmt2) -> (compile stmt1) @ (compile stmt2)
