(* 
 * Name: YOUR NAME HERE
 * Date: YOUR LAST DATE MODIFIED HERE
 * Course: CSCI 330 - Programming Languages
 * Assignment: Variant Types
 *
 * Assignment Attribution:
 *   This lab is based on code by Chris Stone (lab from CSE 130 by Sorin Lerner at UCSD)
 *
 * Description: YOUR DESCRIPTION HERE
 *)

let pi = 4.0 *. (atan 1.0)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | Multiply3 of expr * expr  * expr
  | AbsSqrt     of expr
(* TODO: add two new "types" of expressions *)

type rng = int * int -> int
type builder_fun = rng * int -> expr

let rec exprToString e =
  match e with 
  | VarX -> "x"
  | VarY -> "y"
  | Sine e' -> "sin(pi*" ^ exprToString(e') ^ ")"
  | Cosine e' -> "cos(pi*" ^ exprToString(e') ^ ")"
  | Average (e1,e2) -> "(("^(exprToString e1) ^ "+" ^ (exprToString e2) ^ ")/2)"
  | Times (e1,e2) -> exprToString e1 ^ "*" ^ exprToString e2
  | Thresh(a,b,a_less,b_less) -> 
      exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString a_less ^ ":" ^ exprToString b_less
  | Multiply3(e1,e2,e3) -> exprToString e1 ^ "*" ^ exprToString e2 ^ "*" ^ exprToString e3 
  | AbsSqrt(e') -> "sqrt(abs(" ^ exprToString e' ^ "))"
;;
(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildMultiply3(e1,e2,e3)       = Multiply3(e1,e2,e3)
let buildAbsSqrt(e)                = AbsSqrt(e)

(* TODO: add two new buildXXXXXXX functions *)


let rec eval (e, x, y) =
  match e with 
  | VarX -> x
  | VarY -> y
  | Sine e' -> sin (pi *. eval (e',x,y))
  | Cosine e' -> cos (pi *. eval (e',x,y))
  | Average (e1,e2) -> ((eval(e1,x,y) +. eval(e2,x,y)) /. 2.0)
  | Times (e1,e2) -> (eval (e1,x,y)) *. (eval (e2,x,y))
  | Thresh(a,b,a_less,b_less) ->
      if a < b then (eval (a_less,x,y)) else (eval (b_less,x,y))
  | Multiply3(e1,e2,e3) -> (eval (e1,x,y)) *. (eval (e2,x,y) *. (eval (e3,x,y))) 
  | AbsSqrt(e) -> sqrt (abs_float (eval (e,x,y)))
;;
(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv
;;
let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))




(******************* Functions you need to write **********)

(* build: (int*int->int) * int -> Expr 
   Build an expression tree.  The second argument is the depth, 
   the first is a random function.  A call to rand(2,5) will give
   you a random number in the range [2,5)  
   (2 inclusive, and 5 exclusive).

   Your code should call buildX, buildSine, etc. to construct
   the expression.
*)
[@@@ocaml.warning "-8"]
let rec build (rand,depth) = 
  if depth == 0 then
    match rand(0,2) with
    | 0 -> buildX()
    | 1 -> buildY()
  else
    match rand(0, 5) with
    | 0 -> buildSine(build (rand,depth - 1))
    | 1 -> buildCosine(build (rand,depth - 1))
    | 2 -> buildAverage(build (rand,depth - 1), build (rand,depth - 1))
    | 3 -> buildTimes(build (rand,depth - 1), build (rand,depth - 1))
    | 4 -> buildThresh(build (rand,depth - 1), build (rand,depth - 1), build (rand,depth - 1), build (rand,depth - 1))
;;
let rec build2 (rand,depth) = 
  if depth == 0 then
    match rand(0,2) with
    | 0 -> buildX()
    | 1 -> buildY()
  else
    match rand(0, 7) with
    | 0 -> buildSine(build2 (rand,depth - 1))
    | 1 -> buildCosine(build2 (rand,depth - 1))
    | 2 -> buildAverage(build2 (rand,depth - 1), build2 (rand,depth - 1))
    | 3 -> buildTimes(build2 (rand,depth - 1), build2 (rand,depth - 1))
    | 4 -> buildThresh(build2 (rand,depth - 1), build2 (rand,depth - 1), build2 (rand,depth - 1), build2 (rand,depth - 1))
    | 5 -> buildMultiply3(build2 (rand,depth - 1), build2 (rand,depth - 1), build2 (rand,depth - 1))
    | 6 -> buildAbsSqrt(build2 (rand,depth - 1))
;;
(* g1,c1 : unit -> ((int*int->int) * int -> Expr) * int * int * int
 * these functions should return the parameters needed to create your 
 * top color / grayscale pictures.
 * they should return (function,depth,seed1,seed2)
 * Function should be build or build2 (whichever you used to create
 * the image)
 *)

let g1 () = failwith "to be implemented"  

let c1 () = failwith "to be implemented"