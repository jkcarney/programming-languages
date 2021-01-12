(* Author:      Joshua Carney
 * Instructor:  Dr Killian
 * Date:        9/3/2020
 * Assignment:  Lab 2 - Tail Recursion
 * Description: Writing various functions using tail recursion
 *)
(* CSCI 330: Lab 2
 * misc.ml
 *)

(* 
 *	sqrt: accepts a tolerance (a small number) and a number to find the square
 *				root of, and returns the square root within the tolerance
 *
 *	val sqrt : float -> float -> float
 *)
let rec sqrt tol x =
	let rec sqrtHelper tol x guess = 
		if (abs_float (guess *. guess -. x) <= (abs_float tol)) then
			guess
		else
			sqrtHelper tol x ((guess +. x /. guess) /. 2.0)
	in sqrtHelper tol x x
;;

(* 
 *	sqrt2: returns the square root of a number x with a tolerance of 0.00001.
 *
 *	val sqrt : float -> float
 *)
let rec sqrt2 x =
	sqrt 0.00001 x
;;

(*
 * 	factorial1: A simple recursive factorial function that is implemented with an if-else statement
 *						 Returns the factorial of a number x, mathmatically represented as x!
 *
 *	val factorial1 : int -> int
 *)
let rec factorial1 x =
	if x = 0 then
		1
	else
		x * factorial1 (x - 1)
;;

(*
 * 	factorial2: A simple recursive factorial function that is implemented with a match case. 
 *						 Returns the factorial of a number x, mathmatically represented as x!
 *
 *	val factorial2 : int -> int
 *)
let rec factorial2 x = 
	match x with
	| 0 -> 1
	| _ -> x * factorial2 (x - 1)
;;

(*
 * 	factorial3: A tail-recursive factorial function.
 *						  Returns the factorial of a number x, mathmatically represented as x!
 *
 *	val factorial3 : int -> int
 *)
let rec factorial3 x =
	let rec factorialHelper multiplier x =
		match x with
		| 0 -> multiplier
		| _ -> factorialHelper (x * multiplier) (x - 1)
	in factorialHelper 1 x
;;

(*	
 *	fibbonacci: A tail-recursive tool that will find the fibonacci number in the 
 *							sequence at x, where x is zero based.
 *
 *							The function begins to break down beyond a certain integer. Large numbers
 *							give the incorrect answer (for example 200 gives a negative number), 
 *							likely because of integer overflow since the max integer in OCaml is around 4.6 x 10^18
 *	val fibonacci : int -> int
 *)
let rec fibonacci x = 
	let rec fibHelper x a b =
		match x with
		| 0 -> 0;
		| 1 -> b
		| _ -> fibHelper (x-1) (a+b) a
	in fibHelper x 1 1
;;
(*
 *	rev: A tail recursive function that reverses a given list l.
 *		
 *	val rev : 'a list -> 'a list
 *)
let rec rev l = 
	let rec reverseHelper l e = 
		match l with
		| [] -> e
		| head::tail -> reverseHelper tail (head::e)
	in reverseHelper l []
;;

(*
 *	map: Takes a function and a list of values and returns a new list with 
 *			 the function applied to each member
 *
 *	val map : ('a -> 'b) -> 'a list -> 'b list
 *)
let rec map f l = 
	match List.length l with
	| 0 -> []
	| _ -> [f (List.hd l);] @ map f (List.tl l)
;;

(*
 *	map2: Takes a function and a list of values and returns a new list with 
 *			  the function applied to each member. map2 is implemented tail-recustively. 
 *
 *	val map : ('a -> 'b) -> 'a list -> 'b list
 *)
let rec map2 f l = 
	let rec mapHelper f l mappedList = 
		match l with
		| [] -> mappedList
		| head::tail -> mapHelper f tail ((f head)::mappedList)
	in mapHelper f (rev l) []
;;

(*
 *	range: Takes two integers and returns a list of all integers in that range.
 *				 The list is inclusive to to BOTH arguements.
 *				 If b is less than a, an empty list is returned.
 *
 *	val range : int -> int -> int list
 *)
let rec range a b = 
	let rec rangeHelper a b l =
		if b < a then
			l
		else
			rangeHelper a (b - 1) (b::l)
	in rangeHelper a b []
;;

(*
 * 	roots: value of the square roots of integers 1-20
 *
 * 	val roots : float list
 *)
let roots : float list = map (sqrt2) (map float_of_int (range 1 20))
