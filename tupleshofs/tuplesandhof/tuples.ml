(* Author:      Joshua Carney
 * Instructor:  Dr Killian
 * Date:        9/30/2020
 * Assignment:  Lab 3 - Tuples and Higher Order Functions
 * Description: Writing various functions involving tuples and HOFs
 *)

(* CSCI 330: OCaml Lab 3
 * misc3.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)


(*
 *	assoc: Takes a tuple of three values (d,k,l) where l is a list of key-value
 *         pairs, and finds the first key equal to k. If no such key is found, then 
 *         d is returned.
 *
 *	val assoc : 'a * 'b * ('b * 'a) list -> 'a
 *)
let rec assoc (d,k,l) = 
  match l with
  | [] -> d
  | h::t -> 
    let(key, num) = h in
    if (key = k) then
      num
    else
      assoc(d,k,t)
;;

(*
 *	removeDuplicates: Takes a list, l, where we return the list of elements of l 
 *                    except all duplicates are removed, maintaining the same order.
 *
 *	val removeDuplicates : 'a list -> 'a list
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = 
          if (not (List.mem h seen)) then 
            h::seen 
          else 
            seen 
        in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(*
 *	wwhile: Takes a tuple of (f,b) as an input and calls the function f on b that
 *          continously iterates on b until f returns a c' that is false, which. 
 *
 *	 wwhile : ('a -> 'a * bool) * 'a -> 'a
 *)
let rec wwhile (f,b) =
  let rec wwhileHelper (f,b) c =
    match c with
    | false -> b
    | true -> 
      let(b',c') = f b in
      wwhileHelper (f,b') c'
  in wwhileHelper (f,b) true
;;

(*
 *	fixpoint: repeatedly updates b with f(b) until b = f(b).
 *
 *	fixpoint : ('a -> 'a) * 'a -> 'a)
 *)
let fixpoint (f,b) = wwhile ((fun b -> (f b),(f b) != b),b)
;;

(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)
let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
