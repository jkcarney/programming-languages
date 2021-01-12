(* Author:      Joshua Carney
 * Instructor:  
 * Date:        9/3/2020
 * Assignment:  Lab 1 - Introduction to OCaml
 * Description: A lab introduction to various features about OCaml. Also recursion.
 *)

(* BEGIN PROVIDED FUNCTIONS *)

(* explode : string -> char list
 * (explode s) is the list of characters in the string s in the order in
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s =
        let rec _exp i =
                if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in _exp 0;;

(* END PROVIDED FUNCTIONS *)


(* For ALL of the following method stubs (those with failwith "to be written"),
   add documentation comments including expected behavior *)
   
(*
       sumList: returns the sum of elements in a list of integers (l).
                for example sumList[1;2;3] will return 6
        
        Tested with commands "sumList[1;2;3]", "sumList[1;]", "sumList[]", and sumList[5;67;78;4;3;5;65;67;7;5;54;34;4;5;6;]
 *)
let rec sumList l =         
        match (List.length l) with
        | (0) -> 0
        | _ -> List.hd l + sumList (List.tl l)
;;

(*
        digitsOfInt: accepting a positive integer (n) (strictly > 0), returns a list representation of all digits.
                     for example, digitsOfInt 2451 will return [2;4;5;1]

        Tested with commands "digitsOfInt 12345", "digitsOfInt 54321", "digitsOfInt 5050", "digitsOfInt 1", "digitsOfInt 0"
*)
let rec digitsOfInt n =  
        match (n / 10) with
        |(0) -> [n mod 10]
        |_ -> digitsOfInt(n / 10) @ [n mod 10;]  
;;

(*
        additivePersistance: accepts an integer (n), computes the additive persistence of the number. For example, 9876 would have an A.P of 2.

        Tested with commands "addtivePersistence 9876","additivePersistence 30","additivePersistence 1"
*)
let rec additivePersistence n = 
        if n < 10 then 0 else 
                let l = digitsOfInt n in
                let sum = sumList l in
                match (List.length (digitsOfInt sum)) with
                | 1 -> 1
                | _ -> 1 + additivePersistence(sum)
;;

(*
        digitalRoot: accepts an integer (n) and computes it's digital root. For example, the digital root of 9876 is 3.

        Tested with commands "digitalRoot 9876", "digitalRoot 9", "digitalRoot 0", "digitalRoot 98769576896766"
*)
let rec digitalRoot n = 
        let l = digitsOfInt n in
        let sum = sumList l in
        match (List.length (digitsOfInt sum)) with
        | (1) -> sum
        | _ -> digitalRoot (sum)

;;

(*
        listReverse: accepts a list (l) and returns a new list with the order reversed

        Tested with commands "listReverse[1;2;3;4;5;]", "listReverse["burger_king";"skeetskirt";"yeehaw";]"
*)
let rec listReverse l = 
        match List.length l with
        |(0) -> []
        |(1) -> [List.hd l;]
        |_ ->  listReverse(List.tl l) @ [List.hd l;]
;;

(*
        palindrome: accepts a string (w) and checks to see if it is a palidrome (spelled the same forward and backwards)
        TRUE if w is a palidrome
        FALSE if w is not a palidrome

        Tested with commands "palindrome racecar", "palindrome tacocat", "palindrome RaCecar"
*)
let palindrome w = 
        explode w = listReverse (explode w)
;;

(* BEGIN PROVIDED FUNCTIONS *)

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
let digits n = digitsOfInt (abs n);;

(* END PROVIDED FUNCTIONS *)

(************** Add Testing Code Here ***************)