(*
    Functional Programming - Assignment 5
    Sam Al-Sapti (sals@itu.dk)
    March 8nd, 2022
*)


module Programfs

(*
    GREEN
*)

// Exercise 5.1
let sum m n =
    let rec aux acc =
        function
        | 0 -> acc
        | n' -> aux (acc + m + n') (n' - 1)

    aux m n

// Exercise 5.2
let length lst =
    let rec aux acc =
        function
        | [] -> acc
        | _ :: lst' -> aux (acc + 1) lst'
    
    aux 0 lst

// Exercise 5.3
let rec foldBack (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) =

    let rec aux (lst': 'a list) (acc': 'b) (c: 'b -> 'b) =
        match lst' with
        | [] -> c acc'
        | x :: xs -> aux xs (f x acc') (fun r -> c r)

    aux lst acc (f lst.Head)

// f x (foldBack f xs cont)

// Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

(* TODO: *)
(* Compare the running time between factA and factC. Which solution is faster and why?
   <Your answer goes here>
*)

(* Exercise 5.5 *)

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1

    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1

    res1

(* TODO: *)
(* Compare the running time of fibW, fibA and fibC
   <Your answer goes here>

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow.
   Analyse the problem and describe exactly why this happens.
   Why is this not an iterative function?

   <Your answer goes here>
*)

(* Exercise 5.7 *)

type word = (char * int) list

type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)

and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)

let arithEvalTail a w s cont = failwith "not implemented"

let charEvalTail c w s cont = failwith "not implemented"

let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id
