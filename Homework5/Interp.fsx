(*
Luis Averhoff   PID: 5872592
Dane Parchment  PID: 
*)

// Skeleton file for PCF interpreter

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

let rec subst e x t =
    match e with
    | ID n -> if n = x then t else ID n
    | NUM n -> NUM n
    | BOOL b -> BOOL b
    | SUCC -> SUCC
    | PRED -> PRED
    | ISZERO -> ISZERO
    | IF (b, e1, e2) -> IF (subst b x t, subst e1 x t, subst e2 x t)
    | APP (e1, e2) -> APP (subst e1 x t, subst e2 x t)
    | FUN (s, e1) -> if x = s then FUN (s, e1) else FUN (s, subst e1 x t)
    | REC (s, e1) -> if x = s then REC (s, e1) else REC (s, subst e1 x t)
    | _ -> ERROR (sprintf "Substitution error") 

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.

let rec interp = function
| ERROR s -> ERROR s
| NUM n -> NUM n            
| BOOL b -> BOOL b          
| SUCC -> SUCC              
| PRED -> PRED              
| ISZERO -> ISZERO         
| ID s -> ID s
| FUN (x, e) -> FUN (x, e)  
| REC (x, e) -> REC (x, e) 
| IF (b,e1,e2) -> 
    match (interp b,e1,e2) with
    | (ERROR s, _, _) -> ERROR s         
    | (_, ERROR s, _) -> ERROR s
    | (_, _, ERROR s) -> ERROR s
    | (BOOL true, e1, e2) -> interp e1   
    | (BOOL false, e1, e2) -> interp e2  
    | (v, e1, e2) -> ERROR (sprintf "'if' needs boolean arg, not '%A'" v)
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s          // ERRORs propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)        // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int arg, not '%A'" v)
    | (PRED, NUM 0) -> NUM 0            
    | (PRED, NUM n) -> NUM (n-1)       
    | (PRED, v) -> ERROR (sprintf "'pred' needs int arg, not '%A'" v)
    | (ISZERO, NUM 0) -> BOOL true      
    | (ISZERO, NUM n) -> BOOL false     
    | (ISZERO, v) -> ERROR (sprintf "'iszero' needs int arg, not '%A'" v)
    | (FUN (x, e), ex) -> interp (subst e x ex)                     
    | (REC (x, f), e) -> interp (APP(subst f x (REC (x, f)), e))    
    | (s1, s2) -> APP (s1, s2)

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
