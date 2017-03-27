(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 *  Some tricks
 *
 *)

(* USAGE
 * required module: 
 * compile command: ocamlc Tricks.ml
*)


(* TRICK 1. apply operators sequentialy.
 *  Is now available in caml as |>
 *)

let (pipe: 'a -> ('a -> 'b) -> 'b) = fun data f -> f data 

let (>>) = pipe ;;

(* Example:
   let _ = 1 >> ((+) 1) >> ((/) 2) >> ((+) 3) ;; 
 *)

   
(* TRICK 2. the function composition but in the sequential order (not the mathematical one)
 * - It can be done using >> 
 *)

let (sequential_composition: ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)) = fun f g ->
      (fun a -> g (f a)) 
      
let (-->) = sequential_composition ;;

(* Example: 
let _ =  let op = ((+) 1) --> ((/) 2) --> ((+) 3)
         in op 1 ;;

let _ =  let op x = fun x -> x >> ((+) 1) >> ((/) 2) >> ((+) 3)
         in op 1 ;;
*)

   
(* TRICK 3. Cast to unit *)
    
let (unit: 'a -> unit) = fun _ -> () 

(* Example: 
   let (inc: int -> int) = fun i ->
	 begin
	   print_string (string_of_int i) ;
	   i+1
	 end
   in
     begin
       unit(inc 0) ; 
       inc 1 ;
     end 
*)


(* TRICK 4. Delayed computation *)

type 'a delayed_computation = unit -> 'a
    
let (force: 'a delayed_computation -> 'a) = fun delayed_computation ->
      delayed_computation ()

(* see Tracing.ml for a actual example of usage *)

