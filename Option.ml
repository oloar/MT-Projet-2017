(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Some operators of the Option monad
 *
 *)

(* USAGE
 * required module: 
 * compile command: ocamlc option.ml
*)

(* Recall: The option type is defined in ocaml as  
 *
 *  type 'a option = None | Some of 'a 
 *)  

  
type 'a m = 'a option

let (bind: ('a -> 'b m) -> 'a m -> 'b m) = fun fopt option ->
      match option with
      | None -> None
      | Some a -> fopt a
		
let (map: ('a -> 'b) -> 'a m -> 'b m) = fun f ->
      bind (fun a -> Some (f a))
	
	
let (filter: ('a -> bool) -> 'a m -> 'a m) = fun pred option ->
      match option with
      | Some a when (pred a) -> Some a
      | _ -> None
		
let (to_list:'a m -> 'a list) = fun option ->
      match option with
      | None -> []
      | Some a -> [a]

let (to_string: string m -> string) = fun option ->
      match option with
      | None -> ""
      | Some string -> string
   
let (binop: ('t -> 't -> 't) -> 't option -> 't option -> 't option) = fun binop optx opty ->
      match (optx, opty) with
      | Some x, Some y -> Some (binop x y)
      | Some x, None   -> Some x
      | None  , Some y -> Some y
      | None  , None   -> None

