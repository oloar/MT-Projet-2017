
(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * provides [fresh integers] using object 
 *
 * - The simplest example using object-oriented features of Ocaml
 *
 * - Usage:
 *
 *     let id = new Fresh.counter 0 ; 
 *     id#next ;
 *     id#next ;
 * 
 * - Required:
 *
 * - Compilation: ocamlc Fresh.ml
 *
 *)

  
class counter = fun (initial_value:int) ->
      object
	val mutable counter: int = (initial_value - 1)
	    
	method next: int = 
	  begin
	    counter <- counter + 1 ;
	    counter
	  end
      end
    

(* DEMO
 *
 *  1. compile the module
 *  
 *    ocamlc Fresh.ml
 *
 *  2. launch the ocaml interpreter and load the module
 *
 *    ocaml Fresh.cmo
 * 
 *  3. copy the Use case provided 
 *
 *)    


