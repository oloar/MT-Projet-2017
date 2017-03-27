(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 *  Log computations in a file
 *
 * - Usage:
 *
 *   let log = new logger (String.concat "_" [ Date.pretty_time() ; "file_name.ext" ]
 *   in begin log#print "string1" ; log#print "string2" ; log#close end
 *
 * - Required modules: Unix.cma -> Date.ml
 *
 * - Compilation: ocamlc Unix.cma Date.ml Tracing.ml 
 * 
 *)


(* DELAYING A COMPUTATION
 *
 *   Consider the computation (1+2) of type: int 
 *  then  (fun _ -> (1+2))  is a delayed computation of type: unit -> int 
 *)
type 'a delayed_computation = unit -> 'a      
(*
 *  It can be triggered later by calling it with the value () of type unit.
 *  Indeed,  (fun _ -> (1+2)) ()  beta-reduces to   (1+2)
 *)

type filename = string


module Indent =
  (struct
    
    let _INDENTATION_ = ref 0
	
    let (incr: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) + 2
    let (decr: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) - 2	
	
    let (newline: string -> string) = fun string ->
	  String.concat "" ["\n" ; String.make (!_INDENTATION_) ' ' ; string ]
  end)
    



(* LOGGER as object to allow multiple active logs in different files *)

   
class logger = fun (opt_name: string option) ->
      object(self)

	val opt_filename: string option =
	  match opt_name with
	  | None -> None
	  | Some name ->
		  let path = "_log"
		  in  Some (String.concat "/"  [ path ; name ]) 

	val mutable channel: out_channel = stdout

	initializer
	  channel <-
	    (match opt_filename with
	    | None -> stdout
	    | Some filename -> open_out filename
	    )

	method print: string -> unit = fun string ->
	      output_string channel string
		  
	method newline: string -> unit = fun string -> self#print (Indent.newline string)
		
	method print_msg: string -> unit = fun string ->
	      print_string string 

	method close: unit =
	  begin
	    (match opt_filename with
	    | None -> ()
	    | Some filename -> self#print_msg ("\n\n.... data logged in: " ^ filename ^ "\n")
	    ) ;
	    close_out channel
	  end

      end


type t = logger

