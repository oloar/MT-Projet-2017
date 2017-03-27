(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project: TURING MACHINES FOR REAL
 *
 *  A Turing Machine that performs beta reduction of a lambda term 
 *
 *)


  
open Lambda_Calcul
open Symbol  

(* the Symbol module provides symbols for simulating the beta-reducton with MT 
 *
 *  type symbol = ...
 * 
 *   | L : lambda 
 *   | O : the opening parenthesis 
 *   | C : the closing parenthesis 
 *   | X : the variable symbol followed by a identifier as a sequence of bits 
 *     
 *   | V of (string * int) : /!\ a infinite number of symbols: This is cheating, but convenient for debugging
 *)      
   
type bit = B0 | B1


module Convert =
  (struct

    type t = Lambda_Term.t

    (* CONVERTION FROM lambda term TO list of symbols *)	  
	  
    let rec (int_to_bits: int -> symbol list) = 
      function
	| 0 -> [Z]
	| 1 -> [U]
	| i -> let r = i mod 2 in  (if r=0 then Z else U) :: (int_to_bits ((i-r)/2))


    let (bits_to_int: bit list -> int option) = fun bits ->
	  let rec (rec_bits_to_int: int -> bit list -> int) = fun i ->
		function
		  | []       -> i
		  | B0::bits -> rec_bits_to_int (i*2) bits
		  | B1::bits -> rec_bits_to_int ((i*2)+1) bits
	  in
	    match bits with
	    | [] -> None
	    | _  -> Some (rec_bits_to_int 0 bits) 
	    

    let rec (* USER *) (to_symbols: t -> symbol list) = fun lambda_term ->
	  match lambda_term with
	  | Lambda(var,expr) ->
		  L:: (List.concat [ to_symbols var ; to_symbols expr ])
	  | Apply exprs ->
		  O :: (List.concat (List.map (fun expr -> (to_symbols expr)) exprs)) @ [C]

         (* version lisible pour la mise au point											   
	  | Var (str,int) -> [ V(str,int) ]
	  *)

         (* version minimaliste *)		    
	  | Var (_,int) -> X :: (int_to_bits int)
			


    (* CONVERTION FROM list of symbols TO lambda term *)	  
		    
    let rec (parse: symbols -> t option * symbols) = fun symbols ->
	  match symbols with
	  | []   
	  | C::_ -> (None, symbols)
	  | O::symbols ->
		  let (args,symbols) = parse_args [] symbols in
		    (Some (Apply args), symbols)
		      
	  | L::symbols ->
		  let (opt_var, symbols) = parse symbols in
		    (match opt_var with
		    | None -> (None,symbols)
		    | Some (Var _ as var)  ->
			    let (opt_expr,symbols) = parse symbols in
			      (match opt_expr with
			      | None -> (None, symbols)
			      | Some expr -> (Some(Lambda(var,expr)) , symbols)
			      )
		    )

         (* version lisible
	  | V(str,int)::symbols -> (Some(Var(str,int)), symbols)
	  *)
		      
         (* version minimaliste *)
	  | X::symbols ->
		  let (opt_int,symbols) = parse_bits [] symbols in
		    (match opt_int with
		    | None     -> (None, symbols)
		    | Some int -> (Some(Var ("x",int)), symbols)
		    )
		    
     and (parse_args: t list -> symbols -> t list * symbols) = fun args symbols ->
	   match symbols with
	   | []         -> (args, symbols)
	   | C::symbols -> (args, symbols)
	   | _  -> 
		   let (opt_arg,symbols) = parse symbols in
		     match opt_arg with
		     | None -> ([],symbols) 
		     | Some arg -> parse_args (args @ [arg]) symbols

     and (parse_bits: bit list -> symbols -> int option * symbols ) = fun bits ->
	   function
	     | Z::symbols -> parse_bits (B0::bits) symbols
	     | U::symbols -> parse_bits (B1::bits) symbols
	     | symbols -> (bits_to_int bits, symbols)


		       
    let (* USER *) (from_symbols: symbols -> t option) = fun symbols ->
	  match parse symbols with
	  | Some t, [] -> Some t
	  | _     , _  -> None


    (* VERIFICATION *)
		    
    let (check_correctness_on: t -> unit) = fun t ->
	  assert ((Some t) = from_symbols (to_symbols t))

    let (show_translation_on: t -> symbols * t option) = fun t ->
	  let symbols = to_symbols t 
	  in (symbols, from_symbols symbols)
      
  end)

(* TEST *)

open Convert
let _ = List.iter check_correctness_on Lambda_Term.examples ;;
let _ = show_translation_on Lambda_Term.example1 ;;



    
(* BETA REDUCTION performed by a TURING MACHINE *)
    
module Reduce =
  (struct

    open Band
    open Alphabet
    open Turing_Machine
    open Configuration
    open Execution
      
    let (alphabet: Alphabet.t) = Alphabet.make [B;Z;U;L;O;C;X]

    let (band_from: Lambda_Term.t -> Band.t) = fun lambda_term ->
	  Band.make alphabet (Convert.to_symbols lambda_term)


    let (a_turing_machine_that_beta_reduces_lambda_term: Turing_Machine.t) =
      (* TODO *) (* PROJET 2017: modifiez ce code -> *) Turing_Machine.nop  
	
    let (reduce_by_turing_machine: Band.t -> Band.t) = fun band ->
	  let cfg = Configuration.make a_turing_machine_that_beta_reduces_lambda_term [band]
	  in let final_cfg = Execution.log_run cfg
	  in List.hd final_cfg.bands

    open Tricks
      
    let (is_reduce_correct_on: Lambda_Term.t -> bool) = fun lambda_term ->
	  let reduced_term1 = Lambda_Term.reduce lambda_term in
	    let band1 = band_from reduced_term1
	    and band2 = reduce_by_turing_machine (band_from lambda_term)
	    in let opt_reduced_term2 = Convert.from_symbols (Band.symbols_of band2)
	    in
	      let (equivalent:bool) =  Band.equivalent band1 band2 in
		begin
		  [ "" ;
		    Lambda_Term.pretty lambda_term ; "reduces to" ;
		    Lambda_Term.pretty reduced_term1 ; "encoded as" ;
		    Band.to_ascii band1 ; if equivalent then (Pretty.ascii_green "====") else (Pretty.ascii_red "==") ^ (Pretty.ascii_bold " NOT ") ^ (Pretty.ascii_red "==") ;
		    Band.to_ascii band2 ; "which corresponds to" ;
		    (match opt_reduced_term2 with
		    | None -> "NONE"
		    | Some term -> Lambda_Term.pretty term
		    ) ;
		    ""
		  ]  >> (String.concat "\n") >> print_string
		    ;
		  equivalent
		end
	      
    let (check_reduce_on: Lambda_Term.t -> unit) = fun lambda_term ->
	  assert (is_reduce_correct_on lambda_term)

    let (test: unit -> unit) = fun () ->
	  List.iter check_reduce_on Lambda_Term.examples
	    
  end)



(* DEMO *)

let (demo: unit -> bool list) = fun () ->
      begin
	print_string "\n\n* DEMO * LC_by_MT.ml:\n" ;
	List.map Reduce.is_reduce_correct_on Lambda_Term.examples
      end

    
