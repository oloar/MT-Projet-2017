(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * lambda terms, reduction with tracing of beta-reduction 
 *
 *)
  
  
(* USAGE
 * Required modules: 
 * Compilation     : ocamlc Lambda_Calcul.ml 
*)

  
(* TRACING and DELAYING A COMPUTATION

   Consider the computation (1+2) of type: int 
   then  (fun _ -> (1+2))  is a delayed computation of type: unit -> int 

   It can be triggered later by calling it with the value () of type unit.
   Indeed,  (fun _ -> (1+2)) ()  beta-reduces to   (1+2)
*)


type 'a delayed_computation = unit -> 'a

	
module Trace =
  (struct

    let _TRACE_ = ref (false)

    let (on: unit -> unit)  = fun () -> _TRACE_ := true 
    let (off: unit -> unit) = fun () -> _TRACE_ := false

    let (print: string -> unit) = fun string -> if (!_TRACE_) then print_string string else ()	

    let (trace: (unit -> 'a) -> 'a) = fun delayed_computation ->
	  begin
	    print_string "\n" ; on() ; let result = delayed_computation() in begin off() ; print_string "\n" ; result end
	  end

    let (untrace: (unit -> 'a) -> 'a) = fun delayed_computation ->
	  begin
	    off() ; let result = delayed_computation() in begin on() ; result end
	  end
	    
      
    let _INDENTATION_ = ref 0

    let (indent: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) + 2
    let (nident: unit -> unit) = fun () -> _INDENTATION_ := (!_INDENTATION_) - 2	

    let (newline: string -> unit) = fun string ->
	  print (String.concat "" ["\n" ; String.make (!_INDENTATION_) ' ' ; string ])

    let (call: string list -> ('a -> string) -> (unit -> 'a) -> 'a) = fun strings pretty delayed_computation ->
	  begin
	    newline (String.concat " " strings) ;
	    indent();
	    let result = delayed_computation() in
	      begin
		nident();
		newline (String.concat " " [ "~~~>  " ; pretty result ]) ;
		result
	      end
	  end		  
  end)
    


    
(* REPRESENTATION OF LAMBDA TERMS *)  

type lambda_term =
  | Lambda of var * lambda_term 
  | Apply of lambda_term list
  | Var of (string * int)

and  var = lambda_term




(* OPERATION ON LAMBDA TERMS *)      

module Lambda_Term =
  (struct
    
    type t = lambda_term
	  
    (* PRETTY PRINTING *)
	  
    let (parenthesis: string -> string) = fun string -> "(" ^ string ^ ")"
									 
    let (brace: string -> string) = fun string -> "{" ^ string ^ "}"

    let rec (pretty: t -> string) =
      function
	| Var (string,i) -> string ^ (if i<0 then "" else string_of_int i)
	| Lambda (var,expr) -> brace (String.concat "." [ pretty var ; pretty expr ])
	| Apply [expr] -> pretty expr
	| Apply exprs ->  parenthesis (String.concat " " (List.map pretty exprs))


    (* EXAMPLES *)
		  
    let ref_fresh = ref (-1)
	
    let (i_newvar: unit -> t) = fun () ->
	  begin
	    ref_fresh := (!ref_fresh) + 1 ;
	    Var ("x",!ref_fresh)
	  end
	    
    let (var: string -> t) = fun string ->
	  (* Named variables: Var(string, -1) *)
	  (* OR *)
	  (* Enumerated variable: *) i_newvar()	
	    
    let (z: t) = var "z"
    let (s: t) = var "s"
    let (x: t) = var "x"
    let (y: t) = var "y"
    let (b: t) = var "b"
    let (e: t) = var "e"
    let (t: t) = var "t"
    let (n: t) = var "n"
    let (m: t) = var "m"
	
    let (id: t) = Lambda (x,x)
    let (self: t) = Lambda(x, Apply [x;x])
    let (diverge: t) = Apply [ self ; self ]

    let (ifte: t) = Lambda(b, Lambda(t, Lambda(e, Apply [b;t;e])))
    let (vrai: t) = Lambda(t,Lambda(e,t))
    let (faux: t) = Lambda(t,Lambda(e,e))
	
    let (zero: t) = Lambda(s, Lambda(z, z))
    let (unit: t) = Lambda(s, Lambda(z, Apply[s ; z]))
    let (incr: t) = Lambda(n, Lambda(s, Lambda(z, Apply[s ; Apply [n ; s ; z]])))
    let (sum:  t) = Lambda(n, Lambda(m, Lambda(s, (Lambda (z, Apply[n ; s ; Apply[m ; s ; z]])))))
	
    let (example1: t) = Apply[zero; s; z] 
    let (example2: t) =	Apply[unit; s; z]
    let (example3: t) = Apply[ Apply[incr;zero]; s; z] 
    let (example4: t) = Apply[ Apply[sum;unit;zero]; s; z] 
    let (example5: t) = Lambda(x, Apply[unit; s; z])  
    let (example6: t) = Apply[ Lambda(x, Apply[unit; s; z]) ; diverge ]     

    let (examples: lambda_term list) = 
      [ example1 ; 
	example2 ; 
	example3 ;
	example4 ;
	example5 ;
	example6 
      ]
	
    (* Ocaml Constructors == uninterpreted functions *)
	
    type nat = Zero | Succ of nat
	
    let (fZ: t) = Var("Zero",-1) (* the uninterpreted function Zero of Peano natural numbers *)
    let (fS: t) = Var("Succ",-1) (* the uninterpreted function Succ of Peano natural numbers *)			
	

    (* BETA REDUCTION *)    
      
    (** tracing *)	  

    let (trace_call: string -> t -> t delayed_computation -> t) = fun function_name t delayed_computation ->
	  Trace.call
	    [ function_name ; pretty t ]
	    pretty
	    delayed_computation

    let (trace_beta: (t * t) -> t -> t delayed_computation -> t) = fun (var,arg) expr delayed_computation ->
	  Trace.call
	    [ "*"
	    ; pretty (Apply[ Lambda(var,expr) ; arg])
	    ; "  by beta_reduction"
	    ; "[" ; pretty var ; "/" ; pretty arg ; "]"
	    ]
	    pretty
	    delayed_computation
	    
	  
   (* Fixpoint = iterate a transformation until stabilisation *)                      

    let rec (fixpoint: (t -> t) -> t -> t) = fun transform t ->
	  let t' = transform t in
	    if t'=t
	    then t
	    else fixpoint transform t'

			
   (* REDUCTION OF LAMBDA TERM BY BETA REDUCTION *)
      
    let rec (beta_reduction: (var * t) -> t -> t) = fun (var,t) ->
	  function
	    | Var _ as v -> if v = var  then t else v
	    | Apply(args) ->
		    Apply (List.map (beta_reduction (var,t)) args)
	    | Lambda(v,e) ->
		    if v = var
		    then Lambda(v,e)
		    else Lambda(v, beta_reduction (var,t) e)
	  
                                                                      (* INSTRUMENTATION *)
    let rec (* USER *) (reduce: t -> t) = fun t ->
	  begin                                                          trace_call "reduce" t (fun _ -> 
	    match t with
	    | Apply [expr] -> reduce expr

	    | Lambda(var,expr) 
	      -> begin                                                   trace_call "reduce Lambda:" t (fun _ ->
		    Lambda(var, reduce expr)                                                           )
	         end

	    | t -> fixpoint reduction t                                                        )
	  end

    and (reduction: t -> t) = fun t ->
	  match t with
	  | Apply (Lambda(var,expr) :: arg :: args)
	    -> let beta_reduced_expr =
	         begin                                                   trace_beta (var,arg) expr (fun _ ->
		   beta_reduction (var,arg) expr                                                   )
		 end
	       in
	          Apply ( beta_reduced_expr :: args)

	  | Apply [expr] -> expr

	  | Apply exprs
	    -> begin                                                     trace_call "reduction Apply:" t (fun _ ->   
		  Apply (List.map reduce exprs)                                                          )
	       end

	  | t -> t


    let (* USER *) (i_reduce: t -> t) = fun t ->
	  Trace.trace (fun _ -> reduce t) 
	    
    let (demo: unit -> lambda_term list) = fun () ->
	  List.map i_reduce examples
    
  end)

   
(* DEMO 
   let _ = Reduce.demo()
*)
    
(* DEMO /!\ NON TERMINATING 
   let _ = Reduce.i_reduce Lambda_Term.diverge ;; 
 *)

    



