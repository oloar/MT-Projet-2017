(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * Some Turing Machines
 *
 *)


  
open Alphabet
open State
open Pattern
open Symbol
open Action
open Transition
open Html
open Pretty


module Turing_Machine =
  (struct

    type t = turing_machine
	  
    let (nop: t) = { name = "" ;
		     nb_bands = 1 ;
		     initial = State.initial ; accept = State.accept ; reject = State.reject ;
		     transitions = [ (State.initial, Action(Nop), State.accept)]
		   }
	
    let (finalize: string -> turing_machine -> turing_machine) = fun name tm ->
	  { tm with name = name }

    let (sequence: Instruction.t list -> turing_machine) = fun instructions ->
	  let init = nop.initial and accept = nop.accept in
	    { nop with
	      name = Instruction.to_ascii (Seq instructions) ;
	      transitions = [ (init, Seq instructions, accept) ]	    
	    }

    (* PRETTY PRINTING *)

    let (to_ascii: turing_machine -> string) = fun tm -> tm.name

    let (to_html: Html.options -> turing_machine -> Html.content) = fun _ tm -> Html.italic (to_ascii tm)
	    
    (* user *)
	    
    let (pretty: t -> string) =
      match Pretty.get_format() with
      | Pretty.Html  -> (to_html [])
      | Pretty.Ascii -> to_ascii
		
		
    (* IMPERATIVE FEATURES for reusing existing turing machine *) 	    
		
    class collection_of_turing_machine =
      object
	val mutable collection: turing_machine list = []
	    
	method add: turing_machine -> unit = fun tm ->
	      collection <- tm::collection 
				  
	method find: string -> turing_machine = fun name ->
	      match List.filter (fun tm -> tm.name = name) collection with
	      | [tm] -> tm
	      | [] -> let error_msg = String.concat "" [ "Turing_Machine.collection_of_turing_machine #find: TM " ; name ; " not found in the library." ] in failwith error_msg
	      | _  -> let error_msg = String.concat "" [ "Turing_Machine.collection_of_turing_machine #find: Multiple TM " ; name ; " in the library."  ] in failwith error_msg
      end
	
	
    let object_TM_library = new collection_of_turing_machine
	
    let (i_make: string -> turing_machine -> turing_machine) = fun name turing_machine ->
	  let tm = finalize name turing_machine in
	    begin
	      object_TM_library#add tm ;
	      tm
	    end
	      
    let (i_find_tm_named: string -> turing_machine) = fun name ->
	  object_TM_library#find name
	    
	    
	    
    (* A COLLECTION OF BASIC TM *)
	    
    let (move: Moving.t -> turing_machine) = fun direction ->
	  let init = nop.initial and accept = nop.accept in
	    { nop with
	      name = String.concat "_" [ "move" ; Moving.to_ascii direction ] ;
	      transitions = [ (init, Action(RWM(Match(ANY), No_Write, direction)), accept) ]
	    }
	    
    let (find_symbol_on_the: Symbol.t -> Moving.t -> turing_machine) = fun symbol direction ->
	  let
	      tm_name = String.concat "_" [ Symbol.verbatim symbol ; "on_the" ; Moving.to_ascii direction ]
	  in
	  let init = nop.initial and accept = nop.accept in
	    finalize tm_name
	      { nop with
		transitions =
 		[ (init, Action(RWM(Match(BUT symbol), No_Write, direction)), init) ;
		  (init, Action(RWM(Match(VAL symbol), No_Write, Here)), accept)
		]
	      }
	      
    let (first_blank_on_the: Moving.t -> turing_machine) = find_symbol_on_the B 
      
    let (second_blank_on_the: Moving.t -> turing_machine) = fun direction ->
	  let init = nop.initial and accept = nop.accept in
	    { nop with
	      name = String.concat "_" [ "second_blank_on_the" ; Moving.to_ascii direction ] ;
	      transitions = [ (init, Seq [ Run (first_blank_on_the direction) ; Action(RWM(Match(ANY),No_Write, direction)) ; Run (first_blank_on_the direction) ], accept) ] 
	    }

    let (goto_right_blank:  turing_machine) = find_symbol_on_the B Right

    let (goto_right_dollar: turing_machine) = find_symbol_on_the D Right 
    

    let (most_on_the: Moving.t -> turing_machine) = fun dir ->
	  let rid =
	    (match dir with
	    | Left -> Right
	    | Right -> Left
	    )
	  in
	    let init = nop.initial and accept = nop.accept in let loop = State.fresh_from init in	      
	      { nop with
		transitions = [ (init, Action( RWM (Match(ANY), No_Write, dir)), loop) ;
				(loop, Action(RWM(Match(BUT B), No_Write, dir)), loop) ;
				(loop, Action(RWM(Match(VAL B), No_Write, rid)), accept)			    
			      ]
	      }

    let (right_most: turing_machine) = finalize ">?B" (most_on_the Right)

    let (left_most:  turing_machine) = finalize "B?<" (most_on_the Left)

    let (erase: turing_machine) = 
      let init = nop.initial and accept = nop.accept in let loop = State.fresh_from init in	
	{ nop with
	  name = "erase" ;
	  transitions =
 	  [ (init, Run right_most, loop) ;
	    (loop, Action( RWM (Match(BUT B), Write B,  Left)), loop) ;
	    (loop, Action( RWM (Match(VAL B), No_Write, Here)), accept)
	  ]
	}
	
    let (erase_backward: turing_machine) = 
      let init = nop.initial and accept = nop.accept in let loop = State.fresh_from init in	
	{ nop with
	  name = "<erase" ;
	  transitions =
 	  [ (init, Action( RWM (Match(ANY)  , Write B , Left )), loop) ;
	    (loop, Action( RWM (Match(BUT B), Write B , Left )), loop) ;
	    (loop, Action( RWM (Match(VAL B), No_Write, Right)), accept)
	  ]
	}
	
    let (incr: turing_machine) =
      let init = nop.initial and accept = nop.accept in 	
	{ nop with
	  name =  "incr" ; 
	  transitions = 
 	  [ (init, Action(RWM(Match(VAL U), Write Z, Right)), init) ;
	    (init, Action(RWM(Match(VAL Z), Write U, Here )), accept) ; 
            (init, Action(RWM(Match(VAL B), Write U, Here )), accept) 
	  ]
	}


    let (decr: turing_machine) =
      let init = nop.initial and accept = nop.accept and reject = nop.reject in
 	let unit = State.fresh_from init in
	  let zero = State.fresh_from unit in
	    let back = State.fresh_from zero in
	      { nop with
		name = "decr" ;
		transitions = 
		[ (init, Action( RWM (Match(VAL Z), No_Write, Right)), unit) ;
		  (init, Action( RWM (Match(VAL U), Write Z , Here )), accept) ;
		  (init, Action( RWM (Match(VAL B), No_Write, Here )), reject) ;
		  
		  (unit , Action( RWM (Match(VAL B), No_Write, Left )), reject) ;
		  (unit , Action( RWM (Match(VAL Z), No_Write, Right)), unit) ;
		  (unit , Action( RWM (Match(VAL U), Write B , Right)), zero) ;
		  
		  (zero , Seq [ Action( RWM (Match(VAL B), No_Write, Left)) ; Action( RWM (Match(VAL B), No_Write, Left)) ], back) ;
		  (zero , Seq [ Action( RWM (Match(BUT B), No_Write, Left)) ; Action( RWM (Match(VAL B), Write  Z, Left)) ], back) ;
		  
		  (back , Action( RWM (Match(VAL Z), Write U , Left )), back) ;
		  (back , Action( RWM (Match(VAL B), No_Write, Right)), accept) 
		]
	      }
		
    let (generic_dup: symbols -> turing_machine) = fun symbols ->
	  let init = nop.initial and accept = nop.accept in	  
	    let generic_transitions =
	      Transition.foreach_symbol_of symbols (OUT [B;D]) (fun s ->
		    [ (init     , Action(RWM(Match(VAL(s)), Write B, Right)) , Qs(1,[s])) ; 
		      (Qs(1,[s]), Run(second_blank_on_the Right), Qs(2,[s])) ; 
		      (Qs(2,[s]), Action(RWM(Match(VAL(B)), Write s, Here)) , Qs(3,[s])) ;
		      (Qs(3,[s]), Run (second_blank_on_the Left)            , Qs(4,[s])) ; 
		      (Qs(4,[s]), Action(RWM(Match(VAL(B)), Write s, Right)), init)
		    ])
	    in 
	      { nop with
		name = "dup_" ^ (Pretty.set Symbol.to_ascii symbols) ;
		transitions = generic_transitions
		@ [ (init, Action(RWM(Match(VAL B), Write D, Here)), accept) ] 
	      }
	    
	
    let (generic_swap: symbols -> turing_machine) = fun symbols ->
	  let init = nop.initial and accept = nop.accept in	  
	    let generic_transitions =
	      Transition.foreach_symbol_of symbols ANY (fun s ->
		    Transition.foreach_symbol_of symbols ANY (fun l ->
			  [ (init     , Action( RWM (Match(VAL(s)), Write B, Right)), Qs(1,[s])) ; 
			    (Qs(1,[s]), Action( RWM (Match(VAL(l)), Write s, Left )), Qs(2,[l])) ;
			    (Qs(2,[l]), Action( RWM (Match(VAL(B)), Write l, Right)), accept)
			  ]))
	    in 
	      { nop with
		name = "swap_" ^ (Pretty.set Symbol.to_ascii symbols) ; 
		transitions = generic_transitions
	      }

    (* The busy beavers     : https://en.wikipedia.org/wiki/Busy_beaver
     * Les castors affairés : https://fr.wikipedia.org/wiki/Castor_affairé 
     *)

    let (bb4: turing_machine) = 
      let z = Bit.zero and u = Bit.unit in
	let init = nop.initial and accept = nop.accept in
	  { nop with
	    name = "BB4_stops_after_107_steps" ;
	    transitions = 
	    [ (init, Action( RWM (Match(VAL z), Write u, Right)), Q 2) ;
	      (init, Action( RWM (Match(VAL u), Write u, Left )), Q 2) ;
	      (Q 2, Action( RWM (Match(VAL z), Write u, Left )), init) ;
	      (Q 2, Action( RWM (Match(VAL u), Write z, Left )), Q 3) ;
	      (Q 3, Action( RWM (Match(VAL z), Write u, Here )), accept) ;
	      (Q 3, Action( RWM (Match(VAL u), Write u, Left )), Q 4) ;
	      (Q 4, Action( RWM (Match(VAL z), Write u, Right)), Q 4) ;
	      (Q 4, Action( RWM (Match(VAL u), Write z, Right)), init) 
	    ] 
	  }
	    
    let (bb5: turing_machine) = 
      let z = Bit.zero and u = Bit.unit in
	let init = nop.initial and accept = nop.accept in	    
	  { nop with
	    name = "BB5_stops_after_47_176_870_steps" ;
	    transitions = 
	    [ (init, Action( RWM (Match(VAL z), Write u, Left )), Q 2) ;
	      (init, Action( RWM (Match(VAL u), Write u, Right)), Q 3) ;
	      (Q 2, Action( RWM (Match(VAL z), Write u, Left )), Q 3) ;
	      (Q 2, Action( RWM (Match(VAL u), Write u, Left )), Q 2) ;
	      (Q 3, Action( RWM (Match(VAL z), Write u, Left )), Q 4) ;
	      (Q 3, Action( RWM (Match(VAL u), Write z, Right)), Q 5) ;
	      (Q 4, Action( RWM (Match(VAL z), Write u, Right)), init) ;
	      (Q 4, Action( RWM (Match(VAL u), Write u, Right)), Q 4) ;
	      (Q 5, Action( RWM (Match(VAL z), Write u, Here )), accept) ;
	      (Q 5, Action( RWM (Match(VAL u), Write z, Right)), init) 	    
	    ] 
	  }

	
    let (bb6: turing_machine) = 
      let z = Bit.zero and u = Bit.unit in
	let init = nop.initial and accept = nop.accept in	          
	  { nop with
	    name = "BB6_discovered_in_june_2010_stops_after_3.515_*_10^18267_steps" ;
	    transitions = 
	    [ (init, Action( RWM (Match(VAL z), Write u, Right)), Q 2) ;
	      (init, Action( RWM (Match(VAL u), Write u, Left )), Q 5) ;
	      (Q 2, Action( RWM (Match(VAL z), Write u, Right)), Q 3) ;
	      (Q 2, Action( RWM (Match(VAL u), Write u, Right)), Q 6) ;
	      (Q 3, Action( RWM (Match(VAL z), Write u, Left )), Q 4) ;
	      (Q 3, Action( RWM (Match(VAL u), Write z, Right)), Q 2) ;
	      (Q 4, Action( RWM (Match(VAL z), Write u, Right)), Q 5) ;
	      (Q 4, Action( RWM (Match(VAL u), Write z, Left )), Q 3) ;
	      (Q 5, Action( RWM (Match(VAL z), Write u, Left )), init) ;
    	      (Q 5, Action( RWM (Match(VAL u), Write z, Right)), Q 4) ;
	      (Q 6, Action( RWM (Match(VAL z), Write u, Left )), accept) ;
	      (Q 6, Action( RWM (Match(VAL u), Write z, Right)), Q 3) 	    
	    ] 
	  }

    (* BB 7 and beyond are unknown *)
	
		  
    (* TWO BANDS TURING MACHINES *)

    let (generic_copy: symbols -> turing_machine) = fun symbols ->
	  let init = nop.initial and accept = nop.accept in
	    let q = State.fresh_from init in	          	  
	      let generic_transitions =
		Transition.foreach_symbol_of symbols (BUT B)
		  (fun s ->
			[ (init, Action( Simultaneous [ RWM(Match(VAL s), No_Write, Right) ; RWM(Match ANY, Write s, Right) ]), init) ]
		  )
	      in 
		{ nop with
		  nb_bands = 2 ;
		  name = "copy_" ^ (Pretty.set Symbol.to_ascii symbols) ;
		  transitions = generic_transitions
		  @ [ (init, Action( Simultaneous [ RWM(Match(VAL B), No_Write, Left)  ; RWM(Match ANY, No_Write, Left) ]), q) ;
		      (q, Parallel [ Run(left_most) ; Run (left_most) ], accept) 
		    ] 
		}
	
    let (generic_reverse: symbols -> turing_machine) = fun symbols ->
	  let init = nop.initial and accept = nop.accept in
	    let copy = State.fresh_from init in
	      let head = State.fresh_from copy in
		let	generic_transitions = Transition.foreach_symbol_of symbols (BUT B)
		    (fun s ->
			  [ (copy, Action( Simultaneous [ RWM (Match(VAL s), No_Write, Left) ; RWM (Match ANY, Write s, Right) ]), copy) ]
		    )
		in 
		  { nop with
		    nb_bands = 2 ;
		    name = "rev_" ^ (Pretty.set Symbol.to_ascii symbols) ;
		    transitions = generic_transitions
		    @ [ (init, Parallel [ Run(right_most) ; Action(Nop) ], copy) ;			
			(copy, Parallel [ Action( RWM (Match(VAL B), No_Write, Right)) ; Action( RWM (Match(VAL B), No_Write, Left)) ], head) ;
			(head, Parallel [ Action(Nop) ; Run(left_most) ], accept)
		      ] 
		  }	
		    
    let (xor:turing_machine) = 
      let init = nop.initial and accept = nop.accept in
	let q = State.fresh_from init in	          	  
	  { nop with
	    nb_bands = 2 ;
	    name = "xor" ;
	    transitions =
	    [
	     (init, Action( Simultaneous [ RWM (Match(VAL Z), Write Z, Right) ; RWM (Match(VAL Z), No_Write, Right) ]), init) ;
	     (init, Action( Simultaneous [ RWM (Match(VAL Z), Write U, Right) ; RWM (Match(VAL U), No_Write, Right) ]), init) ;
	     (init, Action( Simultaneous [ RWM (Match(VAL U), Write Z, Right) ; RWM (Match(VAL U), No_Write, Right) ]), init) ;
	     (init, Action( Simultaneous [ RWM (Match(VAL U), Write U, Right) ; RWM (Match(VAL Z), No_Write, Right) ]), init) ;
	     (init, Action( Simultaneous [ RWM (Match(VAL B), No_Write, Left) ; Nop ]), q) ;
	     (q, Parallel [ Run(left_most) ; Run(left_most) ], accept) 
	   ]
	  }

	
	    
  end)

    
