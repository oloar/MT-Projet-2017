(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of THE TURING MACHINE FOR REAL
 *
 * Execution of a Turing Machine (single band or multi-bands TM) 
 *
 *)
  
open Tricks
  
open State  
open Band    
open Action
open Transition
open Turing_Machine
open Configuration  
open Transition

(* PROJECT 2015 *) 

module Execution =
  (struct

    type log = Logger.t list
  
    let (select_enabled_transition: Turing_Machine.t -> Configuration.t -> Transition.t option) = fun tm cfg ->
	  let
	      enabled_transitions = List.filter (Configuration.is_transition_enabled_on cfg) tm.transitions
	  in
	    match enabled_transitions with
	    | []           -> None
	    | [transition] -> Some transition
	    | transitions  ->
		    let error = "Execution.select_enabled_transition: non deterministic TM" in
		      let msg = String.concat " "
			  [ error ; cfg.tm.name ; String.concat "\n - " (List.map Transition.pretty transitions) ]
		      in begin print_string msg ; failwith error  end

	  
   (* mutually recursives functions to deal with the instruction Machine(TM name)  *)

    let rec (execute_instruction_on_one_band: log -> instruction -> Band.t -> Band.t) = fun log instruction band ->
	  List.hd (execute_instruction log instruction [band])
	    
    and (execute_instruction: log -> instruction -> Band.t list -> Band.t list) = fun log instruction bands ->
	  match instruction with
	  | Action action -> Action.perform action bands
         
	  | Run tm -> (* one reason of mutual recursivity: "run" calls "perform" which can call "run" *)
		  let final_cfg = log_run ~log:log (Configuration.make tm bands) in final_cfg.bands
		      
	  | Call name -> run_tm_named name bands
		    
	  | Seq [] -> bands
	  | Seq (inst::instructions) -> bands >> (execute_instruction log inst) >> (execute_instruction log (Seq instructions))

	  | Parallel instructions ->
		  List.map
		    (fun (inst,band) -> execute_instruction_on_one_band log inst band)
		    (Instruction.zip instructions bands)
	    
    and (execute_transition: log -> Transition.t -> Configuration.t -> Configuration.t) = fun log (_,instruction,target) cfg ->
	  { cfg with bands = execute_instruction log instruction cfg.bands ; state = target } 

    and (one_step: int * log -> Configuration.t -> Configuration.t) = fun (call_depth,log) cfg ->
	  begin
	    Configuration.print_using ~n_first:call_depth log cfg
	      ;
	    match select_enabled_transition cfg.tm cfg with
	    | None            -> { cfg with status = Final }
	    | Some transition -> execute_transition log transition cfg
	  end

    and log_run ?call_depth:(call_depth=2) ?log:(log=[]) : (Configuration.t -> Configuration.t) = fun cfg ->
	  let final_cfg = run (call_depth, cfg.logger::log) cfg in
	    begin
	      cfg.logger#close ;
	      final_cfg
	    end

    and (run: int * log -> Configuration.t -> Configuration.t) = fun log cfg ->
    	  if (cfg.status = Final)
	  then cfg
	  else
	    let next_cfg = one_step log cfg
	    in run log next_cfg
	      
    and (run_tm_named: string -> Band.t list -> Band.t list) = fun name bands ->
	  let tm = Turing_Machine.i_find_tm_named name in
	    let final_cfg = run (1,[]) (Configuration.make ~time:true tm bands)
	    in  final_cfg.bands

  end)

