(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Mars 2017 
 *
 * Part of the project: TURING MACHINES FOR REAL
 *
 * A run of a Turing Machine is a sequence of configurations.
 *
 * The configuration contains all the information needed to perform the next step.
 *)

open Tricks

open Band
open State
open Action
open Transition
open Turing_Machine
  
  
type status = Running | Final
  
type configuration = { tm: Turing_Machine.t ;
               bands: Band.t list ;
               state: State.t ;
               status: status ; 
               logger: Logger.t ;
             }
      
type configurations = configuration list

      
module Configuration =
  (struct

    type t = configuration

    let (name: Turing_Machine.t -> Band.t list -> string) = fun tm bands ->
      let nb_bands = List.length bands in
        String.concat "" [ "TM_" ; tm.name ; "_" ; string_of_int nb_bands ; "x" ;  if nb_bands>1 then "Bands" else "Band" ; Pretty.get_extension() ]

    let make ?time:(time=false) : (Turing_Machine.t -> Band.t list -> configuration) = fun tm bands ->
      let
          filename = (* if time then begin Date.sleep_for 0.1 ; String.concat "_" [ Date.pretty_time() ; name tm bands ] end else *) name tm bands 
      in
      { tm = tm ;
        bands = bands ;
        state = tm.initial ;
        status = Running ;
        logger = new Logger.logger (Some filename)
      }

    let (is_transition_enabled_on: configuration -> Transition.t -> bool) = fun cfg (source,instruction,_) ->
      (cfg.state = source)
        &&
      (Instruction.is_enabled_on cfg.bands instruction)
        
        
    (* PRETTY PRINTING *)
      
    (* ascii *)
            
    let (to_ascii: configuration -> string) = fun cfg ->
      cfg.bands >> (List.map Band.to_ascii) >> (String.concat "\n")

    (* html *)

    let (to_html: Html.options -> configuration -> Html.table) = fun options cfg ->
      let tm_name = Html.cell [] cfg.tm.name
          
      and bands = Html.cell [] (Band.to_html_many options cfg.bands)

      and state =
             let state_color =
           if cfg.state = cfg.tm.accept then Color.green
           else if cfg.state = cfg.tm.reject then Color.red
           else Color.white
         in State.to_html [("bgcolor", Html.Color state_color)] cfg.state
          in
        Html.table
          (options @ [ ("bordercolor", Html.Color Color.gray) ; ("cellpadding",Html.Int 0) ; ("cellspacing",Html.Int 0) ; ("border",Html.Int 1) ])
          [ tm_name ; state ; bands ]
          
    (* user *)

    let (pretty: t -> string) = fun t ->
      match Pretty.get_format() with
      | Pretty.Html  -> to_html [] t
      | Pretty.Ascii -> to_ascii t

(*    let (print: t -> unit) = fun cfg ->
      cfg.logger#print (pretty cfg)
 *)
            
    let (do_times: int -> ('t -> unit) -> 't list -> unit) = fun n f ts ->
      List.iteri (fun i t -> if i<n then f t else ()) ts
      
    let print_using ?n_first:(n_first=(-1)) : (Logger.t list -> t -> unit) = fun loggers cfg ->
      let log_cfg = fun logger -> logger#print (pretty cfg) in
        if n_first<0
        then List.iter log_cfg loggers
        else do_times n_first log_cfg loggers 

    let show_using ?n_first:(n_first=(-1)) : (Logger.t list -> t -> t) = fun loggers cfg ->
      begin
        print_using ~n_first:n_first loggers cfg ;
        cfg
      end
          
  end)      


(* DEMO *)      

open Alphabet
open Symbol
  
let (demo: unit -> unit) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
    let band1 = Band.make alphabet [B;D;Z;U;U;Z;B]
    and band2 = Band.make alphabet [B;B;B;L;B;B;B] in
      let cfg = Configuration.make Turing_Machine.nop [ band1 ; band2 ] in
        begin
          print_string (Configuration.to_ascii cfg) ;
          cfg.logger#print (Configuration.to_html [] cfg) ;
          cfg.logger#close 
        end
          
    
