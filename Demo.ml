(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * Runs of Turing Machines
 *
 *)


open Symbol
open Alphabet
open Band
open Transition
open Turing_Machine
open Configuration
open Execution
  
let (incr_decr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
    let band1 = Band.make alphabet [U;U;Z;U] in
      let tm = Turing_Machine.sequence [ Run Turing_Machine.incr ; Run Turing_Machine.left_most ; Run Turing_Machine.decr ] in
        let cfg = Configuration.make tm [ band1 ] in
          Execution.log_run cfg 


let (incr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
    let band1 = Band.make alphabet [U;U;Z;U] in
      let cfg = Configuration.make Turing_Machine.incr [ band1 ] in
        Execution.log_run cfg 
          

let (decr1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
    let band1 = Band.make alphabet [Z;Z;Z;U] in
      let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
        Execution.log_run cfg 

let (decr2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
    let band1 = Band.make alphabet [Z;Z;Z;U;U] in
      let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
        Execution.log_run cfg 
          

let (gen_dup: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
    let dup = Turing_Machine.generic_dup alphabet.symbols in
      let band1 = Band.make alphabet [U;Z;Z;U] in
        let cfg = Configuration.make dup [ band1 ] in
          Execution.log_run cfg 

let (gen_copy: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
    let copy = Turing_Machine.generic_copy alphabet.symbols in
      let band1 = Band.make alphabet [Z;U;U;Z] 
      and band2 = Band.make alphabet [] in
        let cfg = Configuration.make copy [ band1 ; band2 ] in
          Execution.log_run cfg 

          
let (gen_reverse: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
    let reverse = Turing_Machine.generic_reverse alphabet.symbols in    
      let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
      and band2 = Band.make alphabet [] in
        let cfg = Configuration.make reverse [ band1 ; band2 ] in
          Execution.log_run cfg 


let (gen_swap: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
    let swap = Turing_Machine.generic_swap alphabet.symbols in  
      let band1 = Band.make alphabet [U;Z;U;U;Z;Z] in
        let cfg = Configuration.make swap [ band1 ] in
          Execution.log_run cfg 

let (xor: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U] in
    let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
    and band2 = Band.make alphabet [U;Z;U;U;Z;Z] in
      let cfg = Configuration.make Turing_Machine.xor [ band1 ; band2 ] in
        Execution.log_run cfg
        

let (busy_beaver: Turing_Machine.t -> Configuration.t) = fun bb ->
      let alphabet = Alphabet.binary in
    let band = Band.make alphabet [] in
      let cfg = Configuration.make bb [ band ] in
        Execution.log_run cfg 


(* 
  1. [B;Z;U;O;C] -> [O;Z;C;O;C]   BON
  2. [O;L;X;S;C] -> [O;L;X;S;X;C] BON
  3. [O;L;X;S]   -> [O;L;X;S;X]   FAUX
  4. [O;L;X;S;C] -> [C;O;L;X;S;X] FAUX
 *)
let (parenthese: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [O;L;X;S;C] in
      let swap = Turing_Machine.parenthesage_bon alphabet.symbols in
    let band1 = Band.make alphabet [O;L;X;S;X;C] 
    and band2 = Band.make alphabet []
    and band3 = Band.make alphabet [] in
      let cfg = Configuration.make swap [ band1 ; band2 ; band3] in
        Execution.log_run cfg

let (remplace_terme: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [O;L;U;Z;X;S;C] in
      let swap = Turing_Machine.remplace_terme alphabet.symbols in
    let band1 = Band.make alphabet [X;Z;X] 
    and band2 = Band.make alphabet [X;D;U;U]
    and band3 = Band.make alphabet [] in
      let cfg = Configuration.make swap [ band1 ; band2 ; band3] in
        Execution.log_run cfg

let (remplace_lambda_terme: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [O;L;U;Z;X;S;C] in
      let swap = Turing_Machine.remplace_lambda_terme alphabet.symbols in
    let band1 = Band.make alphabet [O;L;X;S;X;Z;S;X;C;U] 
    and band2 = Band.make alphabet []
    and band3 = Band.make alphabet []
    and band4 = Band.make alphabet [] in
      let cfg = Configuration.make swap [ band1 ; band2 ; band3 ; band4 ] in
        Execution.log_run cfg


(* DEMO *)
            
let (demo: unit -> unit) = fun () ->
      begin
    print_string "\n\n* DEMO * Demo.ml:\n\n" ;
    List.iter (fun _ -> ())
      [ incr ()  ;
        decr1 () ;
        decr2 () ;    
        incr_decr () ;
        gen_dup () ;
        gen_copy () ;    
        gen_reverse () ;
        gen_swap () ;
        xor () ;
        parenthese();
        remplace_terme();
        remplace_lambda_terme();
        busy_beaver Turing_Machine.bb4
           (* 
        * /!\  TERMINATING BUT EXTREMLY LONG COMPUTATIONS ... The sun will be dead before the end of BB6.
        *
          busy_beaver Turing_Machine.bb5 ;    
          busy_beaver Turing_Machine.bb6 ;    
        *)
      ]
      end

