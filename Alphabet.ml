(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * - The alphabet of symbols of a Turing Machine
 * 
 * - its conversion into vectors of bits for Turing Machine on binary alphabet {B,D}
 *
 * - IMPORTANT : The default symbol of the band (the blank symbol B) must be one of the two bits
 *               I chose  B <-> 0 , D <-> 1
 *)

open Symbol
 

module Bit =
  struct
    type t = symbol
    let (zero:t) = B
    let (unit:t) = D
    let (pretty: t -> string) = Symbol.pretty 
  end

    
module Bits = Bit_Vector.Made_Of(Bit)
    

    
type alphabet = { symbols: symbols ;  symbol_size_in_bits: int }

module Alphabet =
  (struct

    type t = alphabet

    let (empty: alphabet) = { symbols = [B] ; symbol_size_in_bits = 1 } 

    let (make: symbols -> alphabet) = fun symbols ->
	  let symbols = MyList.union [B] symbols 
	  in
	    { symbols = symbols ;
	      symbol_size_in_bits = Bits.nb_bits_for (List.length symbols)
	    }

    let (binary: alphabet) = make [Bit.zero ; Bit.unit]

    let (full: alphabet) = make [D;U;Z;S;L;O;C;X]
	
  end)


(* DEMO *)

open Pretty
open Tricks (* provides >> *)
  
let _ = Pretty.print ~format:Ascii (fun _ -> (String.concat "\n" [ "\n\n* DEMO * Alphabet.ml:\n" ; (Bits.enumerate 8) >> Bits.prettys ]))

  
