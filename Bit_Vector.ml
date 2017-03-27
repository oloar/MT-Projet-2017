(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * Operation on bit vectors  parmaterized by the representation of bits (using functor)
 * 
 * - Requires an instranciation of a Bit module defining Two_Values zero and unit
 *
 * - How to use it ? Have a look at the DEMO
 *
 *)
  
open Pretty

  
(* REPRESENTATION OF BIT *)
  
module type Two_Values_Sig =
  sig
    type t 
    val zero:t
    val unit:t
    val pretty: t -> string
  end

module Bit_as_Int : Two_Values_Sig =
  struct
    type t = int
    let (zero:t) = 0
    let (unit:t) = 1
    let (pretty: t -> string) = string_of_int 
  end

module Bit_as_Boolean : Two_Values_Sig =
  struct
    type t = bool
    let (zero:t) = false
    let (unit:t) = true
    let (pretty: t -> string) = fun bool -> string_of_int (if bool=zero then 0 else 1)
  end


    
(* BIT VECTOR *)    
    
module Made_Of = functor (Bit : Two_Values_Sig) ->
  (struct

    type bit = Bit.t
    type 'a vector = 'a list
    type 'a vectors = ('a list) list
	  
    type t = bit vector

    let (pretty: bit vector -> string) = fun bits ->
	  Pretty.tuple Bit.pretty bits
	    
    let (print: bit vector -> unit) = fun bits ->
	  print_string (pretty bits)

    let (prettys: (bit vector) list -> string) = fun bits_list ->
	  Pretty.list pretty bits_list
	    
   (* CONVERSION int <-> bit vector (Little endian encoding: the less significant bit is on the left) *) 

    let (nb_bits_for: int -> int) = fun i -> 
	  let rec (nb_bits_rec: (int * int) -> int) = fun (p,n) ->
		if i<= n then p
		else nb_bits_rec (p+1,2*n)
	  in nb_bits_rec (1,2) 
      
    let rec (int_to_bits: int -> t) = 
	  function
	    | 0 -> [ Bit.zero ]
	    | 1 -> [ Bit.unit ]
	    | i ->
		    let r = i mod 2 and half_i = i / 2 in
		      (if r=0 then Bit.zero else Bit.unit) :: (if half_i=0 then [] else int_to_bits half_i)

								
    let (bits_to_int: t -> int option) = fun bits ->
	  let rec
	      (horner: int -> t -> int) = fun int ->
		    function
		      | [] -> int
		      | b::bits ->
			      if b = Bit.zero
			      then horner  (2 * int)    bits
			      else horner ((2 * int)+1) bits
	  in
	    match bits with
	    | [] -> None
	    | _  -> Some (horner 0 (List.rev bits))
		      
    let (unsafe_bits_to_int: t -> int) = fun bits ->
	  match (bits_to_int bits) with
	  | Some int -> int 
	  | None -> assert (false)


		      
   (* OPERATIONS on bit vector *)

   (* The operation +1 *)
		  
    let rec (inc: t -> t) = fun bits ->
	  match bits with
	  | [] -> [Bit.unit]
	  | b::bits ->
		  if b = Bit.zero
		  then Bit.unit :: bits
		  else Bit.zero :: (inc bits)

				    
   (* The operation *2 *)
				
    let (double: t -> t) = fun bits ->
	  Bit.zero :: bits

   (* The operation /2 *)
		 
    let (half: t -> t) = fun bits->
	  List.tl bits


  (* ENUMERATION of bit vectors *)

    let rec (enumerate_from_to: int -> int -> bit vectors) = fun init last ->
	  if init>last
	  then []
	  else (int_to_bits init):: (enumerate_from_to (init+1) last)
	

    (* using enumerate_x_from,  the bit vectors will all have the same size *)
				     
    let rec (enumerate_x_from: int -> bit vector -> bit vectors) = fun counter initial_bit_vector ->
	  if counter<=0 then []
	  else initial_bit_vector :: (enumerate_x_from (counter-1) (inc initial_bit_vector))

    let (enumerate: int -> bit vectors) = fun n ->
	  enumerate_x_from n (List.map (fun _ -> Bit.zero) (int_to_bits (n-1)))
	    
   (* TEST *)
				      
    let (check_operations_on: int -> unit) = fun i ->
	  begin
	    assert ( unsafe_bits_to_int (int_to_bits i) = i ) ;
	    assert ( (inc (int_to_bits i)) = (int_to_bits (i+1)) ) ;
	    assert ( bits_to_int (double (inc (int_to_bits i))) = (Some (2*(i+1))) ) ; 
	    assert ( half (double (int_to_bits i)) = int_to_bits i ) ;
	  end
  
    let (test: unit -> unit) = fun () ->
	  List.iter check_operations_on [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16]

   (* DEMO *)
	    
    open Tricks (* provides >> *)
      
    let (demo: unit -> unit) = fun () ->
	  begin
	    print_string "\n\n* DEMO * Bit_Vector.ml:\n\n"
	      ;
	    [ (enumerate_from_to 3 16) >> prettys ;
	      (enumerate_x_from (16-3+1) [Bit.unit;Bit.unit;Bit.zero;Bit.zero;Bit.zero]) >> prettys ;
	      (enumerate_x_from 8 (int_to_bits 7)) >> prettys ;
	      (enumerate_x_from 8 (List.map (fun _ -> Bit.zero) (int_to_bits 7))) >> prettys ;
	      (enumerate 8) >> prettys	      	      
	    ]
	      >> (String.concat "\n")
	      >> print_string
	  end
  end)

  
(* DEMO *)

module BV = Made_Of(Bit_as_Boolean)

let _ = BV.test () ;;
let _ = BV.demo () ;;


      
