(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Generic pattern matching of symbols with respect to an Alphabet
 * 
 * It provides the constructions
 *  - ANY symbols
 *  - any symbols BUT this symbol
 *  - any symbols IN  a list of symbols
 *  - any symbols OUT of a list of symbols
 *)

open Pretty
open Html
  
type 'a pattern = 
  | ANY
  | VAL of 'a
  | BUT of 'a 
  | IN  of 'a list
  | OUT of 'a list


module Pattern = 
  (struct 

    type 'a t = 'a pattern
	  
    let (exactly: 'a pattern -> 'a -> bool) = fun pattern a -> 
	  pattern = VAL a

    let (matches: 'a pattern -> 'a -> bool) = fun pattern a' ->
	  match pattern with 
	  | ANY -> true
	  | VAL a -> a = a'
	  | BUT a -> a <> a'
	  | IN  aS -> List.mem a' aS
	  | OUT aS -> not (List.mem a' aS)


   (* PRETTY PRINTING *)
		    
   let (to_ascii_wrt: ('a -> string) -> 'a pattern -> string) = fun pp pattern ->
	 match pattern with
	 | ANY -> "?"
	 | VAL a -> pp a
	 | BUT a -> "~" ^ (pp a)
	 | IN  aS -> "{" ^ (String.concat "," (List.map pp aS)) ^ "}"
	 | OUT aS -> "~{" ^ (String.concat "," (List.map pp aS)) ^ "}"


    let (to_html_wrt: ('a -> string) -> Html.options -> 'a pattern -> Html.content) = fun pp _ pattern ->
	  (to_ascii_wrt pp pattern)
	    
    (* user *)

     let (pretty_wrt: ('a -> string) -> 'a t -> string) = fun pp ->
	   match Pretty.get_format() with
	   | Pretty.Html  -> (to_html_wrt pp [])
	   | Pretty.Ascii -> (to_ascii_wrt pp)


    (* TRANSLATION *)
		     
    let (map: ('a -> 'b) -> 'a pattern -> 'b pattern) = fun f pattern ->
	  match pattern with
	  | ANY -> ANY
	  | VAL a -> VAL (f a)
	  | BUT a -> BUT (f a)
	  | IN  aS -> IN (List.map f aS)
	  | OUT aS -> OUT (List.map f aS)


    (* INFORMATION *)
		    
    let (symbols_of: 'a pattern -> 'a list) = fun pattern ->
	  match pattern with
	  | ANY -> []
	  | VAL a -> [a]
	  | BUT a -> [a]
	  | IN  aS -> aS
	  | OUT aS -> aS


   (* ENUMERATION for generic transitions *)
		    
    let (enumerate_symbols_of: 'a list -> 'a pattern -> 'a list) = fun alphabet pattern ->
	  match pattern with
	  | ANY -> alphabet
	  | VAL a -> [a]
	  | BUT a -> MyList.minus alphabet [a]
	  | IN  symbols -> symbols
	  | OUT symbols -> MyList.minus alphabet symbols
	  		    
  end)
