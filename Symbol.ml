(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project: TURING MACHINES FOR REAL
 *
 * - Symbols of the alphabet used by the TM 
 *
 * - Required modules: Pretty Graphics -> Color -> Html 
 *
 * - Compilation: ocaml Pretty.cmo Graphics.cma Color.cmo Html.cmo Symbol.ml
 *)

    
type symbol =
  | B (* Blank *)
  | D (* Dark  *)

  | U (* the 1 bit *)
  | Z (* the 0 bit *)
  | S (* a separator *)
      
 (* additional symbols for simulating the beta-reducton with MT : REQUIRED by LC_by_MT *)

  | L (* lambda *)
  | O (* the opening parenthesis *)
  | C (* the closing parenthesis *)
  | X (* the variable symbol followed by a identifier as a sequence of bits *)
      
  | V of (string * int) (* /!\ a infinite number of symbols: This is cheating, but convenient *)

 (* For simulating a Sigma Turing Machine by a Binary Turing Machine *)

  | Vector of symbols 

 (* For simulation of a k-Bands Turing Machine by a One-Band Turing Machine *)
 	
  | Column of symbols

and symbols = symbol list
      


module Symbol =
  (struct

    type t = symbol      

    let (compare: symbol -> symbol -> int) = Pervasives.compare

    (* PRETTY PRINTING *)
	
    let rec (verbatim: t -> string) =
      function
	| B -> "B"
	| U -> "U"
	| Z -> "Z"
	| D -> "D"
	| L -> "L"	      
	| O -> "O"
	| C -> "C"
	| X -> "X"
	| V(string,int) -> String.concat "" [ "V" ; Pretty.parentheses (string ^ "," ^ (string_of_int int)) ]
	| Vector symbols  -> String.concat "" [ "Vector" ; Pretty.bracket (String.concat ";" (List.map verbatim symbols)) ]
	| Column symbols  -> String.concat "" [ "Column" ; Pretty.bracket (String.concat ";" (List.map verbatim symbols)) ]		    
	
    (* ascii output *)
	
    let rec (to_ascii: symbol -> string) =
      function
	| B -> "_"
	| U -> "U"
	| Z -> "Z"
	| D -> "$"
	| S -> "#"
		  
	| L -> "L"	      
	| O -> "("
	| C -> ")"
	| X -> "x"
	| V(string,int) -> string ^ (if int<0 then "" else string_of_int int)

	| Vector symbols -> Pretty.parentheses (String.concat "," (List.map to_ascii symbols))
		  
	| Column symbols -> Pretty.bracket (String.concat "|" (List.map to_ascii symbols))
		  
		  
   (* html output *)
		    
    let (color: symbol -> Color.t * Color.t) = fun symbol ->
	  match symbol with (* back_ground_color, font_color *)
	  | B -> (Color.white , Color.white)
	  | Z -> (Color.blue  , Color.yellow)
	  | U -> (Color.yellow, Color.blue)
	  | D -> (Color.black , Color.black)
	  | S -> (Color.red   , Color.red)		    
	  | _ -> (Color.white , Color.black)
		    
    let (ft_color: symbol -> Color.t) = fun symbol -> snd (color symbol)
    let (bg_color: symbol -> Color.t) = fun symbol -> fst (color symbol)


    let rec (to_html: Html.options -> symbol -> Html.cell) = fun options symbol ->
	  match symbol with
	  | Vector symbols -> Html.tuple  options (List.map (to_html []) symbols)
	  | Column symbols -> Html.column options (List.map (to_html []) symbols)
	  | _ ->
		  Html.cell [ ("align", Html.Option "center") ; ("bgcolor", Html.Color (bg_color symbol)) ]
		    (Html.font [ ("color", Html.Color (ft_color symbol)) ]
		       (Html.bold (to_ascii symbol)))
		    

    (* user *)
      
    let (pretty: t -> string) = fun t ->
	  match Pretty.get_format() with
	  | Pretty.Html  -> to_html [] t
	  | Pretty.Ascii -> to_ascii t
		    
end)
