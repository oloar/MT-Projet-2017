(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * A library for pretty printing in Latex | Ascii | Html | Dot 
 *
 * - Required modules:  MyList -> MyString  
 * 
 * - Compilation: ocamlc MyList.cmo MyString.cmo Pretty.ml
 *
 * - Usage: see the Turing Machine project, the Automata project
 *
 *)


module STRING = MyString


(* OUTPUT FORMAT *)

type format = Latex | Ascii | Html | Dot 

type output = { format:format ; width:int }

let _ASCII  = { format=Ascii ; width=200 } 

let _LATEX  = { format=Latex ; width=200 } 

let _HTML   = { format=Html  ; width=200 } 

let _DOT    = { format=Dot   ; width=200 } 


(* GLOBAL VARIABLE *)

let _format_ = ref Html


(* OUTPUT in a file *)

type filename = string
      
let output_in ~(filename:string) ~(content:string) : unit =
  let channel_out = open_out filename 
  in
    begin
      print_string (".....data written in: " ^ filename ^ "\n");
      output_string channel_out content ;
      close_out channel_out ;
    end


	  
(* INFORMATION *)
    
let extension: format -> string = fun format ->
      match format with
      | Latex -> "tex"
      | Html  -> "html"
      | Dot   -> "dot"
      | Ascii -> "txt"

let (get_format: unit -> format) = fun () -> !_format_ ;;

let get_extension ?format:(format=(!_format_)) : unit -> string = fun () ->
      let ext = extension format in
	if ext = "" then ext else "."^ext

      
    
(* SETTINGS *)

let (set_format: format -> unit) = fun format ->
      _format_ := format


(** local settings *)

open Tricks
  
let show ?format:(format=(!_format_)) : ('a delayed_computation) -> 'a = fun delayed_computation ->
      if format = !_format_
      then delayed_computation () 
      else
	let current_format = (!_format_) in
	  begin
	    set_format format ;
	    let result = delayed_computation () in
	      begin
 		set_format current_format ; 
		result 
	      end
	  end

let print ?format:(format=(!_format_)) : string delayed_computation -> unit = fun delayed_computation ->
      show ~format:format (fun unit -> print_string (delayed_computation unit))

	
(* ESCAPE STRING *)

let (string_in_string: string -> string) = STRING.string_in_string ;;

let (make_string: int -> string -> string) = STRING.make ;;


    
(* ASCII FONTS & COLORS *)
    
let (ascii_black: string -> string) = fun string ->
       "\x1B[1;30m" ^ string ^ "\x1B[0m" 	

let (ascii_red: string -> string) = fun string ->
      "\x1B[1;31m" ^ string ^ "\x1B[0m" 

let (ascii_green: string -> string) = fun string ->
       "\x1B[1;32m" ^ string ^ "\x1B[0m" 	
				
let (ascii_yellow: string -> string) = fun string ->
      "\x1B[1;33m" ^ string ^ "\x1B[0m" 

let (ascii_blue: string -> string) = fun string ->
       "\x1B[1;34m" ^ string ^ "\x1B[0m" 	

let (ascii_pink: string -> string) = fun string ->
       "\x1B[1;35m" ^ string ^ "\x1B[0m" 	
				 			
let (ascii_bold: string -> string) = fun string ->
      "\x1B[1m" ^ string ^ "\x1B[0m" 
				
let (ascii_italic: string -> string) = fun string ->
      "\x1B[3m" ^ string ^ "\x1B[0m" 

let (ascii_underline: string -> string) = fun string ->
      "\x1B[4m" ^ string ^ "\x1B[0m" 

let (ascii_strikethrough: string -> string) = fun string ->
      "\x1B[9m" ^ string ^ "\x1B[0m" 
			     
			     

(* LATEX *)

module Latex = 
  (struct 
    let (backslash: string) = "\\"
	
    let (macro: string -> string list -> string) = fun macro_name args ->
	  backslash ^ macro_name ^ (String.concat "" (List.map (fun a -> "{"^a^"}") args))
	    
    let (symbol: string -> string) = fun macro_name -> macro macro_name []

    let (newline:string) = "\n" ^ backslash ^ backslash    

    let (hline:string) = newline ^ (macro "hline" [])

    let (math: string -> string) = fun string -> "$" ^ string ^ "$"

  end)


(* AROUND *)

let par str = "(" ^ str ^ ")"

let spc str = " " ^ str ^ " "

let brk str = "{" ^ str ^ "}"

let lst str = "[" ^ str ^ "]"

let (parentheses: string -> string) = fun str -> "(" ^ str ^ ")"							       
							       

let angle ?format:(format=(!_format_)) : string -> string = fun str ->
      let (l,r) =
	match format with 
	| Latex -> Latex.symbol "langle" , Latex.symbol "rangle"
	| _ -> "<",">"
      in l ^ str ^ r

let brace ?format:(format=(!_format_)) : string -> string = fun str ->
      let (l,r) = 
	match format with 
	| Latex -> Latex.symbol "{" , Latex.symbol "}"
	| _ -> "{","}"
      in l ^ str ^ r

let bracket ?format:(format=(!_format_)) : string -> string = fun str ->
      let (l,r) = 
	match format with 
	| Latex -> Latex.symbol "lg" , Latex.symbol "rg"
	| _ -> "\"","\""
      in l ^ str ^ r


(* FONT MODIFIER *)

let bold ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with 
  | Html  -> String.concat "" [ "<B>" ; string ; "</B>" ] 
  | Latex -> Latex.macro "textbf" [ string ] 
  | _ -> string


let italic ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with 
  | Html  -> String.concat "" [ "<I>" ; string ; "</I>" ] 
  | Latex -> String.concat "" [ "\\textit{" ; string ; "}" ]
  | _ -> string


let subscript  ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with
  | Html-> String.concat "" [ "<SUB>" ; string ; "</SUB>" ]
  | Latex -> String.concat "" [ "$_{" ; string ; "}$" ]
  | _ -> "_" ^ string

		 
let supscript ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with
  | Html-> String.concat "" [ "<SUP>" ; string ; "</SUP>" ]
  | Latex -> String.concat "" [ "$^{" ; string ; "}$" ]
  | _ -> "^" ^ string


let math ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with
  | Latex -> Latex.math string
  | _ -> string
		 

(* ENVIRONMENT *)

let blockquote ?format:(format=(!_format_)) : string -> string = fun string ->
  match format with
  | Html  -> String.concat "" [ "<BLOCKQUOTE>" ; string ; "</BLOCKQUOTE>" ]
  | Latex -> String.concat "" [ "\\begin{quote}\n"; string ; "\n\\end{quote}\n" ]
  | _ -> string


let enumerate ?format:(format=(!_format_)) : string list -> string = fun strings ->
  match format with
  | Html -> 
	  String.concat "" 
	    [ "<OL>\n" 
            ; String.concat "" (List.map (fun str -> "<LI>" ^ str ^ "</LI>\n") strings)
	    ; "</OL>\n"
	    ] 
  | Latex ->   
	  String.concat "" 
	    [ "\\begin{enumerate}" 
	    ; String.concat "" (List.map (fun str -> "\n\\item" ^ str ) strings)
	    ; "\n\\end{enumerate}\n" 
	    ]
  | _ -> 
	  String.concat "\n" strings


(* NEWLINE, SPACES *)

let newline ?format:(format=(!_format_)) : unit -> string = fun () ->
      match format with
      | Latex -> "\n\\  \n"
      | Html  -> "<P></P>  "
      | _ -> "\n  "


let space ?format:(format=(!_format_)) : unit -> string = fun () ->
      match format with
      |	Latex -> "~"
      |	Html  -> "&nbsp;"
      |	_ -> " "

let (spaces: int -> string) = fun n ->
      let one_space = space () 
      in make_string n one_space
	  
let (tabulation: int -> string) = fun n ->
      (newline ()) ^ (spaces n)


(* TYPE *)

module Type = 
  (struct
    let bool ?format:(format=(!_format_)) : bool -> string = fun b ->
	  let b_as_string = if b then "true" else "false"
	  in
	    match format with 
	    | Latex -> Latex.symbol b_as_string
	    | _ -> b_as_string
		      
    let (int: int -> string) = string_of_int

    let (aligned_integer: int -> int -> string) = fun maxint int ->
	  let nb_digit = String.length (string_of_int maxint) in
	    let int_as_string = string_of_int int in
	      let n = (String.length int_as_string) - nb_digit in
		(String.make n ' ') ^ int_as_string
	    
    let (filled_integer: int -> int -> string) = fun maxint int ->
	  let nb_digit = String.length (string_of_int maxint) in
	    let int_as_string = string_of_int int in
	      let n = (String.length int_as_string) - nb_digit in
		(String.make n '0') ^ int_as_string

  end)




(* coloring *)

type color_name = string (* inside "  " *)

type lumen = Dark | Light 
    
let _dvips_colors = [ ("GreenYellow",Light) ; ("Yellow",Light) ; ("Goldenrod",Light) ; ("Dandelion",Light) ; ("Apricot",Light) ; ("Peach",Light) ; ("Melon",Light) ; ("YellowOrange",Light) ; ("Orange",Light) ; ("BurntOrange",Light) ; ("Bittersweet",Light) ; ("RedOrange",Light) ; ("Mahogany",Light) ; ("Maroon",Light) ; ("BrickRed",Light) ; ("Red",Light) ; ("OrangeRed",Light) ; ("RubineRed",Light) ; ("WildStrawberry",Light) ; ("Salmon",Light) ; ("CarnationPink",Light) ; ("Magenta",Light) ; ("VioletRed",Light) ; ("Rhodamine",Light) ; ("Mulberry",Light) ; ("RedViolet",Light) ; ("Fuchsia",Light) ; ("Lavender",Light) ; ("Thistle",Light) ; ("Orchid",Light) ; ("LightOrchid",Light) ; ("Purple",Light) ; ("Plum",Light) ; ("Violet",Light) ; ("RoyalPurple",Light) ; ("BlueViolet",Light) ; ("Periwinkle",Light) ; ("CadetBlue",Light) ; ("CornflowerBlue",Light) ; ("MidnightBlue",Light) ; ("NavyBlue",Light) ; ("RoyalBlue",Light) ; ("Blue",Light) ; ("Cerulean",Light) ; ("Cyan",Light) ; ("ProcessBlue",Light) ; ("SkyBlue",Light) ; ("Turquoise",Light) ; ("TealBlue",Light) ; ("Aquamarine",Light) ; ("BlueGreen",Light) ; ("Emerald",Light) ; ("JungleGreen",Light) ; ("SeaGreen",Light) ; ("Green",Light) ; ("ForestGreen",Light) ; ("PineGreen",Light) ; ("LimeGreen",Light) ; ("YellowGreen",Light) ; ("SpringGreen",Light) ; ("OliveGreen",Light) ; ("RawSienna",Light) ; ("Sepia",Light) ; ("Brown",Light) ; ("Tan",Light) ; ("Gray",Light) ]

let _html_colors = [ ("AliceBlue",Light) ; ("AntiqueWhite",Light) ; ("Aqua",Light) ; ("Aquamarine",Light) ; ("Azure",Light) ; ("Beige",Light) ; ("Bisque",Light) ; ("Black",Light) ; ("BlanchedAlmond",Light) ; ("Blue",Light) ; ("BlueViolet",Light) ; ("Brown",Light) ; ("BurlyWood",Light) ; ("CadetBlue",Light) ; ("Chartreuse",Light) ; ("Chocolate",Light) ; ("Coral",Light) ; ("CornflowerBlue",Light) ; ("Cornsilk",Light) ; ("Crimson",Light) ; ("Cyan",Light) ; ("LightBlue",Light) ; ("LightCyan",Light) ; ("LightGoldenRod",Light) ; ("LightGray",Light) ; ("LightGrey",Light) ; ("LightGreen",Light) ; ("LightKhaki",Light) ; ("LightMagenta",Light) ; ("LightOliveGreen",Light) ; ("Lightorange",Light) ; ("LightOrchid",Light) ; ("LightRed",Light) ; ("LightSalmon",Light) ; ("LightSeaGreen",Light) ; ("LightSlateBlue",Light) ; ("LightSlateGray",Light) ; ("LightSlateGrey",Light) ; ("LightTurquoise",Light) ; ("LightViolet",Light) ; ("DeepPink",Light) ; ("DeepSkyBlue",Light) ; ("DimGray",Light) ; ("DimGrey",Light) ; ("DodgerBlue",Light) ; ("FireBrick",Light) ; ("FloralWhite",Light) ; ("ForestGreen",Light) ; ("Fuchsia",Light) ; ("Gainsboro",Light) ; ("GhostWhite",Light) ; ("Gold",Light) ; ("GoldenRod",Light) ; ("Gray",Light) ; ("Grey",Light) ; ("Green",Light) ; ("GreenYellow",Light) ; ("HoneyDew",Light) ; ("HotPink",Light) ; ("IndianRed",Light) ; ("Indigo",Light) ; ("Ivory",Light) ; ("Khaki",Light) ; ("Lavender",Light) ; ("LavenderBlush",Light) ; ("LawnGreen",Light) ; ("LemonChiffon",Light) ; ("LightBlue",Light) ; ("LightCoral",Light) ; ("LightCyan",Light) ; ("LightGoldenRodYellow",Light) ; ("LightGray",Light) ; ("LightGrey",Light) ; ("LightGreen",Light) ; ("LightPink",Light) ; ("LightSalmon",Light) ; ("LightSeaGreen",Light) ; ("LightSkyBlue",Light) ; ("LightSlateGray",Light) ; ("LightSlateGrey",Light) ; ("LightSteelBlue",Light) ; ("LightYellow",Light) ; ("Lime",Light) ; ("LimeGreen",Light) ; ("Linen",Light) ; ("Magenta",Light) ; ("Maroon",Light) ; ("MediumAquaMarine",Light) ; ("MediumBlue",Light) ; ("MediumOrchid",Light) ; ("MediumPurple",Light) ; ("MediumSeaGreen",Light) ; ("MediumSlateBlue",Light) ; ("MediumSpringGreen",Light) ; ("MediumTurquoise",Light) ; ("MediumVioletRed",Light) ; ("MidnightBlue",Light) ; ("MintCream",Light) ; ("MistyRose",Light) ; ("Moccasin",Light) ; ("NavajoWhite",Light) ; ("Navy",Light) ; ("OldLace",Light) ; ("Olive",Light) ; ("OliveDrab",Light) ; ("Orange",Light) ; ("OrangeRed",Light) ; ("Orchid",Light) ; ("PaleGoldenRod",Light) ; ("PaleGreen",Light) ; ("aleTurquoise",Light) ; ("PaleVioletRed",Light) ; ("PapayaWhip",Light) ; ("PeachPuff",Light) ; ("Peru",Light) ; ("Pink",Light) ; ("Plum",Light) ; ("PowderBlue",Light) ; ("Purple",Light) ; ("Red",Light) ; ("RosyBrown",Light) ; ("RoyalBlue",Light) ; ("SaddleBrown",Light) ; ("Salmon",Light) ; ("SandyBrown",Light) ; ("SeaGreen",Light) ; ("SeaShell",Light) ; ("Sienna",Light) ; ("Silver",Light) ; ("SkyBlue",Light) ; ("SlateBlue",Light) ; ("SlateGray",Light) ; ("SlateGrey",Light) ; ("Snow",Light) ; ("SpringGreen",Light) ; ("SteelBlue",Light) ; ("Tan",Light) ; ("Teal",Light) ; ("Thistle",Light) ; ("Tomato",Light) ; ("Turquoise",Light) ; ("Violet",Light) ; ("Wheat",Light) ; ("White",Light) ; ("WhiteSmoke",Light) ; ("Yellow",Light) ; ("YellowGreen",Light) ]

let _svg_colors = [ ("aliceblue",Light) ; ("antiquewhite",Light) ; ("aqua",Light) ; ("aquamarine",Light) ; ("azure",Light) ;
("beige",Light) ; ("bisque",Light) ; ("black",Light) ; ("blanchedalmond",Light) ; ("blue",Light) ;
("blueviolet",Light) ; ("brown",Light) ; ("burlywood",Light) ; ("cadetblue",Light) ; ("chartreuse",Light) ;
("chocolate",Light) ; ("coral",Light) ; ("cornflowerblue",Light) ; ("cornsilk",Light) ; ("crimson",Light) ;
("   cyan   ",Light) ; ("darkblue",Light) ; ("darkcyan",Light) ; ("darkgoldenrod",Light) ; ("darkgray",Light) ;
("darkgreen",Light) ; ("darkgrey",Light) ; ("darkkhaki",Light) ; ("darkmagenta",Light) ; ("darkolivegreen",Light) ;
("darkorange",Light) ; ("darkorchid",Light) ; ("darkred",Light) ; ("darksalmon",Light) ; ("darkseagreen",Light) ;
("darkslateblue",Light) ; ("darkslategray",Light) ; ("darkslategrey",Light) ; ("darkturquoise",Light) ; ("darkviolet",Light) ;
("deeppink",Light) ; ("deepskyblue",Light) ; ("dimgray",Light) ; ("dimgrey",Light) ; ("dodgerblue",Light) ;
("firebrick",Light) ; ("floralwhite",Light) ; ("forestgreen",Light) ; ("fuchsia",Light) ; ("gainsboro",Light) ;
("ghostwhite",Light) ; ("   gold   ",Light) ; ("goldenrod",Light) ; ("   gray   ",Light) ; ("   grey   ",Light) ;
("green",Light) ; ("greenyellow",Light) ; ("honeydew",Light) ; ("hotpink",Light) ; ("indianred",Light) ;
("indigo",Light) ; ("ivory",Light) ; ("khaki",Light) ; ("lavender",Light) ; ("lavenderblush",Light) ;
("lawngreen",Light) ; ("lemonchiffon",Light) ; ("lightblue",Light) ; ("lightcoral",Light) ; ("lightcyan",Light) ;
("lightgoldenrodyellow",Light) ; ("lightgray",Light) ; ("lightgreen",Light) ; ("lightgrey",Light) ; ("lightpink",Light) ;
("lightsalmon",Light) ; ("lightseagreen",Light) ; ("lightskyblue",Light) ; ("lightslategray",Light) ; ("lightslategrey",Light) ;
("lightsteelblue",Light) ; ("lightyellow",Light) ; ("   lime   ",Light) ; ("limegreen",Light) ; ("linen",Light) ;
("magenta",Light) ; ("maroon",Light) ; ("mediumaquamarine",Light) ; ("mediumblue",Light) ; ("mediumorchid",Light) ;
("mediumpurple",Light) ; ("mediumseagreen",Light) ; ("mediumslateblue",Light) ; ("mediumspringgreen",Light) ; ("mediumturquoise",Light) ; ("mediumvioletred",Light) ; ("midnightblue",Light) ; ("mintcream",Light) ; ("mistyrose",Light) ; ("moccasin",Light) ;
("navajowhite",Light) ; ("   navy   ",Light) ; ("oldlace",Light) ; ("olive",Light) ; ("olivedrab",Light) ;
("orange",Light) ; ("orangered",Light) ; ("orchid",Light) ; ("palegoldenrod",Light) ; ("palegreen",Light) ;
("paleturquoise",Light) ; ("palevioletred",Light) ; ("papayawhip",Light) ; ("peachpuff",Light) ; ("   peru   ",Light) ;
("   pink   ",Light) ; ("   plum   ",Light) ; ("powderblue",Light) ; ("purple",Light) ; ("   red   ",Light) ;
("rosybrown",Light) ; ("royalblue",Light) ; ("saddlebrown",Light) ; ("salmon",Light) ; ("sandybrown",Light) ;
("seagreen",Light) ; ("seashell",Light) ; ("sienna",Light) ; ("silver",Light) ; ("skyblue",Light) ;
("slateblue",Light) ; ("slategray",Light) ; ("slategrey",Light) ; ("   snow   ",Light) ; ("springgreen",Light) ;
("steelblue",Light) ; ("   tan   ",Light) ; ("   teal   ",Light) ; ("thistle",Light) ; ("tomato",Light) ;
("turquoise",Light) ; ("violet",Light) ; ("wheat",Light) ; ("white",Light) ; ("whitesmoke",Light) ;
("yellow",Light) ; ("yellowgreen",Light) ]

let _X11_colors = []

let select_color  ?format:(format=(!_format_)) : int -> string * lumen = fun i -> 
  let select_in = fun colors -> List.nth colors (i mod (List.length colors))
  in
    match format with
    | Html  -> select_in _html_colors
    | Latex -> select_in _dvips_colors
    | Dot   -> select_in _svg_colors
    | _     -> ("",Light)

let (get_text_color_for_bg: int -> color_name) = fun i -> 
      match select_color i with
      | (_,Dark)  -> string_in_string "white"
      |	(_,Light) -> string_in_string "black"

let (get_color: int -> color_name) = fun i ->
      string_in_string (fst (select_color i))

let text_color ?format:(format=(!_format_)) : int -> string -> string = fun i text ->
      match format with
      | Html-> String.concat "" [ "<FONT color=" ; get_color i ; ">" ; text ; "</FONT>" ] 
      |	Latex -> String.concat "" [ "\\textcolor{" ; get_color i ; "}{" ; text ; "}" ]
      |	Dot -> String.concat "" [ "[labelcolor=" ; get_color i ; ", label=" ; string_in_string text ; "]" ]
      |	_ -> text

let background_color ?format:(format=(!_format_)) : int -> string -> string = fun i text ->
      match format with
      | Html-> String.concat "" [ "<CELL bgcolor=" ; get_color i ; "><FONT color =" ; get_text_color_for_bg i ; ">" ; text ; "</FONT></CELL>" ] 
      |	Latex -> String.concat "" [ "\\textcolor{" ; get_color i ; "}{" ; text ; "}" ]
      |	Dot -> String.concat "" [ text ; "[style=filled, fillcolor=" ; get_color i ; ", fontcolor=" ; get_text_color_for_bg i ; "]" ]
      |	_ -> text
      

let (* OLD *) color ?format:(format=(!_format_)) : string -> string -> string = fun color_name text ->
      match format with
      | Html-> String.concat "" [ "<FONT color=" ; color_name ; ">" ; text ; "</FONT>" ] 
      |	Latex -> String.concat "" [ "\\textcolor{" ; color_name ; "}{" ; text ; "}" ]
      |	_ -> text


    
(* to be clean up *)

let error msg = "\n\n ** error:" ^ msg ^ " **\n\n"

let info msg = " ** " ^ msg ^ " ** "

let strings = String.concat ""

let list_generic  begSymb  separator  endSymb  pretty_elt l = 
  begSymb ^ (String.concat separator (List.map pretty_elt l)) ^ endSymb

let set pp = list_generic "{" "," "}" pp

let list_newline pp = list_generic "\n[ " "\n; " "\n]\n" pp

let list pp = list_generic "[" "; " "]" pp

let tuple pp = list_generic "(" "," ")" pp

let apply pp = list_generic " (" " " ") " pp

let apply_op op args = apply (fun x->x) (op::args)

let apply_lisp strs = par (String.concat " " strs)

let apply_fun pp f args = if args = [] then f else (apply_lisp (f::(List.map pp args)))

let op_com_ass pp op_str = function
  | [a] -> op_str ^ (pp a)
  | args -> list_generic "" op_str "" pp args

let op pp op_str = function
  | [a] -> op_str ^ (pp a)
  | [a1;a2] -> ( (pp a1) ^ op_str ^ (pp a2))
  | args -> list_generic "(" op_str ")" pp args

let rec op_right_ass pp op_str = function
  | [] -> ""
  | x::xs -> apply_op op_str [ pp x ; op_right_ass pp op_str xs ]


