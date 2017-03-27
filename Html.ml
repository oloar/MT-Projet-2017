(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * A library for generating html output	
 *
 * - Required modules: Tricks MyList Graphics -> Color
 *
 * - Compilation:  ocamlc Tricks.cmo MyList.cmo Graphics.cma Color.cmo html.ml
 *)

open Tricks

(* TYPES *)
    
type content = string
type cell = content
type cells = content list
type row = content
type rows = row list      
type table = content

type valeur =
  | Int of int
  | Option of string
  | Color of Color.t
	      
type options = (string * valeur) list

(* UNUSED ? USEFUL ?
type descriptor = {
    width: int option ;
    height: int option ;
    bgcolor: Color.t option ;
    ftcolor: Color.t option  ;
    font_size: int option ;
    align: string  option ;
    valign: string option 
  }
      
let (descriptor: descriptor) = {
  width  = Some 10 ;
  height = Some 10 ;
  bgcolor = None ;
  ftcolor = None ;
  font_size = None ;
  align  = Some "center" ;
  valign = Some "center" ;
}     
 *)

      


let (concat: content list -> content) = String.concat "\n"
    
let (valeur_to_string: valeur -> string) = fun valeur ->
      match valeur with
      | Int i -> string_of_int i
      | Option s -> s
      | Color c -> Color.color_to_html c


(* OPTIONS:  algin=center  color="red" *)
		
let (process_options: options -> string) = fun options ->
  if options = []
  then ""
  else
    let string =
      options
      >> (List.map (fun (name,valeur) -> name ^ "=" ^ (valeur_to_string valeur)))
      >> (String.concat " ")
    in " " ^ string


(* ENVIRONMENT: <TAG options> content </TAG> *)	       

let (environment: string * string * string -> options -> content -> content) = fun (before_mark,mark,after_mark) options content ->
	String.concat "" [ before_mark ; "<" ^ mark ^ (process_options options) ^ ">" ; after_mark ;
			   content ; after_mark ;
			   before_mark ; "</" ^ mark ^ ">" ; after_mark ]


(* TABLE *)
	
(* table cell: <TD option> content </TD> *)
	
let (cell: options -> content -> cell) = environment ("  ","TD","")
    
let (wide_cell: int -> int -> cell) = fun width n ->
      if n>0
      then cell [ ("COLSPAN", Int n) ; ("bgcolor", Option "white") ] ""
      else ""

let (old_wide_cell: int -> int -> cell) = fun width n ->
      if n>0
      then (cell [("width", Int width) ; ("bgcolor", Option "orange") ] "") >> (MyList.make n) >> (String.concat "")
      else ""

(* table: <TABLE option> rows </TABLE> *)
	  
let (table: options -> rows -> table) = fun options rows ->
      environment ("","TABLE","\n") options (concat rows)

(* table row: <TR option> cells </TR> *)
	
let (row: options -> cell list -> row) = fun options cells ->
      environment (" ","TR","\n") options (concat cells)

(* column = a table of one column *)

let (column: options -> cell list -> table) = fun options cells ->
      let
	  (one_cell_row:cell -> row) = fun cell -> row [] [cell] 
      in
	table options (List.map one_cell_row cells)

(* tuple = a table of one row inside a cell *)
	  
let (tuple: options -> cell list -> cell) = fun options cells ->
      cell options
	(table [ ("border", Int 1) ] 
	   [row [] cells]
	)
	
	
(* FONT *)	  
	  
let (font: options -> content -> content)  = environment ("","FONT","")

let (italic: content -> content) = environment ("","I","") []

let (bold: content -> content) = environment ("","B","") [] 
