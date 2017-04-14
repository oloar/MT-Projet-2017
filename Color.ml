(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Mars 2017 
 *
 * library of predefined colors and color format conversion
 *
 * REQUIRES 
 *   Pretty.cmo
 *
 *)

type rgb = int * int * int 
  
type color = 
  | RGB of rgb 
  | COL of string
  
type t = color


(* RGB COLORS *)
      
let (rgb_to_int: rgb -> int) = fun (r,g,b) ->
     256 * (256 * r + g) + b 

let (int_to_rgb: int -> rgb) = fun c ->
      let r = c / 65536 and g = (c / 256) mod 256 and b = c mod 256 
      in (r,g,b)
  
let (rgb_black:  rgb) = (0,0,0)
let (rgb_blue:   rgb) = (0,0,255)
let (rgb_cyan:   rgb) = (0,255,255)
let (rgb_green:  rgb) = (0,255,0)
let (rgb_gray:   rgb) = (125,125,125)
let (rgb_indigo: rgb) = (75,0,130)
let (rgb_magenta:rgb) = (255,0,255)
let (rgb_orange: rgb) = (229,83,0)
let (rgb_red:    rgb) = (255,0,0)
let (rgb_white:  rgb) = (255,255,255)
let (rgb_yellow: rgb) = (255,255,0)

(* COLOR MAP *)
  
let rec zip: 'x list -> 'y list -> ('x * 'y) list = fun xs ys ->
      match (xs,ys) with
      | ([],_) | (_,[]) -> []
      | (x::xs,y::ys)   -> (x,y)::(zip xs ys)

let _COLOR_MAP_: (string * rgb) list =
  zip
    [    "black";    "blue";    "cyan";    "gray"; "green";    "indigo";    "magenta";    "orange";    "red";    "white";   "yellow" ]
    [ rgb_black ; rgb_blue ; rgb_cyan ; rgb_gray ; rgb_green ; rgb_indigo ; rgb_magenta ; rgb_orange ; rgb_red ; rgb_white ; rgb_yellow ]
  
let (color_to_rgb: color -> int * int * int) = fun color ->
      match color with
      | RGB rgb -> rgb
      | COL name -> List.assoc name _COLOR_MAP_


(* PREDEFINED COLOR *)    

let (black:  color) = COL "black"
let (white:  color) = COL "white"
let (magenta:color) = COL "magenta"
let (cyan:   color) = COL "cyan"
let (blue:   color) = COL "blue"
let (yellow: color) = COL "yellow"
let (red:    color) = COL "red"
let (green:  color) = COL "green"
let (gray:   color) = RGB rgb_gray
let (orange: color) = COL "orange"
let (purple: color) = COL "purple"(*RGB rgb_orange*)
let (indigo: color) = RGB rgb_indigo
let (azure: color) = COL "azure"
let (golden: color) = COL "Goldenrod"
let (peach: color) = COL "Peach"
let(melon: color) = COL "Melon"
let (maroon: color) = COL "Maroon"
let(sepia: color) = COL "Sepia"
let(brown: color) = COL "Brown"

     
let (default:color) = black

    
let rec (random_color: unit -> color) = fun () ->
      let r = Random.int 255 
      and g = Random.int 255 
      and b = Random.int 255 
      in let color = RGB (r,g,b) 
      in if color = white then random_color () else color

    
let (inv_rgb_color: color -> color) = fun color ->
      let (r,g,b) = color_to_rgb color 
      in RGB (255-r , 255-g, 255-b)
    

let (rgb_to_string: rgb -> string) = fun (r,g,b) ->
      String.concat "" (List.map (Pretty.Type.filled_integer 255) [r;g;b])

    
let (color_to_html: color -> string) = fun color ->
      match color with
      | RGB rgb -> "#" ^ (rgb_to_string rgb)
      | COL name -> name