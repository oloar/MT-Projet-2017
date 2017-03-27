(* COMPILATION
   ocamlc -w p common.cmo hof.cmo either.cmo myList.cmo myString.ml
*)

module LIST = MyList

    type letter = string (* string of length = 1 *)

    let (head: string -> string) = 
      fun string -> String.sub string 0 1

    let (tail: string -> string) = 
      fun string -> String.sub string 1 ((String.length string)-1)

    let rec (string_to_letters: string -> letter list) = 
      fun string ->
	    if string = "" 
	    then []
	    else (head string) :: (string_to_letters (tail string))
				   
    let (string_in_string: string -> string) = fun string -> "\"" ^ string ^ "\""

    type t = string 
    
    let default = "" 
    
    let (make: int -> string -> string) = fun n str -> String.concat "" (LIST.make n str)

    let compare = Pervasives.compare
    
    let pretty str = str

    let string_to_list_of_char str = (* FIXME: to be tested *)
      let last = (String.length str)-1
      in
	let rec string_to_list_rec n =
	  if n=last 
	  then []
	  else (String.get str n) :: (string_to_list_rec (n+1)) 
      in 
	  string_to_list_rec 0

   let (char_list_to_string: char list -> string) = fun charS ->
	 let string = Bytes.create (List.length charS) in 
	   begin
	     MyList.iter_counter 0 (fun i c -> Bytes.set string i c) charS ;
	     string 
	   end


    let rev s = 
      let l = String.length s 
      in let last = l-1 in 
	if l = 0 then s
	else
	  begin
	    let r = Bytes.create l in
	      for i = 0 to last do
		Bytes.set r (last-i) (String.unsafe_get s i)
	      done ;
	      r
	  end

    let map f s =
      let l = String.length s in
	if l = 0 then s
	else
	  begin
	    let r = Bytes.create l in
	      for i = 0 to l - 1 do
		Bytes.set r i (f (String.unsafe_get s i))
	      done ;
	      r
	  end


    let separate_with separator stringS =
      let rec rec_separate_with sep = function
	| []  -> ""
	| s::strings ->
		if (String.length s)!=0 
		then sep ^ s ^ (rec_separate_with separator strings)
		else (rec_separate_with sep strings)
      in  rec_separate_with "" stringS

    let pretty_print_separated_with separator pp eltS = 
      separate_with separator (List.map pp eltS)

    let concat_with  beg  sep  end' strings = 
      let core = separate_with sep strings
      in if core="" then "" else beg ^ core ^ end'
		    
    let concat_surrounded_by  sep  beg  end' = 
      concat_with beg (end'^sep^beg) end' 

    let forget_first n str = String.sub str n ((String.length str)-n) 

    let forget_last n str = String.sub str 1 ((String.length str)-n) 
 
    let split_at s c =
      let ic = try String.index s c with Not_found -> ~-1
      and lg = String.length s
      in
	if ic < 0
	then (s,"")
	else (String.sub s 0 ic, String.sub s (ic+1) (lg-(ic+1)))

    (* EXAMPLES
       split_at "abcdef" 'b' ;; = ("a","cdef")
       split_at "abcdef" 'Z' ;; = ("acdef","")
     *)

    let split_before s c =
      let ic = try String.index s c with Not_found -> ~-1
      and lg = String.length s
      in
	if ic < 0
	then (s,"")
	else
	  if ic = 0
	  then ("",s)
	  else (String.sub s 0 ic, String.sub s ic (lg-ic))

    let before c s = fst (split_at s c)

    let split_after s c = 
      let ic = try String.index s c with Not_found -> ~-1
      and lg = String.length s
      in
	if ic < 0
	then ("",s)
	else
	  (String.sub s 0 (ic+1), String.sub s (ic+1) (lg-(ic+1)))

    let after c s = snd (split_after s c)

    let decomp_on c s = 
      let rec decomp_on_rec acc s =
	let (s1,s2) = split_at s c 
	in if s2 = "" then s1::acc else decomp_on_rec (s1::acc) s2
      in
	List.rev (decomp_on_rec [] s)

    let list_to_string stringS = String.concat "" stringS

    let string_to_list str = 
      let max = (String.length str)-1
      in
	let rec f n l =
	  if (n < 0) 
	  then l
	  else let c = String.make 1 (String.get str n) in f (n-1) (c::l)
	in f max []


    let replace_by_in oldS newS str = 
      let oldL = string_to_list oldS
      and newL = string_to_list newS
      and strL = string_to_list str
      in
	String.concat "" (LIST.replace_by_in oldL newL strL)


    let substitute replacements str = 
      LIST.forEachIn 
	replacements 
	(fun (str1,str2) -> replace_by_in str1 str2) 
	str


    let unquote str = String.sub str 1 ((String.length str)-2) 

    let quote str = "\"" ^ (replace_by_in "\"" "\\\"" str) ^ "\""

    (* letter order: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz *)

    let is_letter c = ('A' <= c) && ( c <= 'z') && (not (List.mem c ['\\' ; '_' ; '`' ; '^' ; '[' ; ']' ]))

(* TEST OF: is_letter  
   let rec zip lx ly =
      match (lx,ly) with
      | ([],_) 
      | (_,[]) -> []
      | (x::xs,y::ys)   -> (x,y)::(zip xs ys) ;;
	
   let l = ['-';'/';'|';'\\'; '_';':';';';'\'';'<';'>';'.';'`';'~';'!';'@';'#';'$';'^';'&';'*';'+';'=';'?';'%';','; ';' ; '[' ;']']
   in zip l (List.map is_letter l) ;;

* TEST *)

    let output_with sep stringS = print_string ("\n" ^ sep ^ (String.concat "" stringS) ^ sep)

    let output stringS = output_with "\n" stringS


(* TEST:
   
   *** CONCAT ***

   Ext.STRING.concat_with "d" "s" "f" ["1";"2";"3";"4";"5"] ;;
   Ext.STRING.concat_with "(" "," ")" ["1";"2";"3";"4";"5"] ;;
   Ext.STRING.concat_surrounded_by "(" ")" ["1";"2";"3";"4";"5"] ;;
*)
