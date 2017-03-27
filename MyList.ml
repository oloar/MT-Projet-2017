(* COMPILATION
   ocamlc -w p common.cmo hof.cmo either.cmo myList.ml
*)

(* open Common *)
(* open Hof *)
(* open Either  *)


let rec (iterate: int -> ('a -> 'a) -> 'a -> 'a) = fun n f acc ->
      if (n<=0) 
      then acc 
      else iterate (n-1) f (f acc) 

      
let rec map_fold_right neutral op f = function
  | [] -> neutral
  | x::xs -> op (f x) (map_fold_right neutral op f xs)

(* map_fold_right f (+) 0 [1;2;3] =  (f 1) + ((f 2) + ((f 3) + 0)) *)

let rec map_fold_left acc op f = function
  | [] -> acc
  | x::xs -> let acc' = op (f x) acc in map_fold_left acc' op f xs

(* map_fold_left 0 (-) f [1;2;3] =  ((0 - (f 1)) - (f 2)) - (f 3) *)
	

let forall_in: 't list -> ('t -> bool) -> bool = fun l p -> List.for_all p l
	
let exists_in: 't list -> ('t -> bool) -> bool = fun l p -> List.exists p l 

let (optional_find: ('t -> bool) -> 't list -> 't option) = fun p l ->
      try Some (List.find p l) with Not_found -> None 

let (foreach_in: 'x list -> ('x -> 'y list) -> 'y list) = fun xs f ->
      List.fold_right (fun x ys -> (f x) @ ys) xs []

let (foreach_st: ('x -> bool) -> 'x list -> ('x -> 'y list) -> 'y list) = fun pred xs f ->
      foreach_in (List.filter pred xs) f

let (do_foreach_in: 't list -> ('t -> unit) -> unit) = 
  fun l instruction -> List.iter instruction l

    let rec (iter_counter: int -> (int -> 't -> unit) -> 't list -> unit) = fun i action ts ->
      match ts with
      | [] -> ()
      |	t::ts -> begin (action i t) ; iter_counter (i+1) action ts end

    let rec (numbering_from: int -> 't list -> ('t * int) list) = 
      fun i -> function 
	| [] -> []
	| t::ts -> (t,i) :: (numbering_from (i+1) ts)

    let (minimum: int list -> int) = 
      function x::xs -> List.fold_left min x xs 
	      
    let (maximum: int list -> int) = 
      function x::xs -> List.fold_left max x xs 
	      
    let (prettys: string -> string -> string -> ('t -> string) -> 't list -> string) =
      fun deb sep fin pretty_elt elts -> 
	    deb ^ (String.concat sep (List.map pretty_elt elts)) ^ fin

    let (pretty_list: ('t -> string) -> 't list -> string) = fun pp l ->
	  prettys "[" ";" "]" pp l 

let disjunction  = List.exists (fun b -> b)
	
let conjunction = List.for_all (fun b -> b)
	
let bool_op op l = List.fold_right (fun b acc -> op b acc) l (op true false)

    let equal_with equal lx ly = 
      let rec equal_with_REC = function
	| []   ,[]    -> true
	| x::xs,y::ys -> if equal x y then equal_with_REC (xs,ys) else false
	| _,_         -> false
      in equal_with_REC (lx,ly)
	
    let nieme i l = List.nth l (i-1)

    let first l = List.hd l

    let rec prefix = function
      |	[] -> []
      |	[e] -> []
      |	e::es -> e::(prefix es)

    let safe_tl = function
      |	[] -> []
      |	_::xs -> xs

    let last l = List.nth l ((List.length l)-1) 

    let extremity l =
      let rec extremity_rec (first,body) = function
	| [ ]   -> (first, body, [ ])
	| [e]   -> (first, body, [e])
	| e::es -> extremity_rec (first, e::body) es
      in 
	let (first,body,last) = 
	  if l = [] 
	  then ([],[],[])
	  else extremity_rec ([List.hd l], []) (List.tl l) 
      in 
	(first, List.rev body, last)


    let prolongate l e = l@[e]

    let rec remove_last = function
      |	[]  -> []
      |	[e] -> []
      |	e::es -> e::(remove_last es)

    let rassoc e l = List.assoc e (List.map (fun (x,y) -> (y,x)) l)

    let split_on_with equal elt l = 
      let rec split_on_with_rec (previous,next) = match next with
      | []    -> (previous,next)
      |	x::xs -> 
	      if equal x elt then (previous,xs) 
	      else split_on_with_rec (previous@[x],xs)
      in split_on_with_rec ([],l)

    let split_on elt l = split_on_with (=) elt l

    (* NOTE: it loses the requested element
       split_on 0 [] ;;        = ([], [])
       split_on 0 [1;2;3] ;;   = ([1; 2; 3], [])
       split_on 1 [1;2;3] ;;   = ([], [2; 3])
       split_on 2 [1;2;3] ;;   = ([1],[3])
       split_on 3 [1;2;3] ;;   = ([1; 2], [])
     *)

    let split_after_with equal elt l =
      let rec split_after_with_rec (previous,next) = match next with
      | []    -> (previous,next)
      |	x::xs -> 
	      if equal x elt then (previous@[x],xs) 
	      else split_after_with_rec (previous@[x],xs)
      in split_after_with_rec ([],l)

    let split_after elt l = split_after_with (=) elt l

    (* NOTE: does not lose the requested element
       split_after 0 [] ;;        = ([], [])
       split_after 0 [1;2;3] ;;   = ([1;2;3], [])
       split_after 1 [1;2;3] ;;   = ([1], [2;3])
       split_after 2 [1;2;3] ;;   = ([1;2],[3])
       split_after 3 [1;2;3] ;;   = ([1;2;3], [])
     *)


    let split_at n l =
      let rec split_at_REC n (first,tail) = 
	if (n=0) 
	then (first,tail)
	else 
	  match tail with
	  | []    -> ([],l)
	  | x::xs -> split_at_REC (n-1) (first@[x],xs)
      in if (n<=0) then ([],l) else split_at_REC n ([],l)


    let decomp_along l sizeS = 
      let rec decomp_alongR acc l = function 
	| []    -> acc @ [l]
	| n::ns -> 
		if l=[] then acc
		else
		  let (n_first, tail) = split_at n l
		  in decomp_alongR (acc @ [n_first]) tail ns
      in
	decomp_alongR [] l sizeS


    let decomp_in_pieces l n = (* euclide *)
      let lg = List.length l
      in let size = lg/n
      in let remain = lg mod n
      in decomp_along l (iterate n (fun l -> size::l) [remain])


    let rec separate_by sep = function
      |	[] -> []
      |	e::es -> e::sep::(separate_by sep es)

    let surround_by s = function
      | [] -> []
      | es -> s::(es@[s])

    let sliced_by symb l = 
      surround_by symb
	(separate_by symb l)

    let rec is_prefix_of l1 l2 = match l1,l2 with
      |	[ ], _  -> true
      |	 _ ,[ ] -> false
      |	x::xs, y::ys -> if x=y then is_prefix_of xs ys else false

    (* TEST
     * prefix_of [1;2;3] [1;2;3;4;5] ;; = true
     * prefix_of [2;3] [1;2;3;4;5]   ;; = false
     * prefix_of [1;2;3] [1;2;4;5]   ;; = false
     * prefix_of [1;2;3] [1;2]       ;; = false
     * prefix_of [] [1;2]            ;; = true
     *)

    let rec zip lx ly =
      match (lx,ly) with
      | ([],_) 
      | (_,[]) -> []
      | (x::xs,y::ys)   -> (x,y)::(zip xs ys)
		
    let zip_status lx ly =
      let rec zipR lxy = function
	| []   , []   -> true, List.rev lxy
	| []   , _ 
	| _    , []   -> false,[]
	| x::xs,y::ys -> zipR ((x,y)::lxy) (xs,ys)
      in zipR [] (lx,ly)

    let zipWith op l1 l2 =
      List.map 
	(fun (x,y) -> op x y)  
	(zip l1 l2)
	
	
    let unzip lxy =
      List.fold_right
	(fun (x,y) (xs_acc,ys_acc) -> (x::xs_acc,y::ys_acc))
	lxy
	([],[])
	
    (* EXAMPLES
       let l = zip [1;3;5;7] [2;4;6;8] ;; =  [(1, 2); (3, 4); (5, 6); (7, 8)]
       unzip l ;;                         = ([1;3;5;7],[2;4;6;8])

       let l = zip [[1];[3];[5]] [[2];[4];[6]] ;; = [([1], [2]); ([3], [4]); ([5], [6])]
       unzip l ;; =  ([[1]; [3]; [5]], [[2]; [4]; [6]])
     *)	

(*
    let rec verbose_zip lx ly = 
      let (+) c = function 
	| Succ_Fail.Succ(xs)  -> Succ_Fail.Succ(c::xs)
	| Succ_Fail.Fail(err) -> Succ_Fail.Fail(err)
      in
      match (lx,ly) with   
      | ([],[])       -> Succ_Fail.Succ([])
      | ([],_)        -> Succ_Fail.Fail((lx,ly))
      | (_,[])        -> Succ_Fail.Fail((lx,ly))
      | (x::xs,y::ys) -> (x,y) + (verbose_zip xs ys)
*)	    
	   
    let gather  (split_wrt: 'a list -> 'a -> 'a list *'a list) l = 
      let rec gatherH acc l =
	match l with
	| []   -> acc
	| x::_ -> 
	    let (x_like, others) = split_wrt l x 
	    in gatherH (x_like::acc) others
      in
      gatherH [] l
	
	
    let gather_equivalent equiv l = 
      let split_wrt l x = List.partition (fun y -> equiv x y) l
      in
	gather split_wrt l


    let factorize_wrt split_key_data  elts = 
      let key  e = fst (split_key_data e)
      and data e = snd (split_key_data e) 
      in
	match elts with
	| []    -> []
	| e::es -> [ (key e, List.map data elts) ]
	

    let filter_with l p = List.filter p l 


    let sum_from_with zero f = function
      |	[]    -> zero
      |	x::xs -> List.fold_left f x xs	

    let sum_with f l = List.fold_left f (List.hd l) (List.tl l)
     
    let concat_with plus zero = 
      List.fold_left (fun acc e -> plus e acc) zero
     (* FIXME: the order is not preserved ! *)
     (* plus should operate on arguments of the same type *)

    let forEachIn l f initacc = List.fold_left (fun acc x -> f x acc) initacc l

    (* Three functions that do: List.concat (List.map f l) *)

    let forEach l f = 
      forEachIn l (fun e acc -> (f e)@acc) []

    (* EXAMPLE
     * List.map (fun n -> [n]) [1;2;3;4] ;; = [[1]; [2]; [3]; [4]]
     * List.concat (List.map (fun n -> [n]) [1;2;3;4]) ;; = [1;2;3;4]
     *
     * forEach [1;2;3;4] (fun n -> [n]) ;; = [4;3;2;1]
     *)

    let forEachDo f l = 
      forEachIn l (fun e acc -> (f e)@acc) []

    let rec mapAndConcat f = function 
      |	[] -> []
      |	x::xs -> (f x) @ (mapAndConcat f xs)


    let product_with f l1 l2 = forEach l1 (fun e1 -> List.map (fun e2 -> (f e1 e2)) l2)

    let product l1 l2 = forEach l1 (fun e1 -> List.map (fun e2 -> (e1,e2)) l2)


    let select_one_elt l = 
      let rec f acc took = function
	| []        -> acc
	| e::others -> f ((e,took@others)::acc)  (e::took)  others
      in
	f [] [] l


    let combination_of_size_without_repetition n elts = (* FIXME: PERMUTATION ? STRANGE ? OR UNNECESSARILY COMPLEX ? *)
      let rec f  n  comb_acc  unused_elts =
	if (n=0) || (unused_elts=[])
	then [comb_acc]
	else forEach (select_one_elt unused_elts) 
	      (fun (e,others) -> f (n-1) (e::comb_acc) others)
      in f n [] elts


    let rec distrib_on e = function 
      | []    -> []
      | x::xs -> (e::x)::(distrib_on e xs)
    
 
    let distrib elts ll = 
      List.flatten (List.map (fun e -> distrib_on e ll) elts)


    let combination_of n elts = iterate n (distrib elts) [[]] 


    let subsets elts =
      List.fold_left (fun sets e -> sets@(distrib_on e sets)) [[]] elts


    let subsets_of_size n l =
      let rec spread_out n elts l =
	if (n=0) 
	then [l]
	else match elts with
	| []    -> []
	| e::es -> (spread_out (n-1) es (e::l)) @ (spread_out n es l)
      in
	spread_out n l []


    let rec shareout l ll =
      match (l,ll) with
      | (_,[]) -> List.map (fun x -> [x]) l
      | ([],_) -> ll 
      | (x::xs,l::ls) -> (x::l)::(shareout xs ls)


    (* apply: (f:'env ->'a -> 'env * 'b) -> 'env * 'a list -> 'env * 'b list  *)

    let apply f (env,la) = 
      let (env',lb) = 
	List.fold_left  
	  (fun (env,lb) a -> let (env',b) = f env a in (env', b::lb)) 
	  (env,[])  
	  la
      in (env',List.rev lb)


    let map_right f l = (* its evaluation order is the reversed of those of map ; starts by the end *)
      l |> List.rev |> (List.map f) |> List.rev
		
    let map_on l f = List.map f l
	    
    let distribute_on e xs = List.map (fun x -> (e,x)) xs

    let distribute_fst l = mapAndConcat (fun (e,xs) -> distribute_on e xs) l
			

    let replace_by_in oldV newV l = (* NAIVE VERSION *)
	let rec f beg mid tail old = 
	  match (old, tail) with
	    [],[] -> beg @ newV
	  |
	    _ ,[] -> beg @ mid
			     
	  |
	    [],_    -> f (beg @ newV)           []        tail  oldV
	  | 
	    o::os, x::xs -> 
		  let mid = mid@[x]
		  in let h = List.hd mid
		  in let t = List.tl mid
		  in
		    if o=x 
		    then f beg        mid   xs        os
		    else f (beg@[h])  []    (t @ xs)  oldV
	in
	  f [] [] l oldV


    let mapfold2 default m f l = 
      match l with 
      | []    -> default
      | x::xs -> List.fold_left (fun x y -> f x (m y)) (m x) xs
	    
	  
    let make n elt = iterate n (fun acc -> elt::acc) []
	
	
    let rec fold_until stop_when f acc = function
      | []    -> acc
      | x::xs -> 
	  if (stop_when x acc)  
	  then (f x acc)  
	  else (fold_until stop_when f (f x acc) xs)

(*			
    let rec try_until_success f = function 
      | []    -> SUCCESS_FAILURE.FAILURE
      | x::xs -> 
	      (match f x with 
	      | SUCCESS_FAILURE.FAILURE -> try_until_success f xs
	      | success        -> success
	      )
	
    let compare_with  compare_elt lx ly =
      match  verbose_zip lx ly with
      | Succ_Fail.Fail([],_) -> -1
      | Succ_Fail.Fail(_,[]) ->  1
      | Succ_Fail.Succ(lxy)  -> 
	  fold_until (fun c acc -> (c!=0))
	    (fun c acc -> c)
	    0
	    (List.map (function (x,y) -> compare_elt x y) lxy)
	    
      |  _  -> _INCOMPARABLE (*  for completness of the matching ; never happens. That constant is defined in common.ml *)
	    
 *)
	      
    let extremum inf l = 
      List.fold_left 
	(function min -> function elt -> if (inf min elt) then min else elt)
	(List.hd l)
	(List.tl l)
  
    let extremum_of inf l =
      List.fold_left 
	(function (x,l) -> function y -> if (inf x y) then (x,y::l) else (y,x::l))
	(List.hd l,[]) 
	(List.tl l)  
	
    let max_of f l = extremum (<) (List.map f l)

    let rec take_sublist emin emax l =
      if (emin>1)
      then 
	match l with 
	| []    -> []
	| x::xs -> take_sublist (emin-1) (emax-1) xs
      else (* emin=1 *)
	if (emax>=1)
	then
	  match l with
	  | []    -> []
	  | x::xs -> x::(take_sublist emin (emax-1) xs)
	else (* emin=1, emax=0 *)
	  []
	    
	    
    let transpose ll = 
      List.fold_right (fun l llacc -> distrib l llacc) ll [] 
	
		
    let pretty_generic  begSymb  separator  endSymb  pretty_elt l = 
      begSymb ^ (String.concat separator (List.map pretty_elt l)) ^ endSymb

    let separate_with begSymb separator endSymb pretty_elt l = 
      let str = pretty_generic "" separator "" pretty_elt l in
	if str = "" 
	then ""
	else begSymb ^ str ^ endSymb 

			  
    let pretty_compact pretty_elt = pretty_generic "[" "; " "]" pretty_elt 

    let pretty       pretty_elt = pretty_generic "\n[ " "\n; " "\n]\n" pretty_elt 

    let pretty_parameters p l = pretty_generic "(" "," ")" p l


(* ALL THE FOLLOWING FUNCTION CORRESPONDS TO SETs operations !!! *)

    let rec member_with equal e = function
      |	[]                     -> false
      |	x::xs when (equal e x) -> true
      |	x::xs                  -> member_with equal e xs

    let rec add_merge_with merge when_equal e' = function 
      |	[] -> [e']
      |	e::es ->     
	      if (when_equal e' e) 
	      then (merge e' e)::es
	      else e::(add_merge_with merge when_equal e' es)


    let add_with equal e l = if member_with equal e l then l else e::l 
    let add e l = add_with (=) e l 

    let union_merge_with merge when_equal l1 = 
      function l2 -> forEachIn l1 (fun e1 -> add_merge_with merge when_equal e1) l2 

    let union_with equal l1 = function l2 -> forEachIn l1 (fun e1 -> add_with equal e1) l2 
    let union l1 l2 = union_with (=) l1 l2 

    let remove_duplicates_with equal l = union_with equal l []
    let remove_duplicates l = remove_duplicates_with (=) l

    let inter_with equal l1 = function l2 -> List.filter (fun e2 -> member_with equal e2 l1) l2
    let inter l1 l2 = inter_with (=) l1 l2

    let minus_with equal l1 = function l2 -> forEach l1 (fun e1 -> if member_with equal e1 l2 then [] else [e1]) ;;
    let minus l1 l2 = minus_with (=) l1 l2 ;;

    let subseteq_with equal l1 = function l2 -> (minus_with equal l1 l2)=[] ;;
    let subseteq l1 l2 = subseteq_with (=) l1 l2 ;;

    let equalset_with equal l1 = function l2 -> (subseteq_with equal l1 l2) && (subseteq_with equal l2 l1) ;;
    let equalset l1 l2 = equalset_with (=) l1 l2

    let intersect_with equal l1 = function l2 -> exists_in l1 (fun e1 -> member_with equal e1 l2) 
    let intersect l1 l2 = intersect_with (=) l1 l2 

    let bigUnion l = concat_with union [] l ;;

(*	
    let star_fixpoint  (op_elt:'t -> 't list) (initial_elts:'t list) : 't list = 
      Hof.fixpoint subseteq union minus op_elt initial_elts
*)

(* USEFUL FUNCTIONS ON LIST *)

    let minmax error_value opMinMax = function 
      |	[]        -> error_value
      |	first::xs -> List.fold_left (fun acc e -> opMinMax acc e) first xs


    let mergeWith p plus zero l =
      let (p_like,others) = List.partition (fun e -> p e) l
      in
      (concat_with plus zero p_like) :: others
	

    let get_one pred lref =
      let cons x (lp,l) = (lp,x::l)
      in
	let rec get_one_rec = function
	  | []    -> ([],[])
	  | x::xs -> 
		  if (pred x) 
		  then ([x],xs)
                  else (cons x (get_one_rec xs))
	in 
	  let 
	    (lp,l)= get_one_rec !lref
	  in
	    begin
	      lref:=l; 
	      lp 
	    end

    (* EXAMPLES
     * let lr = ref [1;2;3;4;5];;
     * get_one (fun x -> x<=2) lr ;; = [1]  !lr = [2;3;4;5]
     * get_one (fun x -> x<=2) lr ;; = [2]  !lr = [3;4;5]
     *)

    let filter_elim pred lref =
      let (lpred,others)= List.partition pred !lref
      in 
	begin
	  lref:=others; 
	  lpred 
	end

    (* EXAMPLES
     * let lr = ref [1;2;3;4;5] ;;
     * filter_elim ((<=) 2) lr ;; = [1;2] !lr = [3;4;5] ;;
     * filter_elim ((<=) 2) lr ;; = [ ]   !lr = [3;4;5] ;;
     *)

    let member_with_elim equal e lref =
      let cons x (b,l) = (b,x::l)
      in
	let rec member_with_elim_rec = function
	  | []    -> (false,[])
	  | x::xs -> 
		  if (equal e x) 
		  then (true,xs)
                  else (cons x (member_with_elim_rec xs))
	in 
	  let 
	    (b,l)= member_with_elim_rec !lref
	  in
	    begin
	      lref:=l; 
	      b 
	    end

    (* EXAMPLES
     *
     * member_with_elim (=) 2 [1;2;3;4;5] ;; = (true ,[1;  3;4;5])
     * member_with_elim (=) 6 [1;2;3;4;5] ;; = (false,[1;2;3;4;5])
     *)

    let remove_when p l = List.filter (fun e -> not(p e)) l 

    let removed_by p l = List.partition (fun e -> not(p e)) l 

    let remove_equals_with eq e l = remove_when (fun e' -> eq e' e) l 

    let rec number_with annotate_with step n = function
      | []    -> []
      | x::xs -> (annotate_with n x)::(number_with annotate_with step (step n x) xs)
					
    let number_from n l = number_with (fun n x -> (n,x)) (fun n x -> (n+1)) n l

    let rang_with_update equal e lRef =
      let rec rangRec i e = function
	| x::xs -> if equal x e then i else rangRec (i+1) e xs
	| []    -> begin lRef:=!lRef@[e] ; i end
      in rangRec 0 e !lRef

    let rang e l = 
      let rec rangRec i = function
	| []    -> -1
	| x::xs -> if x=e then i else rangRec (i+1) xs
      in rangRec 0 l


    let assoc_update_with f e lRef =
      try
	List.assoc e (!lRef) 
      with
	Not_found ->  
	      let a = f e (!lRef) in
		begin
		  lRef:= (e,a)::(!lRef) ; 
		  a 
		end

    let collect f l = remove_duplicates (List.map f l)

    (* compact [a;b;b;a;c] = [ (2,a) ; (2,b) ; (1,c) ] *)	
    let rec compact = function
      |	[] -> []
      |	x::l -> 
	      let (xs,os) = List.partition ((=) x) l
	      in let nbx = 1 + List.length xs
	      in (nbx,x)::(compact os)
      
(* 
   intersection and minus are set opertions,
   use common and diff for lists if the number of occurences matters !! 
*)

    let rec remove_one e = function
      |	[] -> []
      |	x::xs when e=x -> xs
      |	x::xs -> x::(remove_one e xs) 

(*		      
    let get e l =
      let rec getR acc = function
	| [] -> Succ_Fail.Fail(e)
	| x::xs -> if (e=x) then Succ_Fail.Succ (e, (List.rev acc)@xs) else getR (x::acc) xs
      in
	getR [] l 

    let rec common lx ly = 
      match lx with
      |	[] -> []
      |	x::xs -> 
	      match get x ly with 
	      | Succ_Fail.Fail(x) -> common xs ly
	      |	Succ_Fail.Succ(x,ys) -> x::(common xs ys)

let rec diff lx ly =
  match lx with
  | [] -> []
  | x::xs -> match get x ly with 
    | Succ_Fail.Fail(_) -> x::(diff xs ly)
    | Succ_Fail.Succ(x,ys) -> (diff xs ys)


let rec is_sublist_of lx ly = 
  match lx with
  | [] -> true
  | x::xs -> match get x ly with
    | Succ_Fail.Fail(_) -> false
    | Succ_Fail.Succ(x,ys) -> is_sublist_of xs ys 
    

*)

  

(* TEST: 

   Ext.LIST.make 5 "a" ;;

   *** ZIP/UNZIP ***
	
   Ext.LIST.verbose_zip [1;2;3;4;5] ["a1";"a2";"a3";"a4";"a5";"a6";"a7"] ;;
   Ext.LIST.verbose_zip [1;2;3;4;5] ["a1";"a2";"a3";"a4";"a5"] ;;
   Ext.LIST.verbose_zip [1;2;3;4;5] ["a1";"a2";"a3"] ;;

   Ext.LIST.zip [1;2;3;4;5] ["a1";"a2";"a3";"a4";"a5";"a6";"a7"] ;;
   Ext.LIST.unzip (Ext.LIST.zip [1;2;3;4;5] ["a1";"a2";"a3";"a4";"a5";"a6";"a7"]) ;;

   Ext.LIST.zip [1;2;3] [[10;11];[20;22];[30;33]] ;;
   Ext.LIST.zipWith (fun x xs -> x::xs) [1;2;3] [[10;11];[20;22];[30;33]] ;;


   *** COMBINATOR ***

   Ext.LIST.combination_of_size 2 [1;2;3;4] ;;
 
   *** COMPARE ***

   Ext.LIST.compare compare [] [] ;;
   Ext.LIST.compare compare [1;2;3;4;5] [] ;;
   Ext.LIST.compare compare [] [1;2;3;4;5] ;;
   Ext.LIST.compare compare [0;0;0;0;0;0;0;0] [1;2;3;4;5] ;; 
   Ext.LIST.compare compare [1;2;3;4;5] [0;0;0;0;0;0;0;0] ;;
   Ext.LIST.compare compare [1;2;3;4;5] [1;2;3;4;0] ;; 
   Ext.LIST.compare compare [1;2;3;4;5] [1;2;3;4;6] ;; 
   Ext.LIST.compare compare [1;2;3;4;5] [1;2;3;4;5] ;;

   *** DUPLICATES ***

   Ext.LIST.gather_equivalent (=) [0;1;2;1;3;2;4;3;5;4;6;5;6] ;;

   List.filter (fun x -> x=1) [0;1;2;1;3;2;4;3;5;4;6;5;6] ;;
   List.partition (fun x -> x=1) [0;1;2;1;3;2;4;3;5;4;6;5;6] ;;

   *** MIN/MAX ***

   Ext.LIST.extremum (<) [1;2;3;4;5;0;6;7;8;9] ;;
   Ext.LIST.extremum (>) [1;2;3;4;5;0;6;7;8;9] ;;

   *** SUBLIST ***

   Ext.LIST.take_sublist ~-1 5 [] ;;
   Ext.LIST.take_sublist 1 5 [1;2;3;4;5] ;;
   Ext.LIST.take_sublist 3 4 [1;2;3;4;5] ;;

   *** DISTRIB ***

   Ext.LIST.distrib [1;3;5] [] ;;
   Ext.LIST.distrib [2;4;6] (Ext.LIST.distrib [1;3;5] []) ;;

   *** SHAREOUT ***

   Ext.LIST.shareout [1;3;5] [] ;;
   Ext.LIST.shareout [2;4;6] (Ext.LIST.shareout [1;3;5] []) ;;

   *** TRANSPOSE ***

   let ll = Ext.LIST.distrib [2;4;6] (Ext.LIST.distrib [1;3;5] []) ;;
   Ext.LIST.transpose ll ;;
   Ext.LIST.transpose (Ext.LIST.transpose ll) = ll ;;


   *** CONCAT WITH ***

   Ext.LIST.concat_with (@) [] [[1;2];[3;4];[5]] ;;
   Ext.LIST.concat_with (fun x l -> x::l) [] [[1;2];[3;4];[5]] ;;
   Ext.LIST.concat_with (fun x l -> x::l) [] [1;2;3;4;5] ;;

   *** MERGE WITH ***

   Ext.LIST.mergeWith (fun l -> List.length l =2) (@) [] [[1;2];[3;4];[5]] ;;

   *** MIC LIST FUNCTOR *** 

   #load "common.cmo" ;;
   open Common ;;

   module IntList = Ext.LIST(Int) ;;
   module IntList = Ext.LIST(Common.Int) ;;

   let module I = Ext.LIST(Common.Int) in
   let module S = Ext.LIST(Common.String)  in (S.pretty (S.make 5 "a")) ^ (I.pretty [0;1;2;1;3;2;4;3;5;4;6;5;6]) ;;

   let module I = Ext.LIST(Common.Int) in
   let module S = Ext.LIST(Common.String)  in  S.zip (S.make 5 Common.String.default) (I.make 6 Common.Int.default) ;;
*)



 

