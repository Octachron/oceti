
type z = Zero
type 'a succ=Succ

(** Type level representation of int 
	Church encoding : natural number are represented as translation :
		zero is represented as 'a -> 'a 
		if n is represented as 'a -> 'b, succ n is represented as 'a -> 'b succ
*)
type (_) t = 
  | Z  : ('a -> 'a) t 
  | S  : ('a -> 'b) t -> ('a -> 'b succ) t  
(** Conversion to int *)
let rec to_int: type nat. nat t -> int = 
  fun nat -> match nat with
    | Z   -> 0
    | S n -> 1 + to_int n


type tautology = unit
  
let assert_eq: type a b. (a -> b) t -> (a -> b) t -> tautology = 
fun _nat _nat' -> ()

(** Arithmetic operations. Beware of complex behavior in presence of monomorphic type  *)
let rec plus : type inf mid sup.
  (mid -> sup) t -> (inf -> mid) t -> (inf -> sup) t =
  fun nat1 nat2 -> match nat1 with
    | Z -> nat2
    | S (nat1) -> S( plus nat1 nat2) 
                    
let rec minus: type inf mid sup .
  (inf -> sup) t -> (mid -> sup) t -> (inf -> mid) t= 
  fun p n -> match p, n with
    | p, Z -> p 
    | S p, S n -> minus p n
    (* impossible branch *)
    | Z , S _ -> assert false


external tm: ('a succ -> 'b succ ) t -> ('a -> 'b ) t = "%identity"

(** Iteration and associated types *)
type ('sup,'a) funR= { f: 'inf . ('inf -> 'sup) t -> 'a  }
type ('sup1,'sup2,'a) funR2 = {f2 : 'inf1 'inf2. ('inf1 -> 'sup1) t -> ('inf2 -> 'sup2) t -> 'a  }

let iter_natl ( n:('inf -> 'sup) t) (r : ('sup,unit) funR)  = 
  let rec iter_natl : type inf mid. (inf -> mid) t -> (mid ->'sup) t -> ('sup, unit) funR -> unit =   
    fun counter n r -> match counter with
      | Z -> r.f n
      | S c -> r.f n; iter_natl c (tm (S n)) r
  in 
  iter_natl n Z r

(** Does not work *) 
(*
let iter_range : type inf mid. (inf,mid) t -> (mid,'sup) t -> ('sup,unit) funR -> unit = 
fun start ende r ->
	let diff = minus ende start in
	let r = { f  =  fun nat -> r.f (plus nat start) } in
	iter_natl diff r 
*)


(** Without Obj.magic *)
let iter_nat2 nat1 nat2 r  = 
  iter_natl nat1 { f = fun nat1 -> iter_natl nat2 (r.f nat1) }


(** Tests *)
(*
let rec p21 : type mid sup inf. (mid -> sup succ succ) t -> (z -> mid succ) t -> (z -> sup succ) t =
fun nat1 nat2 -> match nat1, nat2 with
| S(S Z), S nat2 -> S nat2 
| S(S (S nat1)), S nat2  ->  S ( p21 (S (S nat1) ) (S nat2)  )
|
*)

let assert_eq: type inf sup q1 q2. (inf->sup) t -> (inf->sup) t -> (inf->sup) t  = fun n1 n2 -> n1
