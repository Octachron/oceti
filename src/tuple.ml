
(* 'a t is internaly an Obj.t array, but this representation is
unsafe and hidden away *)
type 'a t

module Unsafe = struct
  let untype: 'a t -> Obj.t array = Obj.magic
  let transmute: Obj.t array -> 'a t = Obj.magic
end

module Phantom_index : sig
  type +'a t
      
  val to_int: 'a t -> int
  val z : <list:'a *'b; selected: 'a> t
  val s: < list:'t; selected:'s > t ->
    < list: 'a * 't; selected: 's > t

end = struct
  type 'a t = int
  let to_int x = x
  let z = 0
  let s = succ
end


module Phantom_defs = struct
  open Phantom_index
  let _0 = z
  let _1 = s _0
  let _2 = s _1
  let _3 = s _2
  let _4 = s _3
end

type void = Hlist.void
module Index = struct
  type _ t =
    | Z : < tuple:
              <list:'a *'b; left: void; selected: 'a; right: 'b>;
            create:
              <f: 'typ -> 'res_c; typ: 'typ; res: 'res_c >;
          > t
    | S: < tuple:
             < list:'t; left: 'left;  selected:'s; right: 'r  >;
           create:
             <f:'f; typ:'ty; res:'res >;
           > t ->
      < tuple:
          < list: 'a * 't; left:'a * 'left; selected: 's; right:'r >;
        create:
          < f:'a_c -> 'f; typ: 'a_c * 'ty; res:'res >;
        > t

  let to_int index =
    let rec to_int: type a. int -> a t -> int  = 
    fun n -> function
    | Z -> n
    | S i -> to_int (n+1) i in
    to_int 0 index

  module Defs = struct 
    let _0 = Z
    let _1 = S _0
    let _2 = S _1
    let _3 = S _2
    let _4 = S _3
  end
end


let n0 k =  k ()
let s n k x = n (fun v -> k (x,v))
let p sel = sel (fun x -> x)

let n0 k seed =  k seed
let s n k x = n (fun v -> k (x,v))
let p sel = sel (fun x -> x)
let id x = x

(* < l : a_0 * ..* a_n; apply: a_0 -> ... -> a_n-> 'res;   *)
let z = n0
let one = s n0

let one_bis = Index.(S Z)
let t = (s @@ s n0) id 1 "2" (+) 

module Try1 = struct
type _ ind =
  | Z : < typ:'typ; f:'typ -> 'res; res:'res; ztyp:'typ > ind
  | S : < typ:'typ; f:'f; res:'res; ztyp:'ztyp> ind -> <typ:'a * 'typ; f:'a -> 'f; res:'res; ztyp:'ztyp> ind 

let rec to_int: type a. a ind -> int = function Z -> 0 | S k -> 1 + to_int k


let rec reify_gen: type ztyp l res f.  <typ:l; res:res; f:f; ztyp:ztyp > ind -> ( l -> res ) -> f  = function
  | Z -> fun kont tuple -> kont tuple
  | S k ->
    let k = reify_gen k in
    fun kont x -> k ( fun acc -> kont (x,acc) )

let reify k= reify_gen k id

let t = reify (S Z) 2 3.

end

(*              < focus:<list:'a *'b; left: void; selected: 'a; right: 'b>; f:<typ:'res; res:'res> >; *)
module Try2 = struct
  type _ ind =
    (*        < focus: <list:'a *'b; left: void; selected: 'a; right: 'b>; f:<typ:'res; res:'res> >; *)
  | Z :  < typ:'typ; ztyp:'typ; f:'res; res:'res > ind
  | S : < typ:'typ; f:'f; res:'res; ztyp:'ztyp> ind -> <typ:'a * 'typ; f:'a -> 'f; res:'res; ztyp:'ztyp> ind 

let z = Z

let rec to_int: type a. a ind -> int = function Z -> 0 | S k -> 1 + to_int k


let rec reify: type l res f.  <typ:l; res:res; f:f; ztyp:Hlist.void > ind -> ( l Hlist.t -> res ) -> f  = function
  | Z -> fun kont -> kont Hlist.Nil
  | S k ->
    let k = reify k in
    fun kont x -> k ( fun acc -> kont @@ Hlist.Cons(x,acc) )

let t = reify (S (S Z)) id 2 3.

end


(*
type _ ind =
  | Z : < typ:'typ; f:'typ t -> 'res; res:'res; ztyp:'typ * 'void > ind
  | S : < typ:'typ; f:'f; res:'res; ztyp:'ztyp> ind -> <typ:'a * 'typ; f:'a -> 'f; res:'res; ztyp:'ztyp> ind 

let rec to_int: type a. a ind -> int = function Z -> 0 | S k -> 1 + to_int k

[%%indexop
let get: 'list t -> <typ:'list; ztyp:'r; .. > ind -> 'r =
  fun arr index ->
    let arr  = Unsafe.untype arr in
    Obj.magic arr.(to_int index)
let set: 'list t -> <typ:'list; ztyp:'r; ..> ind -> 'r -> unit =
  fun arr index value ->
    let arr = Unsafe.untype arr in
    arr.(to_int index) <- Obj.repr value
]

(*
let cut_right: < tuple:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l s -> 'll s =
  fun _n s -> Obj.magic s
*)
    
let rec rewrite: type l res le f.  <typ:l; res:res; f:f; ztyp:le > ind -> ( l t -> res ) -> f  = function
  | Z -> fun kont tuple -> kont tuple
  | S k ->
    let kr = rewrite k in
    fun kont x -> kr ( fun acc -> acc.{k} <- x; kont acc )
 
(*
(*
let rec rewrite: type l res f.  <typ:l; res:res; f:f; ztyp:l t > ind -> ( l t -> res ) -> f  = function
  | Z -> fun kont tuple -> kont tuple
  | S k ->
    let kr = reify k in
    fun kont x -> kr ( fun acc -> (Unsafe.untype acc).(to_int k ) <- Obj.repr x; kont acc )
*)

(*
let zip nt =
  let array = Array.make (Index.to_int nt) @@ Obj.repr 0 in
  let rec zip: type res f. < create:<typ:'l; f:f; res:res >; .. > Index.t -> ( 'l t -> res) -> f = fun n kont -> 
    match n with
    | Index.Z -> fun array -> kont array
    | Index.S k -> fun x -> (zip k) ( fun array -> array.(Index.to_int n) <- Obj.repr x; kont array  )
  in
  zip nt (fun x -> x)
*)


(* Hlist.t are well typed and can be easily constructed using list 
   syntax (from ppx_listlike). Therefore, there are good inializers
   for generic tuple *)
let from_list (l:'a Hlist.t) : 'a t =
  let open Hlist in
  let n = Hlist.length l in 
  let array = Array.make n @@ Obj.repr 0 in
  let rec write: type a. int -> a t -> unit = fun k ->
    function%with_ll
    | [] -> ()
    | a::q -> array.(k) <- Obj.repr a; write (k+1) q in
  let () =
    write 0 l in
  Unsafe.transmute array





let arr =
  from_list Hlist.([%ll 1; 2; 4; (fun x ->  x lsl 4); (+) ])

open Index.Defs

let n = arr.{_0}
let hi = arr.{_1}
let some = arr.{_2}
let k = arr.{_3} n             
let n' =
  arr.{_0} <- 4;
  arr.{_0}



module Slice = struct
type 'a s
type raw = {offset :int; array: Obj.t array }

let untype: 'a s -> raw = Obj.magic
let transmute: raw -> 'a s = Obj.magic                                        

let full: 'a t -> 'a s = fun a -> transmute { offset=0; array=Unsafe.untype a }

let cut_left: < tuple:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l s -> ('a*'r) s =
  fun n s -> let s = untype s in transmute { s with offset= Index.to_int n + s.offset }

let cut_right: < tuple:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l s -> 'll s =
  fun _n s -> Obj.magic s

let cut l r s =
  s |> cut_right r |> cut_left l

let make k a =
  cut_left k @@ full a

[%%indexop
let get: 'list s -> < tuple: <list:'list; selected:'r; .. > ; .. > Index.t -> 'r =
  fun slice index ->
    let raw  = untype slice in
    Obj.magic raw.array.( raw.offset + Index.to_int index)
let set: 'list s -> < tuple:<list:'list; selected:'r; ..>;..> Index.t -> 'r -> unit =
  fun slice index value ->
    let raw = untype slice in
    raw.array.(Index.to_int index) <- Obj.repr value
]

end

let s = Slice.make _1 arr
(*
let rec rewrite: type a l f. (a*l) Slice.s -> < create:<typ:a*l; f:a -> f; res:unit> ; .. > Index.t -> a -> f = fun slice n a ->
  let open Slice in
  match n with
  | Index.S Index.Z -> slice.{_0} <- a
  | Index.S (Index.S k) -> slice.{_0} <- a; rewrite (cut_left _1 slice) (Index.S k)
*)
(*
let zip (nt: 'nt Index) =
  let n = Index.to_int nt in
  let array : 'nt t = Obj.magic @@ Array.make n @@ Obj.repr 0 in
  let rec interm k v= array.{k} <- v; interm (Index.S k) in() 
  *)


(*
let zip (n:< create:<f:'f; typ:'ty; res:'ty t>; .. > Index.t) : 'f =
  let array: 'ty t  = Obj.magic @@ Array.make (Index.to_int n) @@ Obj.repr 0 in
  let rec zip: type f any.
    < create:<f: f; typ: any; res: any t>; ..  > Index.t -> f  =
    fun n ->
    let open Index in
    match n with
    | Z ->  array 
    | S k -> fun a ->  array.{k} <- a; zip k in
  zip
*)

let id x = x

let f tuple =
  tuple.{_4} tuple.{_2} @@ tuple.{_0} + tuple.{_1}

let m = f arr

*)
*)
