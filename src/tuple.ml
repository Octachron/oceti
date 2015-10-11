
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

module Index = struct
  type _ t =
    | Z : < tuple:
              <list:'a *'b; selected: 'a>;
            apply:
              <list: 'a ->'b; result: 'b >
          > t
    | S: < tuple:
             < list:'t; selected:'s >;
           apply:
             < list:'f; result:'r >
         > t ->
      < tuple:
          < list: 'a * 't; selected: 's>;
        apply:
          <list: 'a -> 'f; result:'r >
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

(* 'a t is internaly an Obj.t array, but this representation is
unsafe and hidden away *)
type 'a t

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
  Obj.magic array

[%%indexop
let get: 'list t -> < tuple: <list:'list; selected:'r > ; .. > Index.t -> 'r =
  fun arr index ->
    let arr : Obj.t array = Obj.magic arr in
    Obj.magic arr.(Index.to_int index)
let set: 'list t -> < tuple:<list:'list; selected:'r>;..> Index.t -> 'r -> unit =
  fun arr index value ->
    let arr : Obj.t array = Obj.magic arr in
    arr.(Index.to_int index) <- Obj.repr value
]

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

(*
let zip (n:< tuple:<list:'l;..>; apply:<list:'f;result:'l t> > Index.t) : 'f =
  let array: 'l t  = Obj.magic @@ Array.make (Index.to_int n) @@ Obj.repr 0 in
  let rec zip: type l f a.
    < tuple:<list:a * l;..>; apply:<list:a -> f;result:(a * l) t> > Index.t -> a -> f  =
    fun n a ->
    let open Index in
    match n with
    | S Z -> array.{Z} <- a;  array 
    | S k -> array.{k} <- a; zip k in
  zip 0
*)

let f tuple =
  tuple.{_4} tuple.{_2} @@ tuple.{_0} + tuple.{_1}

let m = f arr
