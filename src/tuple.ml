
(* 'a t is internaly an Obj.t array, but this representation is
unsafe and hidden away *)
type +'a t

module Unsafe = struct
  let untype: 'a t -> Obj.t array = Obj.magic
  let transmute: Obj.t array -> 'a t = Obj.magic
end


type void = Types.void

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

let make n = Hlist.make_kont n from_list

let copy: 'a t -> 'a t = fun a ->
  a |> Unsafe.untype |> Array.copy |> Unsafe.transmute

let length x =
  x |> Unsafe.untype |> Array.length

[%%indexop
let get: 'list t -> < focus: <list:'list; selected:'r; .. > ; .. > Index.t -> 'r =
  fun array index ->
    let array  = Unsafe.untype array in
    Obj.magic array.( Index.to_int index)
(* let set: 'list t ->  < focus: <list:'list; selected:'r; .. > ; .. > Index.t -> 'r -> unit =
  fun array index value ->
    let array = Unsafe.untype array in
    array.(Index.to_int index) <- Obj.repr value *)
]


let cut_right: < focus:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l t -> 'll t =
  fun _n s -> Obj.magic s

let fusion: < fusion:<list:'x; res:'l; tail:'y; right:Types.void; .. >; .. > Index.t -> 'x t -> 'y t -> 'l t = fun _n x y ->
  let ax, ay = Unsafe.(untype x, untype y) in
  let nx, ny = length x, length y in
  let a = Array.make (nx+ny) @@ Obj.repr 0 in
  for i=0 to nx-1 do a.(i)<- ax.(i) done;
  for i=0 to ny - 1 do a.(i+nx) <- ay.(i) done;
  Unsafe.transmute a

let map:
  <  fusion: <
         list:'l;
         right:'r;
         res:'l2;
         sel:'a;
         tail:'b * 'r > ;..>
    Index.t -> ('a -> 'b ) -> 'l t -> 'l2 t = fun k f x ->
  let a = Unsafe.untype @@ copy x in
  a.(Index.to_int k) <- Obj.repr @@ f x.{k}; Unsafe.transmute a


let map_all:
  <  homogeneous: <
         l1: <
           list:'l;
           tail:'r;
           mono:'a
         >;
         l2: <
           list:'l2;
           tail:'r;
           mono:'b
         >
       >; .. >
    Index.t -> ('a -> 'b ) -> 'l t -> 'l2 t = fun k f x ->
  let a = Unsafe.untype @@ copy x in
  for k = 0 to Index.to_int k - 1 do
    a.(k) <- Obj.repr @@ f @@ Obj.magic a.(k)
  done;
  Unsafe.transmute a
    

module Slice = struct
type 'a s
type raw = {offset :int; array: Obj.t array }

let untype: 'a s -> raw = Obj.magic
let transmute: raw -> 'a s = Obj.magic                                        

let length s =
  let r = untype s in
  Array.length r.array -r.offset 

let full: 'a t -> 'a s = fun a -> transmute { offset=0; array=Unsafe.untype a }

let cut_left: < focus:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l s -> ('a*'r) s =
  fun n s -> let s = untype s in transmute { s with offset= Index.to_int n + s.offset }

let cut_right: < focus:<list:'l; left:'ll; selected:'a; right:'r >; .. > Index.t -> 'l s -> 'll s =
  fun _n s -> Obj.magic s

let cut l r s =
  s |> cut_right r |> cut_left l

let make k a =
  cut_left k @@ full a

let copy : 'a s -> 'a t = fun s ->
  let {array;offset} = untype s in
  Unsafe.transmute @@ Array.init (length s) (fun i -> array.( i + offset ) )

[%%indexop
let get: 'list s -> < focus: <list:'list; selected:'r; .. > ; .. > Index.t -> 'r =
  fun slice index ->
    let slice  = untype slice in
    Obj.magic slice.array.( slice.offset + Index.to_int index)
(*let set: 'list s ->  < focus: <list:'list; selected:'r; .. > ; .. > Index.t -> 'r -> unit =
  fun slice index value ->
    let slice  = untype slice in
    slice.array.( slice.offset + Index.to_int index) <- Obj.repr value  *)
]

end
