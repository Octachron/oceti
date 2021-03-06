(** Composable range *)

(** Integer range *)
module Int = struct
type t = { start: int ; end_:int }
let len {start;end_} = start - end_
let range ?(start=0) end_ = {start; end_ = end_ - 1 }
let iter f {start;end_} =
  for i = start to end_ do f i done
end

(** A range is either:*)
type _ t =
  | Lin : Int.t -> < f:int->'a; t:'a > t
  (** an integer range *)
  | LinMap: (int -> 'elt ) * Int.t ->
    <f:'elt->'t; t:'t > t
  (** a linear range:  an integer domain composed with a function
      A linear range has a dimension of 1.
  *)
  | Product :
      <f:'elt->'t; t:'t > t  * <f:'f; t:'t > t
    -> <f: 'elt -> 'f; t:'t > t
  (** the product of a linear domain with another range.
      Increase the dimension by one.
  *)
  | Map: ('g->'f) * <f:'f; t:'t> t -> <f:'g;t:'t> t
  (** a range mapped to a new range. Such a map does not need
      to preserve the dimension of the range.
  *)
  | Seq : 'a t list -> 'a t
  (** a sequence of ranges *)

let range ?(start=0) end_ = Lin ( Int.range ~start end_ )
let integer = range

let linrange start stop step =
  let len =  1 + int_of_float ( (stop -. start) /. step ) in
  let f n =  start +.  float_of_int n  *.  step in
  LinMap( f, Int.range len )
let linear =linrange

module Helper = struct
let ( <<- ) f g x = f @@ g x
let ( ->> ) f g x = x |> f |> g
end
open Helper

let rec length: type a. a t -> int = function
  | Lin n -> Int.len n
  | Product (a,b) -> length a * length b
  | LinMap (_,r) -> Int.len r
  | Map (_,r) -> length r
  | Seq l -> List.fold_left (fun acc x -> acc + length x) 0 l

let rec iter: type f. f -> <f:f; t:unit> t -> unit =
  fun f -> function
  | Lin l -> Int.iter f l
  | LinMap (g,l) -> Int.iter (f <<- g) l
  | Product (lin,rest) ->
    let inner i = iter (f i) rest in
    iter inner lin
  | Map ( funct, r ) -> iter (funct f) r
  | Seq rs -> List.iter (iter f) rs

let rec fold: type acc f. (acc -> f ) -> acc
  -> <f: f; t:acc> t -> acc =
  fun f acc range -> match range with
    | Lin l ->
      let acc = ref acc in
      for i = l.start to l.end_ do
        acc := f !acc i
      done;
      !acc
    | LinMap (g,l) ->
      let acc = ref acc in
      for i = l.start to l.end_ do
        acc := f !acc @@ g i
      done;
      !acc
    | Product ( lin, rest) ->
      let inner acc i = fold (fun acc -> f acc i) acc rest in
      fold inner acc lin
    | Map ( funct, r) ->
      let inner acc = funct @@ f acc in
      fold inner acc r
    | Seq rs ->
      List.fold_left (fold f) acc rs

let to_list_k zip range =
  let kont l x = x :: l in
  let folder l = zip @@ kont l in
  fold folder  [] range

let to_list range = to_list_k (@@) range

let to_array_k zip range =
  Array.of_list @@ to_list_k zip range

let to_array range = to_array_k (@@) range

let iter_on range f = iter f range
let fold_on range f acc = fold f acc range

let rec lin_map: type elt tl. (elt->'a) -> <f:elt->tl; t:tl> t -> <f:'a -> tl; t:tl> t  =
  fun f -> function
  | Lin l -> LinMap(f,l)
  | LinMap (g,l) -> LinMap (  g ->> f, l )
  | Map (g, r ) -> Map( ( (->>) f ) ->> g   , r)
  | Product(_l, _k) -> assert false
  | Seq rs -> Seq (List.map (lin_map f) rs)

let map: type f g tl. (g->f) -> <f:f; t:tl> t -> <f:g; t:tl> t  = fun f r ->
  match[@warning "-4"] r with
  | Map(g, r) -> Map ( f ->> g , r )
  (*  LinMap( g, l ) ≡ Map( g ->> , l ) *)
  | LinMap (g, l) -> Map( (*fun h -> (f h) |- g*) f ->> ( (->>) g ) , Lin l)
  | x -> Map(f,x)

let seq : type a. a t -> a t -> a t =
  fun x y  ->
  match[@warning "-4"] x, y with
  | Seq l, Seq r -> Seq (l@r)
  | Seq l, x -> Seq ( l @ [x] )
  | x, Seq l -> Seq (x::l)
  | x, y  -> Seq [x;y]


module Infix = struct
  let ( $$ ) f r= map f r
  let ( $ ) f r = lin_map f r
  let (|+|) x y = seq x y
  let (^) x y = Product(x,y)

  let ( -- ) start end_ = Lin Int.{start;end_}
  let (--* ) start end_ = start -- ( end_ - 1 )
  let (-->) = linrange
end
include Infix
