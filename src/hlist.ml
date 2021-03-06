
type void = Types.void

type _ t =
  | Nil: void t
  | Cons:
      'elt * 'a t -> ( 'elt * 'a ) t

let rec length: type a. a t -> int = function%with_ll
  | [] -> 0
  | _::q -> 1 + length q

let to_tuple_2 = function
    | [%ll? [a;b] ] -> a, b

let from_tuple_2 (a,b) = [%ll a; b]

let rec make_kont: type l res f h focus.
      <
    make: <res:res; typ:f; list:l; tail:Types.void >;
    homogeneous:h; focus:focus;
  > Index.t
  -> ( l t -> res )
  -> f  = function
  | Index.Z -> fun kont -> kont Nil
  | Index.S k ->
    let k = make_kont k in
    fun kont x -> k ( fun acc -> kont @@ Cons(x,acc) )

let make k = make_kont k (fun x -> x)
let hlist = make
