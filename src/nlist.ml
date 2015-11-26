type z = Nil_z
type 'a succ = Nil_succ

type ('elt,'nat) t =
  | Nil: ('elt,'z->'z) t
  | Cons: 'elt * ('elt,'a -> 'r ) t -> ('elt, 'a -> 'r succ) t

let rec map: type nat.
  ( 'elt -> 'elt_2 ) -> ('elt, nat) t -> ('elt_2, nat) t =
fun f l -> match l with
| Nil -> Nil
| Cons(hd,tail) -> Cons(f hd, map f tail)


let fix: ('elt, z -> 'nat ) t -> ('elt, z -> 'nat ) t = fun l -> l
