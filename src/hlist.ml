
type void

type _ t = 
  | Nil: void t
  | Cons:
      'elt * 'a t -> ( 'elt * 'a ) t

let rec length: type a. a t -> int = function%with_ll
  | [] -> 0
  | _::q -> 1 + length q

let to_tuple_2 = function
    | [%ll? [a;b] ] -> a, b

let to_tuple_2 (a,b) = [%ll a; b]
