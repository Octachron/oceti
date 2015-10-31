type void = Types.void
type _ t =
    | Z :  <
      focus: <list:'a * 'b; left: void; selected: 'a; right: 'b>;
      fusion: <list:'a2 * 'b2; res:'o2; tail:'o2>;
      f:<typ:'res; res:'res>
      > t
    | S : <
        focus: <list:'l;  left: 'le; selected: 's; right: 'r>;
        fusion:<list:'l2; res:'r2; tail:'t2>;
        f:<typ:'f; res:'res>
      > t
      -> <
        focus: <list:'a*'l; left: 'a * 'le; selected: 's; right: 'r >;
        fusion:<list:'elt2*'l2; res:'elt2 * 'r2; tail:'t2>;
        f:<typ:'a -> 'f; res:'res>
      > t


let rec to_int: type a. a t -> int = function Z -> 0 | S k -> 1 + to_int k

module Defs = struct
let _0 = Z
let _1 = S _0
let _2 = S _1
let _3 = S _2
let _4 = S _3
let _5 = S _4
let _6 = S _5    

end
