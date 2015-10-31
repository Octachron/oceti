type void = Types.void
type _ t =
  | Z :  <
      homogeneous:
        <
          l1 : < mono:'h1_a; list: 'h1_a * 'h1_t; tail:'h1_t >;
          l2: < mono: 'h2_a; list: 'h2_a * 'h2_t; tail: 'h2_t >
        >; 
      focus: <list:'a * 'b; left: void; selected: 'a; right: 'b>;
      fusion: <list:'a2 * 'b2; sel:'a2;  res:'o2; tail:'o2; right:'b2 >;
      f:<typ:'res; res:'res>
      > t
  | S : <
      homogeneous:
        <
          l1 : < mono:'h1_m; list: 'h1_l; tail:'h1_t >;
          l2: < mono: 'h2_m; list: 'h2_l; tail:'h2_t >
        >; 
      focus: <list:'l;  left: 'le; selected: 's; right: 'r>;
      fusion:<list:'l2; sel:'s2; res:'r2; tail:'t2; right:'b2 >;
        f:<typ:'f; res:'res>
      > t
      -> <
        homogeneous:
          <
            l1 : < mono:'h1_m; list: 'h1_m * 'h1_l; tail:'h1_t >;
            l2: < mono: 'h2_m; list: 'h2_m * 'h2_l; tail:'h2_t >
          >; 
        focus: <list:'a*'l; left: 'a * 'le; selected: 's; right: 'r >;
        fusion:<list:'elt2*'l2; res:'elt2 * 'r2; tail:'t2; sel:'s2; right:'b2>;
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
