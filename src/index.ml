type void = Types.void
type _ t =
  | Z :  <
      homogeneous:
        <
          l1 : < mono:'h1_a; list: 'h1_a * 'h1_t; tail:'h1_t >;
          l2: < mono: 'h2_a; list: 'h2_a * 'h2_t; tail: 'h2_t >
        >; 
      focus: <
        list:'a2 * 'b2;
        zipper: <left:void; sel:'a2; right:'b2>;
        open_: <list:'o2; tail:'o2>
      >;
      make:<typ:'res; res:'res; list:'t_f; tail:'t_f >
      > t
  | S : <
      homogeneous:
        <
          l1 : < mono:'h1_m; list: 'h1_l; tail:'h1_t >;
          l2: < mono: 'h2_m; list: 'h2_l; tail:'h2_t >
        >; 
      focus: <
        list:'l;
        zipper:<left: 'le; sel: 's; right: 'r>;
        open_: < list:'lo; tail: 'to_ >
      >;
      make: <typ:'f; res:'res; list:'l_f; tail:'t_f>
      > t
      -> <
        homogeneous:
          <
            l1 : < mono:'h1_m; list: 'h1_m * 'h1_l; tail:'h1_t >;
            l2: < mono: 'h2_m; list: 'h2_m * 'h2_l; tail:'h2_t >
          >; 
        focus: <
          list:'a * 'l;
          zipper: < left: 'a * 'le; sel: 's; right: 'r >;
          open_: <list:'a * 'lo; tail: 'to_ >
        >;
        make: <typ:'a_f -> 'f; res:'res; list: 'a_f * 'l_f; tail:'t_f  >
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
