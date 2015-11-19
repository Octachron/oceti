open Natl

type _ t = 
  | Nil: < fun_type:'a; return:'a; arity:z > t
  | Cons:
      'elt * < fun_type:'ty; return:'ret; arity:'b > t -> < fun_type: 'elt -> 'ty; return:'ret; arity:'n succ> t

let rec apply : type f ret n.
  f -> < fun_type :f; return:ret; arity:n > t -> ret = fun f l-> 
  match l with 
  | Nil -> f
  | Cons (a,Nil) -> f a
  | Cons(a, ( Cons _ as l ) ) -> apply (f a ) l

module Infix = struct
  let (@@) f l = apply f l
  let (|>) l f = apply f l  
end
