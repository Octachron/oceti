open Natl

type _ t = 
  | Nil: <f:'a; t:'a; dim:z > t
  | Cons:
      'elt * <f:'ty; t:'ret; dim:'d > t -> <f: 'elt -> 'ty; t:'ret; dim:'d succ> t

let rec apply : type arg ret d .
  arg -> < f :arg; t:ret; dim:d> t -> ret = fun f l-> 
  match l with 
  | Nil -> f
  | Cons (a,Nil) -> f a
  | Cons(a, ( Cons _ as l ) ) -> apply (f a ) l

module Infix = struct
  let (@@) f l = apply f l
  let (|>) l f = apply f l  
end
