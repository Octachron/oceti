
module Flists = struct
  open Flist
  let f x y g = g x y
  let l = [%ll 1; 2; (+) ]
  let n = f $ l
end

module Hlists = struct
  open Index.Defs
  open Hlist
  let list = hlist _3 (+) 2 "A"
end
