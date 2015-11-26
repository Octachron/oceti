open Index.Defs
open Tuple


let x = tuple _2 1 "hi"
let y = tuple _3 [2] [|6|] "hi"
let x_y = append _1 x y

let k = x.{_1}
let w = map _0 float x

let h1 = tuple _3 1 2 "hi"

let h2 = map_all _1 float h1

  let arr =
  tuple _5 1 2 4 (fun x ->  x lsl 4) (+)

  let n = arr.{_0}
  let hi = arr.{_1}
  let some = arr.{_2}
  let k = arr.{_3} n
  let n' =
    (* arr.{_0} <- 4; *)
    arr.{_0}

let s = Slice.create _1 arr

let id x = x
open Index.Defs
let f tuple =
  tuple.{_4} tuple.{_2} @@ tuple.{_0} + tuple.{_1}

let m = f arr

module README = struct
  open Tuple
  open Index.Defs (* definitions of _1, ..., _10 *)
  let x = tuple _3 1 "hi" (+)

  let two = 1 + x.{_0} (* or 1 + get _0 x *)

  let y = tuple _2 [] 3

  let f x y =
    let ( % ) = x.{_2} in
    x.{_0} % y.{_1}

  let four = f x y

  let w = map _1 ( fun s -> s ^ "erophant" ) x
  let hierophant = w.{_1}

  let x_y = append _2 x y
end
