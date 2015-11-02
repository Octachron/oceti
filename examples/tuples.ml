open Index.Defs
open Tuple


let x = make _2 1 "hi"
let y = make _3 [2] [|6|] "hi"    
let xy = fusion _1 x y

let k = x.{_1}
let w = map _0 float x

let h1 = make _3 1 2 "hi"

let h2 = map_all _1 float h1

  let arr =
  make _5 1 2 4 (fun x ->  x lsl 4) (+)

  let n = arr.{_0}
  let hi = arr.{_1}
  let some = arr.{_2}
  let k = arr.{_3} n             
  let n' =
    (* arr.{_0} <- 4; *)
    arr.{_0}



let s = let open Index.Defs in Slice.make _1 arr

let id x = x
open Index.Defs
let f tuple =
  tuple.{_4} tuple.{_2} @@ tuple.{_0} + tuple.{_1}

let m = f arr

