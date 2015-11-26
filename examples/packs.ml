open Pack
    
let a = Array.make 13 0.

let%ppx_listlike ls = { ll with cons = "Cons'"; nil="Nil'"}

let layout = [%ll vec 2; mat 3 3; vec 2]

let%with_ls [v; m; w] = split layout a

let x, y , z = v.[0], m.[2,1], w.[0]
