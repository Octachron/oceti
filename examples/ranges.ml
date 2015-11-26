open Range

let pi = 4. *. atan 1.

let square r = r ^ r
let unit n = 0. --> 1. @@ 1. /. float n

let polar f r theta =
  f ( r *. cos theta ) ( r *. sin theta )

let spherical r f theta phi =
  let sin_phi = r *. sin phi and cos_phi = r *. cos phi in
  f ( cos theta *. cos_phi ) ( sin theta *. cos_phi ) sin_phi  


let translate tx ty tz f x y z = f (x +. tx) ( y +. ty ) ( z +. tz )

let cartesian f x y =
  let r = sqrt @@ x *. x +. y *. y in
  let theta =
    if x = 0. && y < 0. then
      -. pi
    else
      atan @@ x /. ( y +. r ) in
  f r theta

let thetas = -. pi -->  pi  @@ 0.02 *. pi
let phis = -. pi /. 2. --> pi /. 2.  @@ 0.02 *. pi

let radius = 0. --> 1. @@ 0.1
let disc = polar $$ radius ^ thetas

let sphere = spherical 1. $$ thetas ^ phis
let two_spheres = sphere |+| ( translate 0. 0. 1. $$ sphere )

let grid = cartesian $$ disc


let () =
  iter_on two_spheres @@ Printf.printf "%f %f %f\n"

