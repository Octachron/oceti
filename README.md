Oceti is a disparate collections of experimental data structures, all relying on some form of type-level computation. They are mainly intended as brainteasers or unreliable prototypes. Some of the most interesting examples are the reimplementation of tuples as immutable heteregenous array in `src/tuple.ml` or composable ranges in `src/range.ml`.

### Tuple

As an example, the tuple module allow you to define a tuple with

```OCaml
        open Tuple
        open Index.Defs (* definitions of _1, ..., _10 *)
        let x = tuple _3 1 "hi" (+)
```

Elements of the newly defined tuple can be then accessed using the bigarray index operator `.{}`:

```OCaml
  let two = 1 + x.{_0} (* or 1 + get _0 x *)
```
Note that the index starts at `0`. Unfortunately, the contrived construction used to define these tuple types disallows pattern matching. However, having an unified tuple family has some advantages:
```OCaml
  let y = tuple _2 [] 3

  let f x y =
      let ( % ) = x.{_2} in
      x.{_0} % y.{_1}

  let four = f x y
```

These unified tuples are immutable. One way to construct new tuple is to use `map` to apply a given function `f` to a specific element:

```OCaml
  let w = map _1 ( fun s -> s ^ "erophant" ) x
  let hierophant = w.{_1}
```
Another possibility is to append two tuples together
```OCaml
 let x_y = append _2 x y
```
