
exception Invalid_dimension of int * int 
type array_tmp = float array
type array = array_tmp

type z = Nil_z
type 'a succ = Nil_succ

type _ dim =
  | S : (unit) dim
  | Vec: int -> (int) dim
  | Mat: int * int -> ( int * int ) dim
  | T3: int * int * int -> ( int * int * int ) dim

type (_) n_list =
  | Nil : unit n_list
  | Cons: 'a dim * 'b n_list -> ('a * 'b ) n_list 

let size: type t. t dim -> int = function
  | S -> 1
  | Vec n -> n
  | Mat (m,n) -> m*n
  | T3 (m,n,p) -> m*n*p

type 'b f_dim = { f : 'a. 'a dim -> 'b }

let size_f = {f=size}
let size_o: <f:'a.'a dim -> int> = object method f: 'a. 'a dim -> int =  size  end

type 'acc folder = { f : 'a. 'acc -> 'a dim -> 'acc } 

let rec fold_left: type t. 'acc folder -> 'acc -> t n_list -> 'acc  =
  fun u acc -> function
  | Nil -> acc
  | Cons (dim,q) -> fold_left u (u.f acc dim) q

let total_size l =
  let folder = { f = fun acc t -> acc + size t } in
  fold_left folder 0 l  

type 'a view  = { offset : int; stride : int; dims:'a dim }
type 'a block = {view: 'a view;  array: array }

let flat_indice_dim: type t. t dim -> t -> int = fun dims pos ->
  match dims, pos with
  | S, () -> 0
  | Vec _d, k -> k 
  | Mat (_m,n), (i,j)  -> i * n + j
  | T3 (_m,n,q), (i,j,k) -> (i * n + j) * q + k

let flat_indice {offset;stride;dims;} pos = 
  offset + stride * flat_indice_dim dims pos
             
let%indexop.stringlike get : type tuple. tuple block -> tuple -> float =
  fun block pos ->
  block.array.(flat_indice block.view pos)
and set: type tuple. tuple block -> tuple -> float -> unit =
  fun block pos x ->
    block.array.(flat_indice block.view pos) <- x

let vec n = Vec n
let mat m n = Mat(m,n)
let( *: ) m n = mat m n

type (_) n_list_2 =
  | Nil' : unit n_list_2
  | Cons': 'a block * 'b n_list_2 -> ('a * 'b ) n_list_2


let (@:) view array =
  let len = (size view.dims)*view.stride + view.offset and
  len' = Array.length array in
  if len <= len' then
    {view; array}
  else
    raise @@ Invalid_dimension (len,len')
      
let view view array = view @: array

let full_view dims = {dims;offset=0;stride=1}
let full dims array = full_view dims @: array

type to_view = { to_view : 'a. 'a dim -> 'a view }
type to_block = { to_block: 'a. 'a dim -> 'a block }

let to_view offset stride = { to_view = fun dims -> {dims;offset;stride} } 
let to_block array offset stride = { to_block = fun dims ->
    {
      array;
      view = (to_view offset stride).to_view dims
    }
  }
  
type mk_view = { f: 'a. int -> int -> 'a dim -> 'a view }
let mk_view = { f= fun offset stride dims -> {dims;offset;stride} }                  
type mk_block = { f: 'a. array -> 'a view -> 'a block }
let mk_block = { f= fun array view -> {array;view} } 

type 'acc builder = { f :'rk. 'acc -> 'rk dim -> 'rk block * 'acc } 

let rec build: type dims. 'acc builder -> 'acc -> dims n_list -> dims n_list_2 =
  fun uf acc -> function
    | Nil -> Nil'
    | Cons(dims,l) ->
      let s, acc = uf.f acc dims in
      let l = build uf acc l in
      Cons'(s, l) 


let iter_dims: type tu. tu dim -> (tu -> unit) -> unit = fun dims f ->
  let open Range in
  match dims with
  | S -> f ()
  | Vec n -> iter f (range n)
  | Mat (m,n) -> 
    for i = 0 to m-1 do
      for j = 0 to n -1 do
        f (i,j)
      done
    done
  | T3 (l,m,n) ->
    for i = 0 to l - 1 do
      for j = 0 to m-1 do
        for k = 0 to n -1 do
          f (i,j,k)
        done
      done
    done

let blit s f = iter_dims s.view.dims (fun t -> s.[t] <- f t ) 

let split l array =
  let len = total_size l
  and len' = Array.length array in
  if len <> len' then raise @@ Invalid_dimension(len,len')
  else
    build { f = fun offset dims -> mk_block.f array @@ mk_view.f offset 1 dims, offset + size_f.f dims  }
      0 l
