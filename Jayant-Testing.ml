open Core.Std

type comparison = Less | Equal | Greater;;

module type MATRIX =
sig
    exception IncompatibleDimensions
    type elt
    type t
    val of_array: elt array array -> t
    val to_array: t -> elt array array
    val zero : int -> int -> t
    val identity : int -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val scalar : elt -> t -> t
    (* val mul : t -> t -> t
    val det : t -> int
    val inverse : t -> t *)
    val solve : t -> elt array -> t * elt array
    val compare : t -> t -> comparison
    val abs : elt -> elt
    val lu_decomp : t -> t * t
end

module type RING =
    sig
        type t
        val zero : t
        val one : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val comp : t -> t -> comparison
        val abs_val : t -> t
    end

module IntRing  = 
    struct 
        type t = int
        let zero = 0
        let one = 1
        let add = (+)
        let sub = (-)
        let mul = ( * )         
        let div = (/)  
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
			 else Greater
        let abs_val v = match comp v zero with
	  | Less -> mul v (sub zero 1)
          | _ -> v
    end

module FloatRing =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        let add = (+.)
        let sub = (-.)
        let mul = ( *. )
        let div = (/.)
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
			 else Greater
        let abs_val v = match comp v zero with 
	  | Less -> mul v (sub zero one)
          | _ -> v
    end

module MatrixFunctor (M : RING) : MATRIX with type elt = M.t =
    struct
        (* 
            open LU 
            open GaussianElimination    
        *)
exception IncompatibleDimensions

        type elt = M.t

        type t = elt array array 

        let of_array = ident;;

        let to_array = ident;;

        let zero n m = Array.make_matrix n m M.zero;;

        let identity n =
            let result = zero n n in
            for i = 0 to n - 1 do
                result.(i).(i) <- M.one
            done;
            result;;

        let scalar value m1 =
            let row = Array.length m1 in
            let col = Array.length m1.(0) in 
            let result = zero row col in
                for i = 0 to Array.length m1 - 1 do
                    for j = 0 to Array.length m1.(0) - 1 do
                        result.(i).(j) <- M.mul m1.(i).(j) value
                    done;
                done;
            result;;

let do_operation m1 m2 operation = 
    let row = Array.length m1 in
    let col = Array.length m1.(0) in 
    if row = Array.length m2 && col = Array.length m2.(0) then
        (let result = zero row col in
        for i = 0 to row - 1 do
            for j = 0 to col - 1 do
                result.(i).(j) <- operation m1.(i).(j) m2.(i).(j)
            done;
        done;
        result)
    else raise IncompatibleDimensions;;

let add m1 m2 =
        do_operation m1 m2 M.add;;

let sub m1 m2 = 
        do_operation m1 m2 M.sub;;

let compare v1 v2 =
  if v1 = v2 then Equal else if v1 < v2 then Less else Greater

let abs v =
  match compare v M.zero with
  | Less -> M.mul (M.sub M.zero M.one) v
  | _ -> v
;;

let lu_decomp m =
    let rows = Array.length m in
    let cols = Array.length m.(0) in
    if rows <> cols then raise IncompatibleDimensions
    else
    let lower = zero rows cols in
    let upper = zero rows cols in
    for k = 0 to rows - 1 do
        lower.(k).(k) <- M.one;
        for i = k + 1 to cols - 1 do
            (* How to handle the case when m.(k).(k) = 0. Is that possible
               without pivoting? *)
            lower.(i).(k) <- M.div m.(i).(k) m.(k).(k);
            for j = k + 1 to cols - 1 do
                m.(i).(j) <- M.sub m.(i).(j) (M.mul lower.(i).(k) m.(k).(j));
            done;
        done;
        for j = k to cols - 1 do
            upper.(k).(j) <- m.(k).(j);
        done;
    done;
    (upper, lower)

let solve m b =
    let length = Array.length b in
    let rows = Array.length m in
    for k = 0 to length - 1 do
        let max = ref k in
        for i = k + 1 to length - 1 do
            match M.comp (abs m.(i).(k)) (abs m.(!max).(k)) with
            | Greater -> (max := i);
            | _ -> ();
        done;

        let temp_row = m.(k) in
            m.(k) <- m.(!max);
            m.(!max) <- temp_row;
        let temp_b = b.(k) in
            b.(k) <- b.(!max);
            b.(!max) <- temp_b;

        for i = k + 1 to length - 1 do
            let factor = M.div m.(i).(k) m.(k).(k) in
            b.(i) <- M.sub b.(i) (M.mul factor b.(k));
            for j = k to length - 1 do
                m.(i).(j) <- M.sub m.(i).(j) (M.mul factor m.(k).(j));
            done;
        done;    
    done; 

    let solution = Array.create length M.zero in
    for i = rows - 1 downto 0 do
        let sum = M.zero in
        for j = i + 1 to rows - 1 do
            sum = M.add sum (M.mul m.(i).(j) solution.(j))
        done;
        solution.(i) <- M.div (M.sub b.(i) sum) m.(i).(i);
    done;
    (m, solution)

    end

module FloatMatrix = MatrixFunctor (FloatRing);;
let matrix1 = [|[|4.; 3.|]; [|6.; 3.|]|];;
let matrix2 = [|[|8.; 2.; 9.|]; [|4.; 9.; 4.|]; [|6.; 7.; 9.|]|];;
let matrix3 = [|[|1.; 0.|]; [|0.; 1.|]|];;
let (u1, l1) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix1);;
let (u2, l2) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix2);;
let (u3, l3) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix3);;
assert (FloatMatrix.to_array u1 = [|[|4.; 3.|]; [|0.; -1.5|]|]);;
assert (FloatMatrix.to_array l1 = [|[|1.; 0.|]; [|1.5; 1.|]|]);;
assert (FloatMatrix.to_array u2 =
	  [|[|8.; 2.; 9.|]; [|0.; 8.; -0.5|]; [|0.; 0.; 2.59375|]|]);;
assert (FloatMatrix.to_array l2 = 
	  [|[|1.; 0.; 0.|]; [|0.5; 1.; 0.|]; [|0.75; 0.6875; 1.|]|]);;
assert (FloatMatrix.to_array u3 = matrix3);;
assert (FloatMatrix.to_array l3 = matrix3);;
(* Need Akshar's Multiplication function in order to test the two below
assert (FloatMatrix.to_array (FloatMatrix.mul u1 l1) = matrix1);;
assert (FloatMatrix.to_array (FloatMatrix.mul u2 l2) = matrix2);;
 *)
let (m1, sol1) = FloatMatrix.solve 
		   (FloatMatrix.of_array matrix3) (Array.create ~len:3 1.);;
let (m2, sol2) = FloatMatrix.solve 
		   (FloatMatrix.of_array matrix3) (Array.create ~len:3 1.);;

let matrix4 = [|[|1.|]|];;
let (m3, sol3) = FloatMatrix.solve (FloatMatrix.of_array matrix4) (Array.create ~len:1 1.);;


