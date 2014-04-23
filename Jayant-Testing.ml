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
    val mul : t -> t -> t
    (* val det : t -> int
    val inverse : t -> t
    val solve : t -> t -> t *)
    val lu_decomp : t -> elt array -> t * elt array
    val solve : t -> elt array -> t * elt array
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
    end

module IntRing = 
    struct 
        type t = int
        let zero = 0
        let one = 1
        let add = (+)
        let sub = (-)
        let mul = ( * )         
        let div = (/)   
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

let lu_decomp m =
    let rows = Array.length m in
    let cols = Array.length m.(0) in
    let lower = zero rows cols in
    let upper = zero rows cols in
    for k = 0 to rows - 1 do
        lower.(k).(k) <- 1.;
        for i = k + 1 to cols - 1 do
            lower.(i).(k) <- M.div m.(i).(k) m.(k).(k);
            for j = k + 1 to cols - 1 do
                m.(i).(j) <- M.sub m.(i).(j) (M.mul lower.(i).(k) * m.(k).(j));
            done;
        done;
        for j = k to cols - 1 do
            upper.(k).(j) <- m.(k).(j);
        done;
    done;
    (upper, lower)

let solve m b =
    let length = Array.length b in
    for i = 0 to length - 1 do
        let max = i in
        for j = i + 1 to length - 1 do
            if (abs m.(j).(i)) > (abs m.(max).(i)) then max = i
        done;

        let temp_row = m.(i) in
            m.(i) <- m.(max);
            m.(max) <- temp_row;
        let temp_b = b.(i) in
            b.(i) <- b.(max);
            b.(max) <- temp_b;

        for j = i + 1 to length - 1 do
            let factor = M.div m.(j).(i) m.(i).(i) in
            b.(j) <- M.sub b.(j) (M.mul factor b.(i));
            for p = i to length - 1 do
                m.(j).(p) <- M.sub m.(j).(p) (M.mul factor m.(i).(p));
            done;
        done;    
    done; 

    let soluton = Array.make length 0 in
    for i = rows - 1 downto 0 do
        let sum = 0.0 in
        for j = i + 1 to rows - 1 do
            sum = M.add sum (M.mul a.(i).(j) solution.(j))
        done;
        solution.(i) <- M.div (M.sub b.(i) sum) m.(i).(i);
    done;
    (m, b)

    end

module FloatMatrix = MatrixFunctor (FloatRing);;
let matrix1 = FloatMatrix.of_array (Array.make_matrix 5 3 2.);;
let matrix2 = FloatMatrix.of_array (Array.make_matrix 3 5 3.);;
let (u1, l1) = FloatMatrix.LU_decomp matrix1;;
let (u2, l2) = FloatMatrix.LU_decomp matrix2;;
let (m1, sol1) = FloatMatrix.solve matrix1 (Array.make_matrix 5 1 4.);;
let (m2, sol2) = FloatMatrix.solve matrix2 (Array.make_matrix 3 1 1.);;


