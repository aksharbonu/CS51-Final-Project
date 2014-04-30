open Core.Std

(* A generic module to define element types in the matrices *)
module type RING =
    sig
        type t
        val zero : t
        val one : t
        val epsilon : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val comp : 'a -> 'a -> Ordering.t
        val abs_val : t -> t
        val print_elt : t -> unit
    end

(* A module definition to use integers in matrices *)
module IntRing  = 
    struct 
        type t = int
        let zero = 0
        let epsilon = 0
        let one = 1
        let add = (+)
        let sub = (-)
        let mul = ( * )         
        let div = (/)  
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
             else Greater
        let abs_val v = abs v
        let print_elt s = print_endline (Int.to_string s)
    end

(* A module definition to use floating-point numbers in matrices *)
module FloatRing =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        (* Small value for comparisons - cannot use = *)
        let epsilon = 0.0001
        let add = (+.)
        let sub = (-.)
        let mul = ( *. )
        let div = (/.)
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
             else Greater
        let abs_val v = Float.abs v
        let print_elt s = print_endline (Float.to_string s)
    end