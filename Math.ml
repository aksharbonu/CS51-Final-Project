module type MATH =
    sig
        type t
        val zero : t
        val one : t
        val add : t -> t -> t
        val mul : t -> t -> t
    end

module IntMath = 
    struct 
        type t = int
        let zero = 0
        let one = 1
        let add = (+)
        let mul = ( * )            
    end;;

module FloatMath =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        let add = (+.)
        let mul = ( *. )
    end