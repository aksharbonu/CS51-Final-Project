module type Ring =
    sig
        type t
        val zero : t
        val one : t
        val add : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
    end

module IntRing = 
    struct 
        type t = int
        let zero = 0
        let one = 1
        let add = (+)
        let mul = ( * )         
        let div = (/)   
    end;;

module FloatRing =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        let add = (+.)
        let mul = ( *. )
        let div = (/.)
    end