
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
    val mul : t -> t -> t
    val det : t -> int
    val inverse : t -> t
    val solve : t -> t -> t
end

