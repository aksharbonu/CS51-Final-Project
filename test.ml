open Ring
open Matrix
open Core.Std

(* 
  - Checks for floating point impercision
  - Invariant is same dimensions
*)

let is_equal m1 m2 =
  let epsilon = 0.001 in
  Array.for_all2_exn m1 m2 ~f:(fun row1 row2 -> Array.for_all2_exn row1 row2 ~f:(fun v1 v2 -> (Float.abs (v1 -. v2)) <= epsilon))


module FloatMatrix = MatrixFunctor (FloatRing);;

(* Test of_array invariant *)

(* 
  This will raise an exception: 
  let bad_matrix = [|[|8.; 2.; 9.;0.|]; [|4.; 9.; 4.|]; [|6.; 7.; 9.|]|];;
  assert (FloatMatrix.of_array bad_matrix = FloatMatrix.of_array bad_matrix)

*)

(* LU & GAUSSIAN TESTING *)

let matrix1 = [|[|4.; 3.|]; [|6.; 3.|]|];;
assert (FloatMatrix.det (FloatMatrix.of_array matrix1) = -6.);;

let matrix2 = [|[|8.; 2.; 9.|]; [|4.; 9.; 4.|]; [|6.; 7.; 9.|]|];;
let matrix3 = [|[|1.; 0.|]; [|0.; 1.|]|];;

assert (FloatMatrix.det (FloatMatrix.of_array matrix3) = 1.);;

let (u1, l1, p1) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix1);;
let (u2, l2, p2) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix2);;
let (u3, l3, p3) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix3);;

assert (FloatMatrix.to_array u1 = [|[|6.; 3.|]; [|0.; 1.|]|]);;
assert (FloatMatrix.to_array l1 = [|[|1.; 0.|]; [|(2./.3.); 1.|]|]);;
assert (FloatMatrix.to_array u2 =
      [|[|8.; 2.; 9.|]; [|0.; 8.; -0.5|]; [|0.; 0.; 2.59375|]|]);;
assert (FloatMatrix.to_array l2 = 
      [|[|1.; 0.; 0.|]; [|0.5; 1.; 0.|]; [|0.75; 0.6875; 1.|]|]);;
assert (FloatMatrix.to_array u3 = matrix3);;
assert (FloatMatrix.to_array l3 = matrix3);;


assert (is_equal (FloatMatrix.to_array (FloatMatrix.mul l1 u1)) (FloatMatrix.to_array (FloatMatrix.mul p1 (FloatMatrix.of_array matrix1))));;
assert (is_equal (FloatMatrix.to_array (FloatMatrix.mul l2 u2)) (FloatMatrix.to_array (FloatMatrix.mul p2 (FloatMatrix.of_array matrix2))));;


let (m1, sol1) = FloatMatrix.solve (FloatMatrix.of_array matrix3) 
                                    (FloatMatrix.of_array ([|
                                                          [|1.|];
                                                          [|1.|]; 
                                                          |]));;
assert (FloatMatrix.to_array sol1 = 
                                    [|
                                    [|1.|];
                                    [|1.|]; 
                                    |]);;



let matrix4 = [|[|0.; 1.; 1.|]; [|2.; 4.; -2.;|]; [|0.; 3.; 15.|]|];;
let (m4, sol4) = FloatMatrix.solve (FloatMatrix.of_array matrix4) 
                                    (FloatMatrix.of_array  ([|
                                                          [|4.|];
                                                          [|2.|]; 
                                                          [|36.|];
                                                          |]));;
assert (FloatMatrix.to_array sol4 = 
                                    [|
                                    [|-1.|];
                                    [|2.|]; 
                                    [|2.|];
                                    |]);;


let matrix5 = [|[|10.; -7.; 0.|]; [|-3.; 2.; 6.|]; [|5.; -1.; 5.|]|];;
let (u5, l5, p5) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix5);;
assert (is_equal (FloatMatrix.to_array u5)
      [|[|10.; -7.; 0.|]; [|0.; -0.1; 6.|]; [|0.; 0.; 155.|]|]);;
assert (is_equal (FloatMatrix.to_array l5) 
      [|[|1.; 0.; 0.|]; [|-0.3; 1.; 0.|]; [|0.5; -25.; 1.|]|]);;
assert (is_equal (FloatMatrix.to_array p5)
      [|[|1.; 0.; 0.|]; [|0.; 1.; 0.|]; [|0.; 0.; 1.|]|]);; 


(* STRASSEN TESTING *)

(* Test FloatRing *)

let matrix1f = FloatMatrix.of_array (Array.make_matrix ~dimx:5 ~dimy:3 2.);;
let matrix2f = FloatMatrix.of_array (Array.make_matrix ~dimx:3 ~dimy:5 3.);;
assert (FloatMatrix.to_array (FloatMatrix.mul matrix1f matrix2f) = 
		[|[|18.; 18.; 18.; 18.; 18.|]; 
		[|18.; 18.; 18.; 18.; 18.|];
  		[|18.; 18.; 18.; 18.; 18.|]; 
  		[|18.; 18.; 18.; 18.; 18.|];
  		[|18.; 18.; 18.; 18.; 18.|]|]);;

let matrix3f = FloatMatrix.of_array 
[|
[|1.; 2.; 3.; 4.|];
[|5.; 6.; 7.; 8.|]; 
[|-1.; -2.; -3.; -4.|];
[|-5.; -6.; -7.; -8.|]; 
[|1.; 2.; 3.; 4.|];
|];;

let matrix4f = FloatMatrix.of_array 
[|
[|1.; 2.; 3.; 4.; 5.|];
[|5.; 6.; 7.; 8.; 9.|]; 
[|-1.; -2.; -3.; -4.; -5.|];
[|-5.; -6.; -7.; -8.; -9.|]; 
|];;

assert (FloatMatrix.to_array (FloatMatrix.mul matrix3f matrix4f) = 
		[|[|-12.; -16.; -20.; -24.; -28.|]; 
		[|-12.; -16.; -20.; -24.; -28.|];
  		[|12.; 16.; 20.; 24.; 28.|]; 
  		[|12.; 16.; 20.; 24.; 28.|];
  		[|-12.; -16.; -20.; -24.; -28.|]|]);;

(* Test IntRing *)

module IntMatrix = MatrixFunctor (IntRing);;
let matrix1i = IntMatrix.of_array (Array.make_matrix ~dimx:5 ~dimy:3 2);;
let matrix2i = IntMatrix.of_array (Array.make_matrix ~dimx:3 ~dimy:5 3);;

assert (IntMatrix.to_array (IntMatrix.mul matrix1i matrix2i) = 
		[|[|18; 18; 18; 18; 18|]; 
		[|18; 18; 18; 18; 18|]; 
		[|18; 18; 18; 18; 18|];
  		[|18; 18; 18; 18; 18|]; 
  		[|18; 18; 18; 18; 18|]|]);;

let matrix3i = IntMatrix.of_array 
[|
[|1; 2; 3; 4|];
[|5; 6; 7; 8|]; 
[|-1; -2; -3; -4|];
[|-5; -6; -7; -8|]; 
[|1; 2; 3; 4|];
|];;

let matrix4i = IntMatrix.of_array 
[|
[|1; 2; 3; 4; 5|];
[|5; 6; 7; 8; 9|]; 
[|-1; -2; -3; -4; -5|];
[|-5; -6; -7; -8; -9|]; 
|];;

assert (IntMatrix.to_array (IntMatrix.mul matrix3i matrix4i) = 
		[|[|-12; -16; -20; -24; -28|]; 
		[|-12; -16; -20; -24; -28|];
  		[|12; 16; 20; 24; 28|]; 
  		[|12; 16; 20; 24; 28|];
  		[|-12; -16; -20; -24; -28|]|]);;