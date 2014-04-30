open Ring
open Matrix 

(* LU & GAUSSIAN TESTING *)

module FloatMatrix = MatrixFunctor (FloatRing);;

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
assert (FloatMatrix.to_array (FloatMatrix.mul u1 l1) = matrix1);;
assert (FloatMatrix.to_array (FloatMatrix.mul u2 l2) = matrix2);;

let (m1, sol1) = FloatMatrix.solve (FloatMatrix.of_array matrix3) (Array.create 2 1.);;
assert (sol1 = [|1.; 1.|]);;

let matrix4 = [|[|0.; 1.; 1.|]; [|2.; 4.; -2.;|]; [|0.; 3.; 15.|]|];;
let (m4, sol4) = FloatMatrix.solve
           (FloatMatrix.of_array matrix4) ([|4.; 2.; 36.|]);;
assert (sol4 = [|-1.; 2.; 2.|]);;

let matrix5 = [|[|10.; -7.; 0.|]; [|-3.; 2.; 6.|]; [|5.; -1.; 5.|]|];;
let (u5, l5, p5) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix5);;
assert (FloatMatrix.to_array u5 =
      [|[|10.; -7.; 0.|]; [|0.; 2.5; 5.|]; [|0.; 0.; 6.2|]|]);;
assert (FloatMatrix.to_array l5 = 
      [|[|1.; 0.; 0.|]; [|0.5; 1.; 0.|]; [|-0.3; -0.04; 1.|]|]);;
assert (FloatMatrix.to_array p5 = 
      [|[|1.; 0.; 0.|]; [|0.; 0.; 1.|]; [|0.; 1.; 0.|]|]);;

(* STRASSEN TESTING *)

(* Test FloatRing *)

let matrix1f = FloatMatrix.of_array (Array.make_matrix 5 3 2.);;
let matrix2f = FloatMatrix.of_array (Array.make_matrix 3 5 3.);;
FloatMatrix.to_array (FloatMatrix.mul matrix1f matrix2f);;

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

FloatMatrix.to_array (FloatMatrix.mul matrix3f matrix4f);;

(* Test IntRing *)

module IntMatrix = MatrixFunctor (IntRing);;
let matrix1i = IntMatrix.of_array (Array.make_matrix 5 3 2);;
let matrix2i = IntMatrix.of_array (Array.make_matrix 3 5 3);;
IntMatrix.to_array (IntMatrix.mul matrix1i matrix2i);;

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

IntMatrix.to_array (IntMatrix.mul matrix3i matrix4i);;