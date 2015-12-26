open Slap.D;;

(* Compute C = A - B    *)
let diff a b = 
  let c = Mat.copy a in
  Mat.axpy ~alpha: (-1.) b c;
  c;;

(* Compute C = A * k    *)
let mul mat k = Mat.map (fun x->x *. k) mat;;

(* Compute dot multiplication between two matrices *)
let dot = gemm ~transa:Slap.Common.normal ~transb:Slap.Common.normal;;

(* Compute dot multiplicate between two matrices with first one transposed *)
let dot_t = gemm ~transa:Slap.Common.trans ~transb:Slap.Common.normal;;

