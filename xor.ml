(* Machine learning test using SLAP library *)
open Slap.Size;;
open Slap.Io;;
open Slap.D;;
open Slap.Common;;

let bias_input = -1.;; (* Bias input node value *)

(* Compute C = A - B    *)
let diff a b = 
  let c = Mat.copy a in
  Mat.axpy ~alpha: (-1.) b c;
  c;;

(* Compute C = A * k    *)
let mul mat k = Mat.map (fun x->x *. k) mat;;

(* Compute dot multiplication between two matrices *)
let dot = gemm ~transa:normal ~transb:normal;;

(* Compute dot multiplicate between two matrices with first one transposed *)
let dot_t = gemm ~transa:trans ~transb:normal;;

(* Threshold function *)
let threshold x = if x > 0. then 1. else 0. ;; 

let inputs = [%mat[
                    [0.; 0.; bias_input];
                    [0.; 1.; bias_input];
                    [1.; 0.; bias_input];
                    [1.; 1.; bias_input]]];;

let targets = [%mat[
                     [0.];
                     [1.];
                     [1.];
                     [1.]]];;

let () = 
  let weights = 
    let (rows, cols) = (Mat.dim2 inputs, Mat.dim2 targets) in 
    Mat.random rows cols in
  let eta = 0.25 in (* Learning rate *)
  let t = 10 in (* Number of learning steps *)

  for i=0 to t do
    let hypothesis = Mat.map threshold (dot inputs weights) in (* Calculate hypothesis matrix based on 0 as threshold value *)
    let correction = mul ( dot_t inputs (diff hypothesis targets)) eta in (* Calculate weights correction values *)
    Mat.axpy ~alpha: (-1.) correction weights; (* Update weights as weights = weights - correction     *)
    Format.printf "%a\n @." Slap.Io.pp_fmat hypothesis; (* Print current hypothesis matrix *)
  done;;

