(* Machine learning test using SLAP library *)
open Lacaml.D;;
open ML;;

let bias_input = -1.;; (* Bias input node value *)

(* Threshold function *)
let threshold x = if x > 0. then 1. else 0. ;; 

let inputs = Mat.of_array [|
                    [|0.; 0.; bias_input|];
                    [|0.; 1.; bias_input|];
                    [|1.; 0.; bias_input|];
                    [|1.; 1.; bias_input|] |];;

let targets = Mat.of_array [|
                     [|0.|];
                     [|1.|];
                     [|1.|];
                     [|1.|] |];;

let () = 

  Random.self_init(); (* Initialize a random seed *)

  let weights = 
    let (rows, cols) = (Mat.dim2 inputs, Mat.dim2 targets) in 
    Mat.random rows cols in
  let eta = 0.25 in (* Learning rate *)
  let t = 10 in (* Number of learning steps *)

  for i=0 to t do
    let hypothesis = Mat.map threshold (dot inputs weights) in (* Calculate hypothesis matrix based on 0 as threshold value *)
    let correction = mul ( dot_t inputs (diff hypothesis targets)) eta in (* Calculate weights correction values *)
    Mat.axpy ~alpha: (-1.) correction weights; (* Update weights as weights = weights - correction     *)
    Format.printf "%a\n @." pp_mat hypothesis; (* Print current hypothesis matrix *)
  done;;

