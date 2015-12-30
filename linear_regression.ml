open ML;;
open Lacaml.D;;

(* Calculate hypothesis with h = X * Theta *)
let hyp theta x =
  gemm x theta;;

(* Calculates model's error with error = X - Y *)
let error theta x y = 
  let h = hyp theta x in
  diff h y;;

(* Calculate Cost function J with J = 1/2m * sum(error^2)   *)
let j theta x y =
  let m = Mat.dim1 x in (* number of input vectors *)
  let error = squared (error theta x y) in (* (h - y)^2 *)
  (Mat.sum error) /. (2. *. float_of_int m);; (* 1/2m * sum ( error ) *)

(* Gradient descent algorithm using vectorized implementation of 
 * gradient = 1/m * sum_over_i(error * x_i) *)
let rec gradient_descent theta x y alpha m t = 
  Format.printf "Cost function: %f@." (j theta x y);
  Format.printf "Theta values:@\n%a@\n@." pp_mat theta;
  if t = 0 then theta
  else begin
    let error = error theta x y in
    let gradient = mul (dot_t x error) (alpha/.m) in
    gradient_descent (diff theta gradient) x y alpha m (t-1)
  end

(* Main function *)
let () = 
  Random.self_init(); (* Initializes random seed *)
  let file = "data.txt" in 
  let (features, out) = load_data file in (* Reads file and outputs two arrays *)
  let m = Array.length features in (* Number of input vectors *)
  let x = Mat.of_array (append_col (ones m 1) features) in (* adds a column of ones to the features *)
  let y = column_mat out in (* creates a column-matrix for y *)
  let theta = Mat.random (Mat.dim2 x) 1 in (* initializes theta with random values between -1 and 1 *)
  let alpha = 0.001 in (* sets learning rate to a small value *)
  normalize x ~from_col:2; (* normalizes input and output matrices, except for first column of X *)
  normalize y;
  let new_theta = gradient_descent theta x y alpha (float_of_int m) 100000 in (*gradient descent *)
  ();;
