open Lacaml.D;;

(* Computes c = a - b    *)
let diff a b = 
  let c = lacpy a in
  Mat.axpy ~alpha: (-1.) b c;
  c;;

(* Computes C = A * k    *)
let mul mat k = Mat.map (fun x -> x*.k) mat;;

(* Computes dot multiplications between two matrices with first one transposed *)
let dot_t = gemm ~transa:`T;;

(* Squares elementwise a given matrix *)
let squared mat = Mat.map (fun x -> x**2.) mat;;

(* Generates a Matrix initialized with all ones *)
let ones n m = Mat.make n m 1.;;

(* Generates a Matrix initialized with all zeros *)
let zeros n m = Mat.make n m 0.;;

(* Appends a given column matrix into matrix-array *)
let append_col col mat = 
  let append arr i x  = Array.append x arr.(i) in
  let temp = Mat.to_array col in
  Array.mapi (append mat) temp;;

(* Convert an array into a column Matrix (i.e Nx1) *)
let column_mat arr =
  Mat.transpose( Mat.of_array [|arr|] );;

(* tokenizes a string given a separator. Returns a list of strings*)
let tokenize ?(sep=",") line  = 
  Array.of_list( Str.split (Str.regexp (sep ^ "+")) line );;

(* Converts an array of strings into an array of floats *)
let floatize arr = 
  Array.map (fun x -> float_of_string x) arr;;

(* rescaling using (x - min) / (max - min) *)
let normalize_col mat c = 
  let col = Mat.col mat c in
  let min = Vec.min col in
  let max_min = (Vec.max col) -. (min) in 
  for i = 1 to Mat.dim1 mat do
    mat.{i,c} <- (mat.{i,c} -. min) /. max_min;
  done;;

(* normalizes every column of mat from a given column *)
let normalize ?(from_col=1) mat = 
  for i = from_col to Mat.dim2 mat do
    normalize_col mat i;
  done;;

(* loads data from a file in a column-separeted fashion.
 * Every line is considered as a row, which is splitted along a given separator
 * (ususally a space or a comma). Each element is then stored in the following way:
 * - if el is last element then it is the output (y) and it is stored inside a List
 * of float elements;
 * - every other element is packed up and stored as an input variable (x) inside
 * an array which is then appended to a List.
 * At the end of the procedure Lists x and y are reversed and converted into arrays.
 * The following result is of the format:
 * x = [| ...; [| ... |]; ...  |]
 * y = [| y1; y2; ... ...;  yn |]
 *
 * returned as (x,y) *)

let load_data ?(from_col=0) file = 
  let stream = open_in file in 
  let rec aux x y =
    try
      let tokens = tokenize (input_line stream) in
      let n = Array.length tokens in
      aux ( (Array.sub tokens from_col (n-1-from_col))::x)
        ( (float_of_string (tokens.(n-1)))::y)
    with End_of_file -> 
      Array.of_list (List.rev (List.map floatize x)),
      Array.of_list (List.rev y)
  in aux [] [] ;;


