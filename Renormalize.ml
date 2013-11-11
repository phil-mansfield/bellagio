open Signatures
open Gsl_linalg

(* Module name remapping. *)
module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)

(* Compile-time simulation parameters *)
let eq_sweeps = 100
let interactions = [| (MC.NN1, 1.0) |]
let min_site_width = 2

(* Fixed and derived simulation parameters. *)
let bts = [| MC.NN1; MC.NN2; MC.NN3; MC.SQR; MC.DMD |]
let bt_count = Array.length interactions
let min_site_count = min_site_width * min_site_width
let nn_crit_temp = 1. /. (log (1. +. sqrt 2.))

(* Command-line parameters *)
let _ = assert (Array.length Sys.argv > 2)
let width = int_of_string Sys.argv.(1) 
let trials = int_of_string Sys.argv.(2) 
let temp = if Array.length Sys.argv = 3 then nn_crit_temp 
  else float_of_string (Sys.argv.(3))
    
(* Print-out. *)
let print_parameters () =
  let string_of_bt bt =
    match bt with
      MC.NN1 -> "NN1"
    | MC.NN2 -> "NN2"
    | MC.NN3 -> "NN3"
    | MC.SQR -> "SQR"
    | MC.DMD -> "DMD"
  in
  let print_bt bt = 
    print_string (string_of_bt bt) 
  in
  let print_interaction (bt, strength) = 
    Printf.printf "(%s, %g)" (string_of_bt bt) strength 
  in
  
  Printf.printf "Simulation parameters:\n";
  Printf.printf "%16s: %16g\n%16s: %16d\n%16s: %16d\n"
    "Temperature" temp
    "Trials" trials
    "Initial Width" width;
  Printf.printf "%16s: " "Bond Types";
  Utils.print_list print_bt (Array.to_list bts);
  Printf.printf "\n";
  Printf.printf "%16s: " "Interactions";
  Utils.print_list print_interaction (Array.to_list interactions);
  Printf.printf "\n"

let count_renorms () = 
  let renorms = ref 0 in
  let width' = ref width in
  while !width' >= min_site_width do
    renorms := !renorms + 1;
    width' := !width' / 2
  done;
  !renorms - 1

let corr_array lat =
  Array.init bt_count (fun i -> MC.correlation lat bts.(i))

let update_sum_and_sqr sum_array1 sum_array2 sqr_mat corr1 corr2 =
  for alpha=0 to bt_count - 1 do
    sum_array1.(alpha) <- sum_array1.(alpha) +. corr1.(alpha);
    sum_array2.(alpha) <- sum_array2.(alpha) +. corr2.(alpha);
    for beta=0 to bt_count - 1 do
      sqr_mat.(alpha).(beta) <- (sqr_mat.(alpha).(beta) +.
                                   corr1.(alpha) *. corr2.(beta))
    done;
  done

let update_all_staggered sum_array1s sum_array2s sqr_mats lat =
  let curr_lat = ref lat in
  let old_corr = ref (corr_array lat) in
  for i=0 to (Array.length sqr_mats) - 1 do
    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
    let curr_corr = corr_array !curr_lat in
    update_sum_and_sqr sum_array1s.(i) sum_array2s.(i) sqr_mats.(i) 
      !old_corr curr_corr;
    old_corr := curr_corr;
  done

let update_all_samesies sum_array1s sum_array2s sqr_mats lat =
  let curr_lat = ref lat in
  for i=0 to (Array.length sqr_mats) - 1 do
    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
    let corr = corr_array !curr_lat in
    update_sum_and_sqr sum_array1s.(i) sum_array2s.(i) sqr_mats.(i) corr corr;
  done

let calc_t_ab sum_array1 sum_array2 sqr_mat trials =
  let t_ab = Array.make (bt_count * bt_count) 0. in
  let n = float trials in

  for b=0 to bt_count - 1 do
    for a=0 to bt_count - 1 do
      let idx = a + b * bt_count in
      t_ab.(idx) <- sqr_mat.(a).(b) /. n -.
        sum_array1.(a) /. n *. sum_array2.(b) /. n;
    done;
  done;
  t_ab

(* This arrangement is sub-optimal. *)
let calc_t_abs lat trials =
  let renorms = count_renorms () in

  let new_array _ = Array.make bt_count 0. in
  let new_mat _ = Array.make_matrix bt_count bt_count 0. in

  let sum_array1s_same = Array.init renorms new_array in
  let sum_array2s_same = Array.init renorms new_array in
  let sum_array1s_stag = Array.init renorms new_array in
  let sum_array2s_stag = Array.init renorms new_array in
  let sqr_mats_same = Array.init renorms new_mat in
  let sqr_mats_stag = Array.init renorms new_mat in

  for trial=1 to trials do
    MC.sweep lat;
    update_all_staggered 
      sum_array1s_stag 
      sum_array2s_stag
      sqr_mats_stag lat;
    update_all_samesies
      sum_array1s_same
      sum_array2s_same
      sqr_mats_same lat;
  done;

  let create_pair i =
    (calc_t_ab sum_array1s_stag.(i)
       sum_array2s_stag.(i) 
       sqr_mats_stag.(i) 
       trials,
     calc_t_ab sum_array1s_same.(i)
       sum_array2s_same.(i) 
       sqr_mats_same.(i)
       trials) 
  in

  Array.init renorms create_pair
;;

(* This code works, don't quesiton it. *)
let extract_max_eigen (t_ab_stag, t_ab_same) = 
   let same_inv = Gsl_linalg.invert_LU ~protect:true
    (`A (t_ab_same, bt_count, bt_count)) in
  let stag_mat = `M (Gsl_matrix.of_array t_ab_stag bt_count bt_count) in
  let prod = `M (Gsl_matrix.create ~init:0. bt_count bt_count) in
  Gsl_linalg.matmult stag_mat same_inv prod;
  let (eval, _) = Gsl_eigen.nonsymmv prod in
  let max = ref 0. in
  for i=0 to bt_count - 1 do
    let {Complex.re = re; im = im} = eval.{i} in
    if re > !max then max := re
  done;
  !max
;;

let main () =
  print_parameters ();

  Random.self_init ();

  let lat = MC.init width in
  MC.set_temp lat temp;
  MC.set_bond_types lat interactions;

  for i=0 to eq_sweeps do
    MC.sweep lat
  done;

  let t_abs = calc_t_abs lat trials in
  let eigens = Array.map extract_max_eigen t_abs in
  Printf.printf "Eigenvalue exponent for dK_n+1/dK_n:\n";
  Printf.printf "(We want values close to 1.)\n";
  Printf.printf "%16s %16s\n" "Renormalizations" "y_T";
  for renorm=1 to Array.length eigens do
    Printf.printf "%16d %16g\n" renorm (log (eigens.(renorm - 1)) /. log 2.)
  done;
  Printf.printf "Rough execution estimate: %g seconds :(\n" (Sys.time ());
;;

main ()
