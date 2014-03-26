open Signatures

(* Module name remapping. *)
module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)
module T = AsciiTable.OutTable

(* Compile-time simulation parameters *)
let interactions = [| (MC.NN1, 1.0) |]
let crit_idx = 0
let min_site_width = 2

(* Fixed and derived simulation parameters. *)
let bts = [| MC.NN1; MC.NN2; MC.NN3; MC.SQR; MC.DMD |]
let bt_count = Array.length bts
let min_site_count = min_site_width * min_site_width
let eq_sweeps = 500
let nn_crit_temp = 1. /. (log (1. +. sqrt 2.))

(* Command-line parameters *)
let _ = assert (Array.length Sys.argv > 3)
let input_output_file = Sys.argv.(1)
let input_width = int_of_string Sys.argv.(2) 
let input_trials = int_of_string Sys.argv.(3) 
let input_finder_iters = int_of_string Sys.argv.(4) 
let input_temp = if Array.length Sys.argv = 5 then nn_crit_temp 
  else float_of_string (Sys.argv.(5))

let sweep_width = 0.3
    
(* `print_parameters` prints the input parameters to the terminal in a
 * somewhat sane way. *)
let print_parameters () =
  let string_of_bt bt =
    match bt with
      MC.NN1 -> "NN1"
    | MC.NN2 -> "NN2"
    | MC.NN3 -> "NN3"
    | MC.SQR -> "SQR"
    | MC.DMD -> "DMD"
    | MC.MAG -> "MAG"
  in
  let print_bt bt = 
    print_string (string_of_bt bt) 
  in
  let print_interaction (bt, strength) = 
    Printf.printf "(%s, %g)" (string_of_bt bt) strength 
  in
  
  Printf.printf "Simulation parameters:\n";
  Printf.printf "%16s: %16g\n%16s: %16d\n%16s: %16d\n"
    "Temperature" input_temp
    "Trials" input_trials
    "Initial Width" input_width;
  Printf.printf "%16s: " "Bond Types";
  Utils.print_list print_bt (Array.to_list bts);
  Printf.printf "\n";
  Printf.printf "%16s: " "Interactions";
  Utils.print_list print_interaction (Array.to_list interactions);
  Printf.printf "\n"

(* `count_rentorms` counts the maximum number of renormalizaitons that
 * can be performed on a lattice with the given number of sites. This
 * could be done better with bit twiddling tricks, but I don't really
 * care enough to do it. *)
let count_renorms sites = 
  let renorms = ref 0 in
  let sites' = ref sites in
  while !sites' >= min_site_count do
    renorms := !renorms + 1;
    sites' := !sites' / 4
  done;
  !renorms - 1

(* `corr_array` creates an array of all lat's bond correlation
 * strengths. *)
let corr_array lat =
  Array.init bt_count (fun i -> MC.correlation lat bts.(i))

(* `update_sum_and_sqr` takes a pair of correlation arrays and then updates
 * the sum_arrays and sqr_mat by their values and products, respectively. *)
let update_sum_and_sqr sum_array1 sum_array2 sqr_mat corr1 corr2 =
  for alpha=0 to bt_count - 1 do
    sum_array1.(alpha) <- sum_array1.(alpha) +. corr1.(alpha);
    sum_array2.(alpha) <- sum_array2.(alpha) +. corr2.(alpha);
    for beta=0 to bt_count - 1 do
      sqr_mat.(alpha).(beta) <- (sqr_mat.(alpha).(beta) +.
                                   corr1.(alpha) *. corr2.(beta))
    done;
  done

(* `update_all` repeatedly renormalizes the given lattice. At each step
 * it computes `update_sum_and_sqr` with respect to the original lattice
 * and the current renormalization. I have no fucking clue why
 * sum_arrays_base is not completely irrelevent. *)
let update_all sum_arrays_base sum_arrays_curr sqr_mats lat =
  let base_corr = corr_array lat in
  let curr_lat = ref lat in
  for i=0 to Array.length sqr_mats - 1 do
    let corr = corr_array !curr_lat in
    update_sum_and_sqr sum_arrays_base.(i) sum_arrays_curr.(i) 
      sqr_mats.(i) base_corr corr;

    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
  done

(* `calc_t_ab` calculates a t_ab matrix using the given sum arrays
 * and matrices. For reference, t_ab = dS_a/dK_b  and 
 * dS_a/dK_b = <S_a S_b> - <S_a> <S_b>.*)
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

(* `calc_t_abs` *)
let calc_t_abs lat trials =
  let renorms = count_renorms (MC.site_count lat) in

  let new_array _ = Array.make bt_count 0. in
  let new_mat _ = Array.make_matrix bt_count bt_count 0. in

  let sum_arrays_base = Array.init (renorms + 1) new_array in
  let sum_arrays_curr = Array.init (renorms + 1) new_array in
  let sqr_mats = Array.init (renorms + 1) new_mat in

  for trial=1 to trials do
    MC.sweep lat;
    update_all sum_arrays_base sum_arrays_curr sqr_mats lat;
  done;

  (* WHY THE HELL DID I DO THIS *)
  (Array.init renorms
     (fun i -> calc_t_ab 
       sum_arrays_base.(i) 
       sum_arrays_curr.(i)
       sqr_mats.(i) trials),
     sum_arrays_curr)
;;

(* Attempts to iteratatively close in on the point where *)
let find_iter big_lat small_lat =
  let n = float input_trials in
  let temps = Array.make (input_finder_iters + 1) 0. in
  temps.(0) <- input_temp;

  for i=1 to input_finder_iters - 1 do
    Printf.printf " "
  done;
  print_endline "|";

  let t = ref (T.make ["    Temperature"; "   Inv. Temp."; "Delta S_nn";
                       "Delta T_nn_nn"; "Zero"]) in

  for i=1 to input_finder_iters do
    (* Printf.printf ".";
    flush_all (); *)
    
    MC.set_temp big_lat temps.(i-1);
    MC.set_temp small_lat temps.(i-1);
    
    for j=1 to eq_sweeps do
      MC.sweep big_lat;
      MC.sweep small_lat;
    done;
        
    let big_t_abs, big_sums = calc_t_abs big_lat input_trials in
    let small_t_abs, small_sums = calc_t_abs small_lat input_trials in
        
    let big_nn_sum = big_sums.(Array.length big_sums - 1).(0) /. n in
    let small_nn_sum = small_sums.(Array.length small_sums - 1).(0) /. n in  
    let big_bottom_t_ab_nn = big_t_abs.(Array.length big_t_abs - 1).(crit_idx) in
    let small_bottom_t_ab_nn = small_t_abs.(Array.length small_t_abs - 1).(crit_idx) in
    
    let sum_diff = (big_nn_sum -. small_nn_sum) in
    let t_ab_diff = (big_bottom_t_ab_nn -. small_bottom_t_ab_nn) in

    let dk = -. sum_diff /. t_ab_diff in

    temps.(i) <- 1. /. (1. /. (temps.(i - 1)) +. dk);
    Printf.printf "%.5g --(%.5g, %.5g)--> %.5g\n" temps.(i-1) sum_diff t_ab_diff temps.(i);
    flush_all ();

    t := T.add_row !t [temps.(i - 1); 1./.temps.(i - 1); sum_diff; t_ab_diff; 0.];
  done;

  T.write !t input_output_file;
  print_newline ();
  temps
;;

let find_sweep big_lat small_lat =
  let n = float input_trials in
  let temps = Array.make (input_finder_iters + 1) 0. in
  temps.(0) <- input_temp -. sweep_width /. 2.;

  for i=1 to input_finder_iters - 1 do
    Printf.printf " "
  done;
  print_endline "|";

  let t = ref (T.make ["     Temperature"; "      Delta S";
                       "     Delta T_ab"; "Zero"]) in

  for i=1 to input_finder_iters do
    Printf.printf ".";
    flush_all ();
    
    MC.set_temp big_lat temps.(i-1);
    MC.set_temp small_lat temps.(i-1);
    
    for i=1 to eq_sweeps do
      MC.sweep big_lat;
      MC.sweep small_lat;
    done;
        
    let big_t_abs, big_sums = calc_t_abs big_lat input_trials in
    let small_t_abs, small_sums = calc_t_abs small_lat input_trials in

    let big_nn_sum = big_sums.(Array.length big_sums - 1).(0) /. n in
    let small_nn_sum = small_sums.(Array.length small_sums - 1).(0) /. n in  
    let big_bottom_t_ab_nn = big_t_abs.(Array.length big_t_abs - 1).(crit_idx) in
    let small_bottom_t_ab_nn = small_t_abs.(Array.length small_t_abs - 1).(crit_idx) in
    
    let sum_diff = (big_nn_sum -. small_nn_sum) in
    let t_ab_diff = (big_bottom_t_ab_nn -. small_bottom_t_ab_nn) in

    t := T.add_row !t [temps.(i - 1); sum_diff; t_ab_diff; 0.];
    temps.(i) <- temps.(i-1) +. sweep_width /. (float input_finder_iters);
  done;
  T.write !t input_output_file;
  print_newline ();
  temps
;;

let main () =
  print_parameters ();

  Random.self_init ();

  let big_lat = MC.init input_width in
  let small_lat = MC.init (input_width / 2) in

  MC.set_bond_types small_lat interactions;
  MC.set_bond_types big_lat interactions;

  let temps = find_sweep big_lat small_lat in
  (*let temps = find_iter big_lat small_lat in*)
  for i=0 to input_finder_iters do
    Printf.printf "%7g " temps.(i);
    if i mod 10 = 9 then print_newline ();
  done;
  Printf.printf "\n"
;;

main ()
