*open Signatures
open Gsl_linalg

module OutTable = AsciiTable.OutTable
module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)

let eq_sweeps = 100
let interactions = [(MC.NN1, "s_NN3");
                    (MC.NN2, "s_NN2");
                    (MC.NN3, "s_NN3");
                    (MC.SQR, "s_SQR");
                    (MC.DMD, "s_DMD") ]
let bts_list, names_list = List.split interactions
let bts, names = Array.of_list bts_list, Array.of_list names_list
let bt_count = Array.length bts

let interaction_index = 0
let interaction_bond_types = [| (bts.(interaction_index), 1.0) |]

let min_site_width = 4
let min_site_count = min_site_width * min_site_width

let width = int_of_string Sys.argv.(1)
let trials = int_of_string Sys.argv.(2)
let temp_guess = float_of_string Sys.argv.(3)
let iters = int_of_string Sys.argv.(4)

let () = Printf.printf "Simulation parameters:\n"
let () = Printf.printf "%16s: %16g\n%16s: %16d\n%16s: %16d\n%16s: "
  "Temp Guess" temp_guess
  "Trials" trials
  "Initial Width" width
  "Bond Types"
let () = Utils.print_list print_string (Array.to_list names)
let () = Printf.printf "\n\n"
let () = Printf.printf "%16s: [(SQR, 1.0)]\n" "Interactions"

let update_sum_and_sqr sum_array1 sum_array2 sqr_mat lat1 lat2 =
  let corr1, corr2 = Array.make bt_count 0.0, Array.make bt_count 0.0 in
  (* This is sub-optimal. We should save the correlation functions from
     the previous lattice. *)
  for i=0 to bt_count - 1 do
    corr1.(i) <- MC.correlation lat1 bts.(i);
    corr2.(i) <- MC.correlation lat2 bts.(i);
  done;

  for alpha=0 to bt_count - 1 do
    sum_array1.(alpha) <- sum_array1.(alpha) +. corr1.(alpha);
    sum_array2.(alpha) <- sum_array2.(alpha) +. corr2.(alpha);
    for beta=0 to bt_count - 1 do
      sqr_mat.(alpha).(beta) <- (sqr_mat.(alpha).(beta) +.
                                   corr1.(alpha) *. corr2.(beta))
    done;
  done

let update_all_base sum_array1s sum_array2s sqr_mats lat =
  let curr_lat = ref lat in
  for i=0 to (Array.length sqr_mats) - 1 do
    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
    update_sum_and_sqr sum_array1s.(i) sum_array2s.(i) 
      sqr_mats.(i) !curr_lat lat;
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
let calc_t_abs_and_corrs lat trials =
  let renorms = ref 0 in
  let div = ref 1 in
  while (MC.site_count lat) / !div > min_site_count do
    renorms := !renorms + 1;
    div := !div * 4;
  done;
  renorms := !renorms - 1;

  let new_array _ = Array.make bt_count 0. in
  let new_mat _ = Array.make_matrix bt_count bt_count 0. in
  let sum_array1s = Array.init !renorms new_array in
  let sum_array2s = Array.init !renorms new_array in
  let sqr_mats = Array.init !renorms new_mat in

  for trial=1 to trials do
    MC.sweep lat;
    update_all_base sum_array1s sum_array2s sqr_mats lat;
  done;

  let create_t_ab i =
    calc_t_ab sum_array1s.(i) sum_array2s.(i) sqr_mats.(i) trials
  in

  (Array.init !renorms create_t_ab, sum_array1s)
;;

let min_base_deriv_and_corr lat trials = 
  let t_abs, coors = calc_t_abs_and_corrs lat trials in
  let last_t_ab = t_abs.(Array.length t_abs - 1) in
  (last_t_ab.(interaction_index + interaction_index * bt_count),
   coors.(Array.length t_abs - 1).(interaction_index) /. (float trials))

let main () =
  Random.self_init ();

  let big_lat = MC.init width in
  let small_lat = MC.init (width / 2) in

  let temp = ref temp_guess in

  MC.set_bond_types big_lat interaction_bond_types;
  MC.set_bond_types small_lat interaction_bond_types;

  for i=1 to iters do
    Printf.printf "Iteration: %d, guess temperature: %g\n" i !temp;

    MC.set_temp big_lat !temp;
    MC.set_temp small_lat !temp;

    for i=0 to eq_sweeps do
      MC.sweep big_lat;
      MC.sweep small_lat;
    done;

    let big_deriv, big_corr = min_base_deriv_and_corr big_lat trials in
    let small_deriv, small_corr = min_base_deriv_and_corr small_lat trials in

    Printf.printf "bd: %g bc: %g\n" big_deriv big_corr;
    Printf.printf "sd: %g sc: %g\n" small_deriv small_corr;
   
    let dk = (big_corr -. small_corr) /. (big_deriv -. small_deriv) in
    Printf.printf "k0 = %g dk = %g\n" (1. /. !temp) dk;
    temp := 1. /. (1. /. !temp +. dk);
  done;
;;

main ()
