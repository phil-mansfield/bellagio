open Signatures
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

let interaction_bond_types = [| (MC.SQR, 1.0) |]

let bt_count = List.length interactions
let min_site_count = 16

let width = int_of_string Sys.argv.(1)
let trials = int_of_string Sys.argv.(2)
(*let temp = 2. /. (log (1. +. sqrt 2.)) *)
let temp = 1.5

let () = Printf.printf "Simulation parameters:\n"
let () = Printf.printf "%16s: %16g\n%16s: %16d\n%16s: %16d\n%16s: "
  "Temperature" temp
  "Trials" trials
  "Initial Width" width
  "Bond Types"
let () = Utils.print_list print_string (Array.to_list names)
let () = Printf.printf "\n\n"
let () = Printf.printf "%16s: [(NN1, 1.0); (SQR, 0.001)]" "Interactions"

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

(* TODO: make this more elegant. *)
let update_all_staggered sum_array1s sum_array2s sqr_mats lat =
  let curr_lat, prev_lat = ref lat, ref lat in
  for i=0 to (Array.length sqr_mats) - 1 do
    prev_lat := !curr_lat;
    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
    update_sum_and_sqr sum_array1s.(i) sum_array2s.(i) 
      sqr_mats.(i) !prev_lat !curr_lat;
  done

(* TODO: make this more elegant. *)
let update_all_samesies sum_array1s sum_array2s sqr_mats lat =
  let curr_lat = ref lat in
  for i=0 to (Array.length sqr_mats) - 1 do
    curr_lat := MC.renormalize !curr_lat MC.Random2By2 None;
    update_sum_and_sqr sum_array1s.(i) sum_array2s.(i) 
      sqr_mats.(i) !curr_lat !curr_lat;
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
  let renorms = ref 0 in
  let div = ref 1 in
  while (MC.site_count lat) / !div > min_site_count do
    renorms := !renorms + 1;
    div := !div * 4;
  done;

  let new_array _ = Array.make bt_count 0. in
  let new_mat _ = Array.make_matrix bt_count bt_count 0. in
  let sum_array1s_same = Array.init !renorms new_array in
  let sum_array2s_same = Array.init !renorms new_array in
  let sum_array1s_stag = Array.init !renorms new_array in
  let sum_array2s_stag = Array.init !renorms new_array in
  let sqr_mats_same = Array.init !renorms new_mat in
  let sqr_mats_stag = Array.init !renorms new_mat in

  for trial=1 to trials do
    MC.sweep lat;
    update_all_staggered sum_array1s_stag sum_array2s_stag sqr_mats_stag lat;
    update_all_samesies sum_array1s_same sum_array2s_same sqr_mats_same lat;
  done;

  let create_pair i =
    (calc_t_ab sum_array1s_stag.(i) sum_array2s_stag.(i) sqr_mats_stag.(i) trials,
     calc_t_ab sum_array1s_same.(i) sum_array2s_same.(i) sqr_mats_same.(i) trials)
     
  in

  Array.init !renorms create_pair
;;

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
  Random.self_init ();

  let lat = MC.init width in
  MC.set_temp lat temp;
  MC.set_bond_types lat interaction_bond_types;

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
;;

main ()
