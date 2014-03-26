open Signatures

module G = Grid.FlatGrid
module MC = MCarlo.MakeSwendsenWang(Histogram.ArrayHistogram)

(* Compile-time constants. *)

let TOP_LEVEL_INTERACTIONS = [| (MC.NN1, 1.0) |]
let EQ_SWEEPS = 1_000
let MIN_GRID_WIDTH_BASE = 1 (* MIN_GRID_WIDTH = 2 ** MIN_GRID_WIDTH_BASE *)

(* Things you should probably not change. *)
let RENORMALIZATION_INTERACTIONS = [| MC.NN1; MC.NN2; MC.NN3; MC.SQR; MC.DMD |]
let MIN_GRID_WIDTH = 2 ** MIN_GRID_WIDTH_BASE
let TOP_LEVEL_RENORMALIZATIONS = 1
let COLUMN_WIDTH = 

(* Named tuple types *)

type sweep_info = { corr : float array; 
                    cross_corr: float array array }

type trial_info = { corr : float array;
                    corr_derivative : float array array}

type command_line_info = { out_file : string;
                           temperature : float;
                           width_base : float;
                           sweeps_per_trial : int;
                           trials : int }
(* helper functions *)

let print_parameters (cli : command_line_info) : unit = ()

let perform_sweep (lat : ref MC.lattice) (si : ref sweep_info) : unit = ()

let perform_trial (lat_width_base : int) (sweeps : int) : trial_info = ()

(* Simulation main-loops. *)

let sweep_find (start_temp : float) (end_temp: float) (steps : int)
    (out_file : string) (width_base : int) (sweeps_per_trial : int) : float =
  0.0

let iter_find (start_temp : float) (steps : int) (out_file : string)
    (width_base : int) (sweeps_per_trial : int) : float =
  0.0

(* main *)

let main () = ()
