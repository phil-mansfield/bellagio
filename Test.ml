open Signatures

module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)

module OT = AsciiTable.OutTable

let width = int_of_string Sys.argv.(1)
let trials = int_of_string Sys.argv.(2)

let t_start = 0.1
let t_end = 3.0
let t_step = 0.1

let main () =
  Random.self_init ();
  let lat = ref (MC.init width) in
  MC.set_bond_types !lat [| (MC.NN1 , 1.0) |];

  let t = ref (OT.make ["T"; "E"; "M"]) in
  let trial_t = ref t_start in
  while !trial_t < t_end do
    MC.set_temp !lat !trial_t;
    for i=0 to 100 do MC.sweep !lat; done;
    Printf.printf "trial temp: %g\n" !trial_t;
    MC.sweep !lat;
    let e_sum = ref 0. in
    let m_sum = ref 0. in
    for j=1 to trials do
      e_sum := !e_sum +. MC.energy !lat;
      m_sum := !m_sum +. MC.magnetization !lat;
    done;
    let e = !e_sum /. (float trials) in
    let m = !m_sum /. (float trials) in
    Printf.printf "Corr:   %.4g\n" ((MC.correlation !lat MC.SQR) /. (16. *. 16.));
    Printf.printf "Energy: %.4g\n" e;
    Printf.printf "Mag:    %.4g\n" m;
    t := OT.add_row !t [!trial_t; e; m];
    trial_t := !trial_t +. t_step
  done;

  OT.write !t "out.table";

  let e = MC.energy !lat /. (float (MC.site_count !lat)) in
  let m = MC.magnetization !lat /. (float (MC.site_count !lat)) in
  
  Printf.printf "Energy:   %.4g\n" e;
  Printf.printf "Mag:      %.4g\n" m;
  Printf.printf "corr NN1: %g\n" (MC.correlation !lat MC.NN1);
  Printf.printf "corr NN2: %g\n" (MC.correlation !lat MC.NN2);
  Printf.printf "corr NN3: %g\n" (MC.correlation !lat MC.NN3);
  Printf.printf "corr SQR: %g\n" (MC.correlation !lat MC.SQR);
  Printf.printf "corr DMD: %g\n" (MC.correlation !lat MC.DMD);
;;

main ()
