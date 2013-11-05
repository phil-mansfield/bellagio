open Signatures

module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)

let width = int_of_string Sys.argv.(1)
let trials = int_of_string Sys.argv.(2)
let temp = float_of_string Sys.argv.(3)

let main () =
  Random.self_init ();
  let lat = ref (MC.init width) in
  MC.set_temp !lat temp;
  MC.set_bond_types !lat [| (MC.SQR, 1.0) |];

  for i = 0 to trials do
    MC.print !lat;
    print_newline ();
    MC.sweep !lat;
  done;
  MC.print !lat;

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
