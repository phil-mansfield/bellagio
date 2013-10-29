open Signatures

module OutTable = AsciiTable.OutTable
module G = Grid.FlatGrid
module Hist = Histogram.ArrayHistogram
module MC = MCarlo.MakeSwendsenWang(Hist)

let width = int_of_string Sys.argv.(1)
let trials = int_of_string Sys.argv.(2)
let temp = float_of_string Sys.argv.(3)
let gamma = float_of_string Sys.argv.(4)

let main () =
  Random.self_init ();
  let lat = ref (MC.init width) in
  MC.set_temp !lat temp;
  let t = ref (OutTable.make ["Normalizations";
                              "Correlation Function";
                              "Initial Correlation"]) in

  for i = 0 to 1_000 do
    MC.sweep !lat;
  done;

  let e_0 = MC.energy !lat /. (float (MC.site_count !lat)) in
  let m_0 = MC.magnetization !lat /. (float (MC.site_count !lat)) in
  
  MC.print !lat;
  t := OutTable.add_row !t [0.0; e_0; e_0];

  let e, m = ref e_0, ref m_0 in
  let e_sqr, m_sqr = ref (e_0 *. e_0), ref (m_0 *. m_0) in

  for i = 1 to trials - 1 do
    lat := MC.renormalize !lat MC.InPlace3By3 (Some gamma);
    t := OutTable.add_row !t 
      [float_of_int i;
       (MC.energy !lat) /. (float_of_int (MC.site_count !lat));
       e_0];
    let e' = MC.energy !lat /. (float (MC.site_count !lat)) in
    let m' = MC.magnetization !lat /. (float (MC.site_count !lat)) in
    e := !e +. e';
    e_sqr := !e_sqr +. e' *. e';
    m := !m +. m';
    m_sqr := !m_sqr +. m' *. m';
  done;
  
  print_newline ();  
  MC.print !lat;

  let e_var = !e_sqr /. (float trials) -. (!e *. !e) /. (float (trials * trials)) in
  let m_var = !m_sqr /. (float trials) -. (!m *. !m) /. (float (trials * trials)) in

  Printf.printf "avg energy diff: %.3g\n" (e_0 -. !e /. (float trials));
  Printf.printf "avg mag diff:    %.3g\n" (m_0 -. !m /. (float trials));
  Printf.printf "energy var:      %.3g\n" e_var;
  Printf.printf "mag var:         %.3g\n" m_var;

  OutTable.write !t "correlation.table";
;;

main ()
