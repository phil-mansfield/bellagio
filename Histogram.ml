open Array
open Signatures

module ArrayHistogram : HISTOGRAM =
struct
  type histogram = { bins : int array;
                     start_bin : float;
                     end_bin : float;
                     bin_width : float }
  
  let init start_bin bin_width bin_count =
    { bins = Array.make bin_count 0;
      start_bin = start_bin;
      end_bin = start_bin +. (float bin_count) *. bin_width;
      bin_width = bin_width }

  let init_bounded start_bin end_bin bin_count = 
    { bins = Array.make bin_count 0;
      start_bin = start_bin;
      end_bin = end_bin;
      bin_width = (end_bin -. start_bin) /. (float bin_count) }

  let bin_value h bin_index =
    h.start_bin +. (float bin_index) *. h.bin_width
  let bin_index h bin_value = 
    truncate ((bin_value -. h.start_bin) /. h.bin_width)

  (* TODO: this may end up crashing due to floating point craziness? *)
  let add h bin_value = 
    let incr i = h.bins.(i) <- h.bins.(i) + 1 in
    if bin_value = h.end_bin then incr ((Array.length h.bins) - 1)
    else incr (bin_index h bin_value)

  let bins h = Array.to_list h.bins
end
