open Array

module ArrayHistogram =
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

  let bin_value h bin_index = 0.0
  let bin_index h bin_value = 0

  let add h bin_value = ()
  let bins h = []
end
