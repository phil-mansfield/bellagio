open Signatures

module OutTable : OUT_TABLE =
struct
  type addition_type = Columns | Rows
  type out_table = { info : float list list;
                     names : string list;
                     at : addition_type option;
                     header : string }

  (* This ensures at least three significant digits. *)
  let min_width = 10
  let sed_str =
    "(use \"$ sed -e '/^#/d' -e 's/#.*$//' <filename>\" to remove comments)"

  let new_table info names at header =
    { info=info; names=names; at=at; header=header }

  let make names = 
    let expand_to_min_width name =
      let len = String.length name in
      if len >= min_width then name
      else String.concat "" [String.make (min_width - len) ' '; name] in
    new_table [] (List.map expand_to_min_width names) None "#"
      
  let set_header t str = 
    let rows = Str.split (Str.regexp "\n") str in
    new_table t.info t.names t.at (String.concat "\n# " rows)
      
  let rec add_row t row =
    if (List.length row) <> (List.length t.names) then
      failwith (Printf.sprintf "List of length %d doesn't fit with %d cols."
                  (List.length row) (List.length t.names))
    else match t.at with
      None -> add_row (new_table t.info t.names (Some Rows) t.header) row
    | Some Rows -> new_table (row :: t.info) t.names t.at t.header
    | _ -> failwith "Cannot add row to column-aligned ascii_table"

  (* Whatever, I don't feel like adding error handling right now. 
     Use this at your own risk, or just don't me an idiot. *)
  let rec add_column t col =
    match t.at with
      None -> add_column (new_table t.info t.names (Some Columns) t.header) col
    | Some Columns -> new_table (col :: t.info) t.names t.at t.header
    | _ -> failwith "Cannot add row to column-aligned ascii_table"

  let write t file_name =
    let col_widths = List.map String.length t.names in
    let data_rows = match t.at with
        None -> failwith "Attempting to write an empty ascii_table."
      | Some Columns -> Utils.zip_grid t.info
      | Some Rows -> List.rev t.info in

    let chan = open_out file_name in

    Printf.fprintf chan "# %s\n" sed_str;
    Printf.fprintf chan "%s\n# %s\n" t.header (String.concat " " t.names);

    let write_row row = 
      Printf.fprintf chan "  ";
      (* We need to reserve 7 spaces: one for sign, 'e', and exponent sign, 
       * and 3 more for exponent magnitude. *)
      List.iter2 (fun x n -> Printf.fprintf chan "%*.*g " n (n - 7) x)
        row (Utils.sub_list col_widths 0 (List.length row));
      Printf.fprintf chan "\n" in
    List.iter write_row data_rows;

    close_out chan
      
end
