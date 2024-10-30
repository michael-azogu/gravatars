open OcamlCanvas.V1

module IntSet = Set.Make (Int)

(** computes l combination r with f *)
let combine ~f l r =
  let open IntSet in
  fold
    (fun a acc -> fold (fun b acc' -> add (f (a, b)) acc') r acc)
    l
    empty

let bit_count n =
  let rec aux acc n =
    if n = 0 then
      acc
    else
      aux (acc + Bool.to_int (n land 1 = 1)) (n lsr 1)
  in
    aux 0 n

let filename = "./avatars"

let pixels, x, y = (50, 5, 5)
let w, h = Int.(to_float (x * pixels), to_float (y * pixels))

let used_width = 1
let used_msb = used_width
let color_width = bit_count 0xFFFFFF
let color_msb = color_width + used_msb
let center_width = y
let center_msb = center_width + color_msb
let mirror_width = y * ((x - 1) / 2)
let mirror_msb = mirror_width + center_msb

let byte = 8
let width = mirror_msb
let parts = (width + byte - 1) / byte

let generate configurations =
  let file_out = open_out_bin filename in
  let write_config config =
    for i = parts downto 1 do
      let byte = (config lsr ((i - 1) * 8)) land 0xFF in
        output_byte file_out byte
    done
  in
    configurations |> IntSet.iter write_config ;
    close_out file_out

let use_random_config () =
  let file_in = open_in_bin filename in
  let n_chunks = in_channel_length file_in / parts in
  let used =
    List.init n_chunks (fun i ->
      let fifth = (i * parts) + parts - 1 in
        seek_in file_in fifth ;
        let byte = input_byte file_in in
        let is_used = byte land 1 <> 0 in
          (i * parts, is_used)
    )
  in
  let unused =
    used
    |> List.filter_map (fun (i, is_used) ->
         if not is_used then
           Some i
         else
           None
       )
  in

  let bytes_to_int b =
    let len = Bytes.length b in
    let rec aux int i =
      if i >= len then
        int
      else (
        let byte = int_of_char (Bytes.get b i) in
          aux ((int lsl 8) + byte) (i + 1)
      )
    in
      aux 0 0
  in
  let bool_list w n =
    List.init w (fun i ->
      let mask = 1 lsl i in
        n land mask <> 0
    )
  in

  let parse bytes =
    let packed = bytes_to_int bytes in
    let extract a b =
      let mask = (1 lsl (b - a + 1)) - 1 in
        (packed lsr a) land mask
    in
    let used = 1 = extract 0 used_width in
    let color =
      String.uppercase_ascii
      @@ Printf.sprintf "#00%06x"
      @@ extract used_msb color_width
    in
    let center =
      bool_list center_width @@ extract color_msb center_width
    in
    let mirror =
      bool_list mirror_width @@ extract center_msb mirror_width
    in
      (mirror, center, color, used)
  in
  let random_chunk_idx =
    List.nth unused (Random.int (List.length unused))
  in
  let bytes = Bytes.create parts in
    seek_in file_in random_chunk_idx ;
    really_input file_in bytes 0 parts ;
    close_in file_in ;
    parse bytes

let draw_config (mirror, center, color, _) ~path =
  let open Canvas in
  let bg_color = "#00CCDDFF" in

  let radius =
    1.1 *. Float.sqrt (((w /. 2.) ** 2.) +. ((h /. 2.) ** 2.))
  in
  let diameter = 2.0 *. radius in
  let canvas =
    create_offscreen ~size:Float.(to_int diameter, to_int diameter) ()
  in

  set_fill_color canvas (Color.of_string bg_color) ;
  arc
    canvas
    ~radius
    ~center:(radius, radius)
    ~theta1:0.0
    ~theta2:(2.0 *. Float.pi)
    ~ccw:false ;
  fill canvas ~nonzero:false ;
  clear_path canvas ;

  translate canvas (radius -. (w /. 2.), radius -. (h /. 2.)) ;

  set_line_width canvas 0.3 ;
  set_stroke_color canvas Color.black ;

  let fill_column x flags =
    for y' = 0 to y - 1 do
      if List.nth flags y' then (
        set_fill_color canvas (Color.of_string color) ;
        Int.(
          rect
            canvas
            ~pos:(to_float (x * pixels), to_float (y' * pixels))
            ~size:(to_float pixels, to_float pixels)
        ) ;
        fill canvas ~nonzero:false ;
        stroke canvas ;
        clear_path canvas
      )
    done
  in

  fill_column (x / 2) center ;
  for i = 0 to (x / 2) - 1 do
    Array.(
      let flags = sub (of_list mirror) (i * y) y |> to_list in
        fill_column i flags ;
        fill_column (x - i - 1) flags
    )
  done ;

  export_png canvas path

let () =
  let used = false |> Bool.to_int |> IntSet.singleton in
  let colors =
    IntSet.of_list
      [
        0x0055CC;
        0x866DFF;
        0x38E44F;
        0x00DDBB;
        0xF066AA;
        0xA66060;
        0xFF8000;
        0xCCCC00;
      ]
  in

  let within low high =
    let open Base.Int in
    List.filter (fun n ->
      (bit_count n) |> between ~low ~high
    )
  in
  let centers =
    IntSet.of_list
    @@ (List.init (1 lsl center_width) Fun.id |> within 1 5)
  in
  let mirrors =
    IntSet.of_list
    @@ (List.init (1 lsl mirror_width) Fun.id |> within 2 8)
  in

  let shift_by amount (a, b) = (a lsl amount) lor b in

  let configurations =
    used
    |> combine colors ~f:(shift_by used_msb)
    |> combine centers ~f:(shift_by color_msb)
    |> combine mirrors ~f:(shift_by center_msb)
  in

  generate configurations ;

  configurations
  |> IntSet.cardinal
  |> Printf.printf "%d combinations\n" ;

  let config_string (mirror, center, color, _) =
    let bool_string bl =
      String.concat
        ""
        (List.map (fun b -> Bool.to_int b |> string_of_int) bl)
    in
    let bools_as_string =
      bool_string mirror ^ "-" ^ bool_string center
    in
      Printf.sprintf "%s-%s" bools_as_string color
  in

  Backend.init () ;
  Random.init 0xFACED ;

  for _ = 1 to 30 do
    let config = use_random_config () in
      draw_config
        config
        ~path:("./generated/" ^ config_string config ^ ".png")
  done