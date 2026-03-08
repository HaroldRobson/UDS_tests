open Unix

[%%cstruct
type ob = {
  bids: uint32_t [@len 20];
  asks: uint32_t [@len 20];
} [@@little_endian]]
let[@inline] get_bid bids i =
  Int32.float_of_bits
    (Cstruct.LE.get_uint32 bids (i lsl 2))

let[@inline] set_bid bids i v =
  Cstruct.LE.set_uint32 bids (i lsl 2)
    (Int32.bits_of_float v)

let[@inline] get_ask asks i =
  Int32.float_of_bits
    (Cstruct.LE.get_uint32 asks (i lsl 2))

let[@inline] set_ask asks i v =
  Cstruct.LE.set_uint32 asks (i lsl 2)
    (Int32.bits_of_float v)
let path_ocaml = "/dev/shm/ocaml_meow.sock"



let calculate_order buf = 
  let bids = get_ob_bids buf in
  let asks = get_ob_asks buf in
  (*let _ = print_endline (string_of_float (get_bid  bids 12)) in*)
  let rec calculate_order_helper size n = 
    match n with
    | (-1) -> size
    | _ ->    
    match  get_bid bids n > get_ask asks n with 
    | true -> calculate_order_helper (size +. (get_bid bids n ) -. (get_ask asks n )) (n-1)
    | false -> calculate_order_helper size (n-1) in
  calculate_order_helper 0.0 19

let () =
  let fd = socket PF_UNIX SOCK_DGRAM 0 in
  (try unlink path_ocaml with _ -> ());
  bind fd (ADDR_UNIX path_ocaml);


  Printf.printf "OCaml ready on %s\n%!" path_ocaml;
let obuf = Bytes.create sizeof_ob in
let (_len, _addr) = recvfrom fd obuf 0 sizeof_ob [] in
let buf = Cstruct.of_bytes obuf in
let _bids = get_ob_bids buf in
let _asks = get_ob_asks buf in
  while true do
    let (len, addr) =
      recvfrom fd (Cstruct.to_bytes buf) 0 sizeof_ob []
    in

    let _order = calculate_order buf in
    if len = sizeof_ob then begin
      ignore (sendto fd (Cstruct.to_bytes buf) 0 sizeof_ob [] addr)
    end
  done

