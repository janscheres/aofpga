open Hardcaml

module Sim = Cyclesim.With_interface(Dial.I)(Dial.O)

let () =
  let sim = Sim.create (Dial.create (Scope.create ())) in
  (* Note: We removed the waveform viewer here to make it run fast! *)
  
  let i = {
    Dial.I.
    clock     = Cyclesim.in_port sim "clock";
    clear     = Cyclesim.in_port sim "clear";
    start     = Cyclesim.in_port sim "start";
    direction = Cyclesim.in_port sim "direction";
    amount    = Cyclesim.in_port sim "amount";
  } in
  let o = {
    Dial.O.
    current   = Cyclesim.out_port sim "current";
    password  = Cyclesim.out_port sim "password";
    ready     = Cyclesim.out_port sim "ready";
  } in

  let move dir_str amt =
    (* Convert "L"/"R" string to 0 or 1 *)
    let dir_bit = if String.equal dir_str "R" then 1 else 0 in
    
    i.start     := Bits.one 1;
    i.direction := Bits.of_int ~width:1 dir_bit;
    i.amount    := Bits.of_int ~width:16 amt;
    
    Cyclesim.cycle sim;
    i.start := Bits.zero 1;

    while Bits.to_int !(o.ready) = 0 do
      Cyclesim.cycle sim;
    done;
    Cyclesim.cycle sim
  in

  (* --- MAIN LOGIC --- *)
  
  (* 1. Reset *)
  i.clear := Bits.one 1;
  Cyclesim.cycle sim;
  i.clear := Bits.zero 1;

  (* 2. Open the file and loop through lines *)
  let filename = "input.txt" in
  let chan = In_channel.open_text filename in
  
  try
    while true do
      let line = In_channel.input_line chan in
      match line with
      | None -> raise End_of_file
      | Some l -> 
          (* Parse "L68" -> "L", 68 *)
          let l = String.trim l in
          if String.length l > 0 then (
            let dir = String.sub l 0 1 in
            let amt = int_of_string (String.sub l 1 (String.length l - 1)) in
            move dir amt
          )
    done
  with End_of_file ->
    In_channel.close chan;
    
    (* 3. Print the Answer! *)
    Printf.printf "--- SOLVED ---\n";
    Printf.printf "Final Password: %d\n" (Bits.to_int !(o.password))
