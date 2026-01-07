open Hardcaml

module Sim = Cyclesim.With_interface(Dial.I)(Dial.O)

let () =
  let sim = Sim.create (Dial.create (Scope.create ())) in
  
  let i = {
    Dial.I.
    clock = Cyclesim.in_port sim "clock";
    clear = Cyclesim.in_port sim "clear";
    start = Cyclesim.in_port sim "start";
    direction = Cyclesim.in_port sim "direction";
    amount = Cyclesim.in_port sim "amount";
  } in
  let o = {
    Dial.O.
    current = Cyclesim.out_port sim "current";
    password = Cyclesim.out_port sim "password";
    part2 = Cyclesim.out_port sim "part2";
    ready = Cyclesim.out_port sim "ready";
  } in

  let move dir_str amt = (*function to move to the dir indicated in dir_str by amount amt*)
    (* turn L/R to 0 or 1 *)
    let dir_bit = if String.equal dir_str "R" then 1 else 0 in
    
    i.start := Bits.one 1;
    i.direction := Bits.of_int ~width:1 dir_bit;
    i.amount := Bits.of_int ~width:16 amt;
    
    Cyclesim.cycle sim;
    i.start := Bits.zero 1;

    while Bits.to_int !(o.ready) = 0 do
      Cyclesim.cycle sim;
    done;
    Cyclesim.cycle sim
  in

  (* reset *)
  i.clear := Bits.one 1;
  Cyclesim.cycle sim;
  i.clear := Bits.zero 1;

  (* loop thru lines in file *)
  let filename = "input.txt" in
  let chan = In_channel.open_text filename in
  
  try
    while true do
      let line = In_channel.input_line chan in
      match line with
      | None -> raise End_of_file
      | Some l -> 
          (* seperate line into direction and amount *)
          let l = String.trim l in
          if String.length l > 0 then (
            let dir = String.sub l 0 1 in (* from 0 index take one *)
            let amt = int_of_string (String.sub l 1 (String.length l - 1)) in (* take everything from index 1 on*)
            move dir amt (* use previously defined function to update *)
          )
    done
  with End_of_file ->
    In_channel.close chan;
    
    (* 3. Print the Answer! *)
    Printf.printf "--- SOLVED ---\n";
    Printf.printf "Part 1: %d\n" (Bits.to_int !(o.password));
    Printf.printf "Part 2: %d\n" (Bits.to_int !(o.part2))
