open Hardcaml (*equivalent to imports*)
open Signal

module I = struct (*make a new struct for the input*)
    type 'a t = { (*make a generic variable t*)
        clock : 'a; (*make generic variable clock*)
        clear : 'a;
        start : 'a; (*when this is 1, we are ready to move to next value*)
        direction : 'a; (* true means right, false is left*)
        amount : 'a[@bits 16]; (*number to count down from*)
    }
    [@@deriving sexp_of, hardcaml](*macro magic*)
end

module O = struct
    type 'a t = {
        current : 'a[@bits 8]; (*generic of length 8 bits*)
        password : 'a[@bits 16];
        part2 : 'a[@bits 16];
        ready : 'a;
    }
    [@@deriving sexp_of, hardcaml]
end

(* Make new func create*)
(*scope part is just for debugging it exposes variable names*)
(*function takes in i which is the input, of type signal.t which is a hardware wire and the input is I.t*)
let create (_scope : Scope.t) (i : Signal.t I.t) =
    (*make new register using the input clock as clock and clear if input clear is true ig?*)
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    let left = wire 16 in
    let is_moving = left>:.0 in (*is the timer running*)

    let next =
        (*mux2 confition iftrue iffalse*)
        (* did "user" press start, if so the left should be amount*)
        mux2 i.start i.amount

        (*if start is false, still counting down.*)
        (mux2 (left >:. 0)
            (*if we are greater than 0 decrease else set to 0*)
            (left -:. 1)
            (zero 16)
        )
    in

    left <== reg spec next;

    let current_pos = wire 8 in

    let turn_right = mux2 (current_pos==:. 99) (zero 8)
        (current_pos +:. 1) in

    let turn_left = mux2 (current_pos ==:. 0) (of_int ~width:8 99)
        (current_pos -:. 1) in

    let next_pos = mux2 is_moving
        (*if we are moving, *)
        (mux2 i.direction turn_right turn_left)
        (*if we arent moving, stay the same*)
        current_pos in
    
    (*specific register to reset to 50*)
    let spec_pos = Reg_spec.override spec ~clear_to:(of_int ~width:8 50) in

    (* 2. Use that spec for this register *)
    current_pos <== reg spec_pos next_pos;

    let score = wire 16 in
    let part2score = wire 16 in

    let just_zero_now = (left ==:. 1) &: (next_pos==:. 0) in

    (* part 2*)
    let part2_condition = is_moving &: (next_pos==:. 0) in

    let next_score = mux2 just_zero_now (score+:.1) score in
    let next_score_part2 = mux2 part2_condition (part2score+:.1) part2score in

    score <== reg spec next_score;
    part2score <== reg spec next_score_part2;

    {O.
        current = current_pos; (*wire carrying 8 bits 0-255, rn is 50*)
        password = score; (*16 bit wire connected to ground*)
        part2 = part2score;
        ready = ~: is_moving; (*only ready if we have 0 left to do*)
    }
