(**

  Regular blinking of three LEDs except when a button is pressed. 

  Generation of a VHDL wrapper module "top" for synthesis 
  on an OrangeCrab FPGA board :

   ./mixc examples/blink.ml -clk-top=clk48 -top "usr_btn:1|rgb_led0_r:1,rgb_led0_g:1,rgb_led0_b:1"

*)

let period : int<24> = 3000000 ;;

(* val step : int<24> -> int<24> *)
let step(x) =
  if x >= period then 0 else (x + 1) ;;


let on = false ;;
let off = true ;;

(* val bool : bool -> (bool * bool * bool) *)
let main(button) =

  let c = reg step last 0 in

  if button then (off,off,off)
  else if c > (period / 2) then (off,on,on) 
  else (on,off,off) ;;