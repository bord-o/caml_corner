
let window_x = 1280 
let window_y = 720

let () = Graphics.open_graph @@ Printf.sprintf " %ix%i" window_x window_y


let img = Png.load "/home/bordo/caml_corner/bin/dvd.png" [];;
let g = Graphic_image.of_image img;;

let draw_dvd dvd x y = 
  Graphics.draw_image dvd x y;

type velocity = {
  vx: float;
  vy: float
}
type position = {
  x: int;
  y: int
}

type state = {
  v: velocity;
  p: position
}

let state_trans state = 
  (* draws the dvd with the current state and returns the next state *)
  Graphics.clear_graph ();

  Graphics.set_color Graphics.white;
  Graphics.draw_rect 0 0 window_x window_y;
  Graphics.fill_rect 0 0 window_x window_y;

  draw_dvd g state.p.x state.p.y;
  let bx = window_x - 225 in
  let by = window_y - 225 in

  let negx = if state.p.x > bx then {p = {x = bx; y=state.p.y}; v={vx= -.state.v.vx -.1.0; vy=state.v.vy}} else state in
  let negzx = if negx.p.x < 0 then {p = {x = 1; y=negx.p.y}; v={vx= -.negx.v.vx +. 1.0 ; vy=negx.v.vy}} else negx in

  let negy = if negzx.p.y > by then {p = {x=negzx.p.x; y=by}; v={vx= negzx.v.vx; vy= -.negzx.v.vy -. 1.0}} else negzx in
  let negzy = if negy.p.y < 0 then {p = {x=negy.p.x; y=1}; v={vx= negy.v.vx; vy= -.negy.v.vy +. 1.0 }} else negy in

  let new_state = {negzy with p={x=negzy.p.x + int_of_float negzy.v.vx; y=negzy.p.y + int_of_float negzy.v.vy}}
  in new_state


let rec event_loop initial_state = 
  Unix.sleepf 0.02;
  initial_state |> state_trans |> event_loop

let () =
  let default_state = {v= {vx=5.0; vy=1.0}; p= {x=1; y=1}} in
  event_loop default_state;
