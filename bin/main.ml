open Bogue
module W = Widget
module L = Layout

let box_speed = 10

let move_box box_widget (dx, dy) =
  let layout = L.containing_widget box_widget |> Option.get in
  let x = L.getx layout + dx in
  let y = L.gety layout + dy in
  L.setx layout x;
  L.sety layout y;
  Update.push box_widget

let handle_key box_widget event =
  match Trigger.event_kind event with
  | `Key_down -> (
      let keycode = Tsdl.Sdl.Event.(get event keyboard_keycode) in
      match keycode with
      | 119 -> move_box box_widget (0, -box_speed) (* 'w' key for up *)
      | 115 -> move_box box_widget (0, box_speed) (* 's' key for down *)
      | 97 -> move_box box_widget (-box_speed, 0) (* 'a' key for left *)
      | 100 -> move_box box_widget (box_speed, 0) (* 'd' key for right *)
      | _ -> ())
  | _ -> ()

let main () =
  let box_style =
    Style.create ~background:(Style.color_bg (Draw.opaque Draw.blue)) ()
  in
  let box_widget = W.box ~w:50 ~h:50 ~style:box_style () in

  let box_layout = L.resident ~x:0 ~y:0 box_widget in
  let layout = L.tower ~scale_content:false [ box_layout ] in

  L.claim_keyboard_focus box_layout;

  let connection =
    W.connect box_widget box_widget
      (fun w _ event -> handle_key w event)
      [ Trigger.key_down ]
  in
  W.add_connection box_widget connection;

  let board = Bogue.of_layout layout in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
