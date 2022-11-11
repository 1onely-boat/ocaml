open OUnit2
open Uno_game
module Str_Map = Core.Map.Make (Core.String)

let test_gen_id _ =
  let num_id = 10 in
  let ids = Uno_game.Id_gen.gen_id_n ~pwd_length:10 num_id in
  assert_equal num_id
    (let s = Core.Hash_set.create (module Core.String) in
     ignore (ids |> Core.List.map ~f:(fun id -> Core.Hash_set.add s id));
     Core.Hash_set.length s);
  let id_len = 999 in
    assert_equal id_len (String.length @@ Uno_game.Id_gen.gen_id ~length:id_len ())
;;

let number_of_cards_in_player_hands (s : Uno_game.t) : int =
  Core.List.fold s.players ~init:0 ~f:(fun num id ->
      num + List.length (Core.Map.find_exn s.player_hands id))

let cards_in_player_hands (s : Uno_game.t) : Uno_game.card list =
  s.players
  |> Core.List.fold ~init:[] ~f:(fun res id ->
         Uno_game.player_hand ~id s :: res)
  |> List.concat

let all_cards (s : Uno_game.t) : Uno_game.card list =
  let player_hands = cards_in_player_hands s in
  let draw_pile = s.draw_pile in
  let discard_pile =
    Core.List.take s.discard_pile
    @@ Uno_game.game_rules.total_number_of_cards
       - (List.length player_hands + List.length draw_pile)
  in
  List.concat [ player_hands; draw_pile; discard_pile ]

let count_card (s : Uno_game.t) =
  let cards = all_cards s in
  let card_map =
    Core.List.fold cards ~init:Str_Map.empty ~f:(fun m c ->
        Str_Map.change m (Uno_game.show_card c) ~f:(fun count ->
            match count with Some c -> Some (c + 1) | None -> Some 1))
  in
  card_map

let card_invariant_check (s : Uno_game.t) =
  let cards = count_card s in
  let number_of_card (c : Uno_game.card) =
    match Str_Map.find cards (Uno_game.show_card c) with
      | Some(c) -> c
      | None -> 0
  in
  let check_number (c : Uno_game.card) (count : int) =
    assert_equal count (number_of_card c) in
  let check_wild () =
    let wild_count = ref 0 in
    let draw4_count = ref 0 in
      Uno_game.iter_color ~f:(fun c -> wild_count := !wild_count + number_of_card (Wild(c)));
      Uno_game.iter_color ~f:(fun c -> draw4_count := !wild_count + number_of_card (Draw4(c)));
      assert_equal 4 !wild_count;
      assert_equal 4 !draw4_count;

  in
  Uno_game.iter_color ~f:(fun c -> Uno_game.(check_number (Number (c, 0)) 1));
  ignore
  @@ Core.List.init 9 ~f:(fun i ->
         Uno_game.(
           iter_color ~f:(fun c ->
               Uno_game.(check_number (Number (c, i + 1)) 2))));
  Uno_game.iter_color ~f:(fun c -> Uno_game.(check_number (Skip c) 2));
  Uno_game.iter_color ~f:(fun c -> Uno_game.(check_number (Reverse c) 2));
  Uno_game.iter_color ~f:(fun c -> Uno_game.(check_number (Draw2 c) 2));
  check_wild ()
;;

(* let show_all_cards (s : Uno_game.t) =
   s |> count_card
   |> Core.List.map ~f:(fun (card_str, count) ->
          card_str ^ " : " ^ string_of_int count)
   |> Core.String.concat ~sep:"\n"
   |> fun x -> "\n------\n" ^ x ^ "\n-------\n" *)

let test_game_init _ =
  let n_player = 5 in
  assert_equal
    (n_player * Uno_game.game_rules.init_dealed_cards)
    Uno_game.(
      create ~number_of_players:n_player () |> number_of_cards_in_player_hands)
;

let n_player = 5 in
Uno_game.(create ~number_of_players:n_player () |> card_invariant_check)
;

let n_player = 5 in
assert_equal Uno_game.game_rules.total_number_of_cards
  Uno_game.(create ~number_of_players:n_player () |> all_cards |> List.length)
;

assert_raises (Uno_game.GameError NotEnoughCardsInDrawPile) (fun _ ->
    Uno_game.(
      create
        ~number_of_players:
          (1 + (game_rules.total_number_of_cards / game_rules.init_dealed_cards))
        ()))
;

assert_raises (Uno_game.GameError PlayerNotExist) (fun _ ->
    Uno_game.(create ~number_of_players:3 () |> deal_cards ~id:"" ~number:10))
;

assert_raises (Uno_game.GameError NotEnoughCardsInDrawPile) (fun _ ->
    Uno_game.(
      let s = create ~number_of_players:3 () in
      deal_cards
        ~id:(Core.List.hd_exn s.players)
        ~number:(Uno_game.game_rules.total_number_of_cards + 1)
        s))

let test_card_rules _ =
  assert_equal true
    Uno_game.(card_can_play_after (Number (Blue, 5)) (Number (Red, 5)))
;

assert_equal false Uno_game.(card_equal (Number(Red, 5)) (Number(Red, 6)));
assert_equal false Uno_game.(card_valid (Number(Red, 99)));

assert_equal true Uno_game.(card_can_play_after (Number (Red, 5)) (Skip Red));
assert_equal true Uno_game.(card_can_play_after (Skip Red) (Skip Red));
assert_equal true Uno_game.(card_can_play_after (Reverse Red) (Number (Red, 0)))
;
assert_equal true Uno_game.(card_can_play_after (Draw4 Red) (Skip Red));
assert_equal true Uno_game.(card_can_play_after (Draw4 Red) (Draw2 Red));
assert_equal true Uno_game.(card_can_play_after (Draw2 Red) (Draw4 Red));
assert_equal true Uno_game.(card_can_play_after (Draw2 Red) (Wild Red));
assert_equal false Uno_game.(card_can_play_after (Draw2 Blue) (Wild Red));
assert_equal true Uno_game.(card_can_play_after (Draw4 Blue) (Draw4 Red));

assert_equal false
  Uno_game.(card_can_stack_after (Number (Blue, 2)) (Draw4 Red))
;

assert_equal false Uno_game.(card_can_stack_after (Draw2 Red) (Draw4 Red));
assert_equal true Uno_game.(card_can_stack_after (Draw4 Blue) (Draw2 Red));;

let series =
  "uno_game tests"
  >::: [
         "Gen_id" >:: test_gen_id;
         "Card Rules" >:: test_card_rules;
         "Game Initialize" >:: test_game_init;
       ]

let () = run_test_tt_main series
