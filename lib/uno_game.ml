open Core

(* TODO: add state for player to indicate 
   whehter the player has drew card this turn *)

module Uno_game = struct
  type game_error =
    | NotEnoughCardsInDrawPile
    | PlayerNotExist
    | InvalidCard
    (* InvalidCardInHand *)
    | NotSuchCardInHand
    | CardNotMatchWithDiscardTop
    | DrawStackHasNonDrawCard
    | NotCurrentPlayer
    | CardNotStackWithOtherDrawCards

  exception GameError of game_error

  (* Unique Id Generator *)
  module Id_gen = struct
    let gen_id ?(length = 10) () : string =
      let gen () =
        char_of_int
          (match Random.int (26 + 26 + 10) with
          | n when n < 26 -> int_of_char 'a' + n
          | n when n < 26 + 26 -> int_of_char 'A' + n - 26
          | n -> int_of_char '0' + n - 26 - 26)
      in
      Core.String.of_char_list (List.init length ~f:(fun _ -> gen ()))

    let gen_id_n ?(pwd_length = 10) (n : int) =
      let pwds = Core.Hash_set.create (module Core.String) in
      while Core.Hash_set.length pwds < n do
        Core.Hash_set.add pwds (gen_id ~length:pwd_length ())
      done;
      Core.Hash_set.to_list pwds
  end

  (* Card Definitions *)
  type color = Red | Blue | Yellow | Green [@@deriving show, ord]

  let default_color : color = Red
  let equal_color l r = compare_color l r = 0
  let colors = [ Red; Blue; Yellow; Green ]

  type symbol = SDraw4 | SWild | SDraw2 | SSkip | SReverse | SNumber
  [@@deriving ord, show]

  let equal_symbol l r = compare_symbol l r = 0

  type card =
    | Draw4 of color
    | Wild of color
    | Draw2 of color
    | Skip of color
    | Reverse of color
    | Number of (color * int)
  [@@deriving show, ord]

  let card_equal l r = compare_card l r = 0

  let card_valid (c : card) : bool =
    match c with
    | Draw4 _ -> true
    | Wild _ -> true
    | Draw2 _ -> true
    | Skip _ -> true
    | Reverse _ -> true
    | Number (_, num) when num >= 0 && num <= 9 -> true
    | _ -> false

  let card_color (c : card) =
    match c with
    | Draw4 color -> color
    | Wild color -> color
    | Draw2 color -> color
    | Skip color -> color
    | Reverse color -> color
    | Number (color, _) -> color

  let card_number (c : card) = match c with Number (_, n) -> n | _ -> ~-1

  let card_symbol (c : card) =
    match c with
    | Draw4 _ -> SDraw4
    | Wild _ -> SWild
    | Draw2 _ -> SDraw2
    | Skip _ -> SSkip
    | Reverse _ -> SReverse
    | Number _ -> SNumber

  let card_symbol_match (l : card) (r : card) : bool =
    equal_symbol (card_symbol l) (card_symbol r)

  let card_color_match (l : card) (r : card) : bool =
    equal_color (card_color l) (card_color r)

  let card_number_match (l : card) (r : card) : bool =
    let l = card_number l in
    let r = card_number r in
    l <> ~-1 && r <> ~-1 && l = r

  let card_can_play_after (l : card) (r : card) : bool =
    match (l, r) with
    | Draw4 _, _ -> true
    | Wild _, _ -> true
    | _ ->
        card_symbol_match l r || card_color_match l r || card_number_match l r

  let card_can_stack_after (l : card) (r : card) : bool =
    match (l, r) with
    | Draw4 _, _ -> true
    | Draw2 _, Draw2 _ -> true
    | _ -> false

  let for_each_color ~(f : color -> card list) : card list =
    List.concat @@ List.map ~f colors

  let iter_color ~(f : color -> unit) : unit =
      ignore @@ List.map ~f colors

  (* Cards Processing *)
  (* Cards Generator / Card Dealing *)

  module Cards = struct
    let number_cards : card list =
      let gen_number_cards (color : color) : card list =
        Core.List.init 10 ~f:(fun i -> Number (color, i))
        @ Core.List.init 9 ~f:(fun i -> Number (color, i + 1))
      in
      for_each_color ~f:gen_number_cards

    let action_cards : card list =
      Core.List.concat
        [
          for_each_color ~f:(fun color ->
              [
                Skip color;
                Skip color;
                Reverse color;
                Reverse color;
                Draw2 color;
                Draw2 color;
              ]);
          Core.List.init 4 ~f:(fun _ -> Wild default_color);
          Core.List.init 4 ~f:(fun _ -> Draw4 default_color);
        ]

    let cards = Core.List.concat [ action_cards; number_cards ]

    let shuffle_cards (cards : card list) : card list =
      cards
      |> Core.List.map ~f:(fun card -> (Core.Random.bits (), card))
      |> Core.List.sort ~compare:(fun (l, _) (r, _) -> l - r)
      |> Core.List.map ~f:snd

    let create_draw_pile () = cards |> shuffle_cards

    let merge_cards (l : card list) (r : card list) =
      List.sort ~compare:compare_card (l @ r)

    let deal_cards (draw_pile : card list) ~(number : int) :
        card list * card list =
      if Core.List.length draw_pile < number then
        raise @@ GameError NotEnoughCardsInDrawPile
      else
        match Core.List.split_n draw_pile number with
        | player, draw_pile -> (merge_cards player [], draw_pile)

    let has_card (c : card) (cs : card list) : bool =
      Core.List.mem cs c ~equal:(fun l r -> compare_card l r = 0)

    let card_can_play_after_discard_pile_top (c : card)
        (discard_pile : card list) : bool =
      match discard_pile with
      | [] -> true
      | top_card :: _ -> card_can_play_after c top_card

    let card_can_play_after_draw_stack_top (c : card) (draw_stack : card list) :
        bool =
      match draw_stack with
      | [] -> true
      | top_card :: _ -> card_can_stack_after c top_card

    let remove_one (x : 'a) (xs : 'a list) ~(equal : 'a -> 'a -> bool) =
      snd
      @@ List.fold_right xs ~init:(Some x, [])
           ~f:(fun cur_x (maybe_to_remove, result) ->
             match maybe_to_remove with
             | None -> (None, cur_x :: result)
             | Some to_remove ->
                 if equal to_remove cur_x then (None, result)
                 else (maybe_to_remove, cur_x :: result))

    let play_card (c : card) (hands : card list) (discard_pile : card list)
        (draw_stack : card list) : card list * card list =
      if not @@ card_valid c then raise @@ GameError InvalidCard
      else if not @@ has_card c hands then raise @@ GameError NotSuchCardInHand
      else if not @@ card_can_play_after_discard_pile_top c discard_pile then
        raise @@ GameError CardNotMatchWithDiscardTop
      else if not @@ card_can_play_after_draw_stack_top c draw_stack then
        raise @@ GameError CardNotStackWithOtherDrawCards
      else (remove_one c hands ~equal:card_equal, c :: discard_pile)

    let cannot_play_any ~(hands : card list) ~(discard_pile : card list)
        ~(draw_stack : card list) : bool =
      Core.List.for_all hands ~f:(fun c ->
          not
          @@ (card_valid c
             && card_can_play_after_discard_pile_top c discard_pile
             && card_can_play_after_draw_stack_top c draw_stack))
  end

  type direction = Positive | Negative

  let direction_delta direction =
    match direction with Positive -> 1 | Negative -> ~-1

  let direction_reverse direction =
    match direction with Positive -> Negative | Negative -> Positive

  module Player_hands = Core.Map.Make (Core.String)

  type t = {
    draw_pile : card list;
    discard_pile : card list;
    draw_stack : card list;
    players : string list;
    player_hands : card list Player_hands.t;
    current_player : string;
    direction : direction;
  }

  type game_rules = { total_number_of_cards : int; init_dealed_cards : int }

  let game_rules = { total_number_of_cards = 108; init_dealed_cards = 7 }

  let init_players (s : t) : t =
    let players = Id_gen.gen_id_n @@ List.length s.players in
    let current_player = Core.List.hd_exn players in
    let player_hands =
      Core.List.fold players ~init:s.player_hands ~f:(fun res key ->
          Player_hands.add_exn ~key ~data:[] res)
    in
    { s with players; player_hands; current_player }

  let try_reshuffle (s : t) ~(number : int) : t =
    let draw_pile_left = List.length s.draw_pile in
    if draw_pile_left < number then
      let number_of_take = game_rules.total_number_of_cards - draw_pile_left in
      {
        s with
        draw_pile =
          s.draw_pile @ Cards.shuffle_cards
          @@ Core.List.take s.draw_stack number_of_take;
      }
    else s

  let player_hand (s : t) ~(id : string) : card list =
    if not @@ Core.List.mem s.players id ~equal:String.equal then
      raise @@ GameError PlayerNotExist
    else Map.find_exn s.player_hands id

  let player_hand_change (s : t) ~(id : string) ~(new_hands : card list) : t =
    {
      s with
      player_hands =
        Map.change s.player_hands id ~f:(fun hands ->
            match hands with
            | Some _ -> Some new_hands
            | _ -> raise @@ GameError PlayerNotExist);
    }

  let deal_cards ~(id : string) ~(number : int) (s : t) =
    let s = try_reshuffle s ~number in
    let dealed, draw_pile = Cards.deal_cards ~number s.draw_pile in
    let s = player_hand_change s ~id ~new_hands:(dealed @ player_hand ~id s) in
    { s with draw_pile }

  let init_player_hands (s : t) : t =
    Core.List.fold s.players ~init:s ~f:(fun s id ->
        deal_cards ~id ~number:game_rules.init_dealed_cards s)

  let create ~(number_of_players : int) () : t =
    {
      draw_pile = Cards.create_draw_pile ();
      discard_pile = [];
      draw_stack = [];
      players = Core.List.init number_of_players ~f:(fun _ -> "");
      player_hands = Player_hands.empty;
      current_player = "";
      direction = Positive;
    }
    |> init_players |> init_player_hands

  let next_player (s : t) : string =
    match
      Core.List.findi s.players ~f:(fun _ p -> String.equal p s.current_player)
    with
    | None -> raise @@ GameError PlayerNotExist
    | Some (idx, _) ->
        let next_idx =
          idx + (direction_delta s.direction mod List.length s.players)
        in
        Core.List.nth_exn s.players next_idx

  let player_cannot_play_any ~(id : string) (s : t) : bool =
    Cards.cannot_play_any ~hands:(player_hand s ~id)
      ~discard_pile:s.discard_pile ~draw_stack:s.draw_stack

  let count_draw_stack (s : t) : int =
    Core.List.fold s.draw_stack ~init:0 ~f:(fun res c ->
        match c with
        | Draw4 _ -> res + 4
        | Draw2 _ -> res + 2
        | _ -> raise @@ GameError DrawStackHasNonDrawCard)

  let clear_draw_stack (s : t) : t = { s with draw_stack = [] }

  let before_play ~(id : string) (s : t) : t =
    if player_cannot_play_any ~id s then
      let draw_stack_count = count_draw_stack s in
      let s = clear_draw_stack s in
      match draw_stack_count with
      | 0 -> deal_cards ~id ~number:1 s
      | n -> deal_cards ~id ~number:n s
    else s

  let play_card_inner ~(id : string) ~(card : card) (s : t) : t =
    let hands = player_hand s ~id in
    let discards = s.discard_pile in
    let draw_stack = s.draw_stack in
    let new_hands, new_discards = Cards.play_card card hands discards draw_stack in
    { (player_hand_change s ~id ~new_hands) with discard_pile = new_discards }

  let play_card_action ~(card : card) (s : t) : t =
    let move_to_next_player s = { s with current_player = next_player s } in
    let push_draw_stack () = { s with draw_stack = card :: s.draw_stack } in

    let s =
      match card with
      | Draw2 _ -> push_draw_stack ()
      | Draw4 _ -> push_draw_stack ()
      | Reverse _ -> { s with direction = direction_reverse s.direction }
      | Skip _ -> move_to_next_player s
      | _ -> s
    in

    move_to_next_player s

  let play ~(id : string) ~(card : card) (s : t) : t =
    if not @@ String.equal id s.current_player then
      raise @@ GameError NotCurrentPlayer
    else
      let s = play_card_inner ~card ~id s in
      play_card_action ~card s
end
