datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* val card_color = fn : card -> color *)

fun card_color (c: card) = 
    case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Hearts, _) => Red
      | (Diamonds, _) => Red

(* val card_value = fn : card -> int *)

fun card_value (c: card) = 
    case c of
        (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num n) => n

fun remove_card (cards: card list, cardToRemove: card, exn: exn) =
        let
                fun remove ([], _) = raise exn
                    | remove (c::cs, cardToRemove) =
                        if c = cardToRemove then cs
                        else c :: remove (cs, cardToRemove)
        in
                remove (cards, cardToRemove)
        end

val cards = [(Clubs, Num 2), (Diamonds, Ace), (Hearts, Jack), (Spades, Num 7)]

(* val remove_card = fn : card list * card * exn -> card list *)

val cards = remove_card (cards, (Hearts, Jack), IllegalMove)

(* val all_same_color = fn : card list -> bool *)

fun all_same_color (cards: card list) = 
    let
        fun same_color ([], _) = true
          | same_color ([_], _) = true
          | same_color (c1::c2::cs, color) =
            if card_color c1 = card_color c2 then same_color (c2::cs, color)
            else false
    in
        same_color (cards, card_color (hd cards))
    end

val cards_color_test = [(Clubs, Num 2), (Clubs, Ace), (Hearts, Jack), (Clubs, Num 7)]
val all_same_colors_test = all_same_color cards_color_test;

(* val all_same_suit = fn : card list -> bool *)

(* val all_same_color = fn : card list -> bool *)

(* val all_same_suit = fn : card list -> bool *)

(* val sum_cards = fn : card list -> int *)

fun sum_cards (cards: card list) = 
    let
        fun sum ([], _) = 0
          | sum (c::cs, cards) = card_value c + sum (cs, cards)
    in
        sum (cards, cards)
    end

val cards_sum_test = [(Clubs, Num 2), (Clubs, Ace), (Hearts, Jack), (Clubs, Num 7)]
val sum_cards_test = sum_cards cards_sum_test;

(* val is_legal = fn : card list * move -> bool *)


(* val score = fn : card list * int -> int *)

fun score (cards: card list, goal: int) = 
    let
        val sum = sum_cards cards
        val preliminary_score = goal - sum
        val final_score = if all_same_color cards then preliminary_score div 2 else preliminary_score
    in
        if sum > goal then 3 * (sum - goal) else final_score
    end


(* val officiate = fn : card list * move list * int -> int *)

fun officiate (cards: card list, moves: move list, goal: int) = 
    let
        fun officiate' ([], _, _) = goal
          | officiate' (c::cs, [], goal) = score (c::cs, goal)
          | officiate' (c::cs, m::ms, goal) = 
            let
                val new_cards = 
                    case m of
                        Discard card => remove_card (c::cs, card, IllegalMove)
                      | Draw => c::cs
            in
                officiate' (new_cards, ms, goal)
            end
    in
        officiate' (cards, moves, goal)
    end


val cards = [(Clubs, Num 2), (Clubs, Ace), (Clubs, Jack), (Spades, Num 7)]
val test_officiate = officiate (cards, [Discard (Clubs, Num 2), Draw, Discard (Clubs, Jack)], 21)