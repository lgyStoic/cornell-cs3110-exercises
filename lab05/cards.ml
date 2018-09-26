type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank = int

type card = {
  suit: suit;
  rank: rank;
}

let ace_of_clubs = {
  suit = Clubs;
  rank = 14;
}

let queen_of_hearts = {
  suit = Hearts;
  rank = 12;
}

let two_of_diamonds = {
  suit = Diamonds;
  rank = 2;
}

let seven_of_spades = {
  suit = Spades;
  rank = 7;
}
