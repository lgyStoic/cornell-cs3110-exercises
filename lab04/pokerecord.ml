type poketype = Normal | Fire | Water

type pokemon = {
  name: string;
  hp: int;
  ptype: poketype;
}

let charizard = {
  name = "charizard";
  hp = 78;
  ptype = Fire
}

let metapod = {
  name = "metapod";
  hp = 50;
  ptype = Normal
}

let rec max_hp = function
  | [] -> 0
  | { hp }::xs -> max hp (max_hp xs)

let _ = max_hp [charizard; metapod]
