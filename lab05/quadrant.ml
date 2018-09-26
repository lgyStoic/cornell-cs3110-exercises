let sign (x:int) =
  if x > 0 then `Pos
  else if x < 0 then `Neg
  else `Zero

let quadrant (x,y) =
  match (sign x, sign y) with
    | (`Pos, `Pos) -> Some `I
    | (`Neg, `Pos) -> Some `II
    | (`Neg, `Neg) -> Some `III
    | (`Pos, `Neg) -> Some `IV
    | (`Zero, _) | (_, `Zero) -> None
