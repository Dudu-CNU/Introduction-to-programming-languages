module F = Format

type ae =
  | Num of int
  | Add of ae * ae
  | Sub of ae * ae
  | Neg of ae

let rec pp fmt expr = 
  match expr with
  | Num n -> F.fprintf fmt "(Num %d)" n 
  | Add (e1, e2) -> F.fprintf fmt "(Add %a %a)" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub %a %a)" pp e1 pp e2
  | Neg e -> F.fprintf fmt "(Neg %a)" pp e
