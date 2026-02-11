let x = (2, (3, 4)) in match x with (3, _) -> 4 | (x, (3, y)) -> x+y | _ -> 2;; 
