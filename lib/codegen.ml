open Expr

let rec print_pattern fmt pat = 
  match pat with
  | PBool b -> Format.fprintf fmt "%b" b
  | PInt i -> Format.fprintf fmt "%d" i
  | PVar s -> Format.fprintf fmt "%s" s
  | PTuple [] -> Format.fprintf fmt "()"
  | PTuple l -> 
    Format.fprintf fmt "(@[<hv 1>%a@])" (
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") (fun _ -> print_pattern fmt))
    ) l

let rec print fmt expression = match expression with
  | Int x -> Format.fprintf fmt "%d" x
  | Bool b -> Format.fprintf fmt "%b" b
  | String s -> Format.fprintf fmt "%s" s
  | If(cond,trb,flb) -> Format.fprintf fmt "(@[<hv 1>@[if@ %a@]@, @[then@ %a@]@, @[else@ %a@]@])" print cond print trb print flb
  | Let(pat,e1,e2,true) -> Format.fprintf fmt "(@[<hv 1>@[let rec @[%a@ =@]@ @[%a@ in@]@]@ @[%a@]@])" print_pattern pat print e1 print e2
  | Let(pat,e1,e2,false) -> Format.fprintf fmt "(@[<hv 1>@[let @[%a@ =@]@ @[%a@ in@]@]@ @[%a@]@])" print_pattern pat print e1 print e2
  | Fun(pat,e) -> Format.fprintf fmt "(@[<hv 1>fun@ @[%a@ ->@]@ @[%a@]@])" print_pattern pat print e
  | App(App(String ":=", e1), e2) -> Format.fprintf fmt "(@[<hv 1>%a :=@ %a@])" print e1 print e2
  | App(e1,e2) -> Format.fprintf fmt "@[<hv 1>(@[%a@]@ @[%a@])@]" print e1 print e2
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple l -> Format.fprintf fmt "(@[<hv 1>%a@])" (
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") (fun _ -> print fmt))
    ) l
  | Op(s,e1,e2) -> Format.fprintf fmt "@[<hv 1>(@[%a@]@ %s@ @[%a@])@]" print e1 s print e2
  | Seq(e1,e2) -> Format.fprintf fmt "@[%a@];@ @[%a@]" print e1 print e2
  | Match(e1,lst) -> Format.fprintf fmt "(@[<v 1>@[<hv 1>match@ %a@ with@]@ %a@])" print e1 (
    Format.(pp_print_list (fun _ -> (fun (p,e) -> Format.fprintf fmt "| @[<hv 1>@[%a@]@ ->@ @[%a@]@]" print_pattern p print e)))
  ) lst