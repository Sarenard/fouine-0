let rec fact_aux = (fun acc -> fun counter -> (if (counter = 0) then acc else fact_aux (acc*counter) (counter-1))) in let fact = fact_aux 1 in fact 10;;
