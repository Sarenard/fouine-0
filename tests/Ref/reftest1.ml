let x = ref 3 in (let _ = prInt !x in ()); x := 4; (let _ = prInt !x in ());;
