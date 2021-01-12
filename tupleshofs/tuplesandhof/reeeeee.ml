let rec wwhile (f,b) =
  let rec wwhileHelper (f,b) c =
    match c with
    | false -> b
    | true -> 
      let(b',c') = f b in
      wwhileHelper (f,b') c'
  in wwhileHelper (f,b) true
;;