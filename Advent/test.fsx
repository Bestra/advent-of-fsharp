module Day1 = 
    type Change = Add of int | Subtract of int
    
    let a = "+15"
    let b = "-12"
    let parseEntry e = 
      match e.[0] with
      | "+" -> Add 1
      | "-" -> Subtract 1
      | _ -> Add 0
