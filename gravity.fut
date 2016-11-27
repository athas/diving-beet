fun applyGravity (x: weight_env) (n: MargPos): MargPos =
  let (a,b,c,d) = applyGravity' x
  in if      n == 0 then a
     else if n == 1 then b
     else if n == 2 then c
     else                d

fun ignoreL (n: MargPos) (x: weight_env): weight_env =
  if      n == 0 then x & 0b11111101u8
  else if n == 1 then x & 0b11110111u8
  else if n == 2 then x & 0b11011111u8
  else                x & 0b01111111u8

fun applyGravity' (wenv: weight_env): (MargPos, MargPos, MargPos, MargPos) =
  let mask = wenv & 0b01010101u8 in
  if      ignoreL 3 wenv                         == 0b00111111u8 then (0,1,3,2)
  else if ignoreL 2 wenv                         == 0b11001111u8 then (0,1,3,2)
  else if ignoreL 3 (ignoreL 2 (ignoreL 1 wenv)) == 0b01010011u8 then (1,0,2,3)
  else if ignoreL 3 (ignoreL 2 (ignoreL 0 wenv)) == 0b01011100u8 then (0,1,3,2)
  else if ignoreL 3 (ignoreL 1 (ignoreL 0 wenv)) == 0b00110000u8 then (0,1,3,2)
  else if ignoreL 2 (ignoreL 1 (ignoreL 0 wenv)) == 0b11000000u8 then (0,1,3,2)

  else if mask == 0b00000001u8 then (2,1,0,3)
  else if mask == 0b00010101u8 then (0,3,2,1)
  else if mask == 0b00000101u8 then (2,3,0,1)

  else if mask == 0b00010100u8 then (0,3,2,1)
  else if mask == 0b00000100u8 then (0,3,2,1)
  else if mask == 0b01000101u8 then (2,1,0,3)

  else if mask == 0b01000001u8 then (2,1,0,3)
  else if mask == 0b00010001u8 then (3,1,2,0)
  else if mask == 0b01000100u8 then (0,2,1,3)

  else (0,1,2,3)
