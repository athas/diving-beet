 -- RGB, stored in least significant three bytes.
type colour = int

fun mk_colour (r: int) (g: int) (b: int): colour =
  ((min_channel r 255) << 16) | (min_channel g 255<<8) | (min_channel b 255)

fun rgb_of_colour (x: colour): (int,int,int) =
  ((x>>16) & 0xFF,
   (x>>8) & 0xFF,
   x & 0xFF)

fun add_colours (x: colour) (y: colour): colour =
  let (r0,g0,b0) = rgb_of_colour x
  let (r1,g1,b1) = rgb_of_colour y
  in mk_colour (max_channel r0 r1)
               (max_channel g0 g1)
               (max_channel b0 b1)

fun mix_colours (m1: f32) (m2: f32) (c1: colour) (c2: colour): colour =
  let (r1,g1,b1) = rgb_of_colour c1
  let (r2,g2,b2) = rgb_of_colour c2

  let m12 = m1 + m2
  let m1' = m1 / m12
  let m2' = m2 / m12

  let r1s = f32 (r1 * r1) / 255f32
  let r2s = f32 (r2 * r2) / 255f32

  let g1s = f32 (g1 * g1) / 255f32
  let g2s = f32 (g2 * g2) / 255f32

  let b1s = f32 (b1 * b1) / 255f32
  let b2s = f32 (b2 * b2) / 255f32

  in mk_colour (int ((sqrt32 (m1' * r1s + m2' * r2s)) * 255f32))
               (int ((sqrt32 (m1' * g1s + m2' * g2s)) * 255f32))
               (int ((sqrt32 (m1' * b1s + m2' * b2s)) * 255f32))

fun bright (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (int (f32 r * 1.2f32)) (int (f32 g * 1.2f32)) (int (f32 b * 1.2f32))

fun dim (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (int (f32 r * 0.8f32)) (int (f32 g * 0.8f32)) (int (f32 b * 0.8f32))

fun light (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r + 40) (g + 40) (b + 40)

fun dark (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r - 40) (g - 40) (b - 40)


fun max_channel (x: int) (y: int): int =
  if x < y then y else x

fun min_channel (x: int) (y: int): int =
  if x < y then x else y

val black: colour = mk_colour 0 0 0
val red: colour = mk_colour 255 0 0
val green: colour = mk_colour 0 255 0
val blue: colour = mk_colour 0 0 255
val white: colour = mk_colour 255 255 255
val brown: colour = mk_colour 129 49 29
val yellow: colour = add_colours red green
val orange: colour = add_colours yellow red
val magenta: colour = add_colours red blue
val violet: colour = add_colours magenta blue

fun grayN (d: f32): colour =
  mk_colour (int (d*255f32)) (int (d*255f32)) (int (d*255f32))
