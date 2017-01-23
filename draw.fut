include futlib.numeric

default (f32)

-- RGB, stored in least significant three bytes.
type colour = i32

fun clampChannel (x: f32): f32 =
  if x < 0f32 then 0f32 else if x > 1f32 then 1f32 else x

fun mk_colour (r: f32) (g: f32) (b: f32): colour =
  (i32 (clampChannel r * 255f32) << 16) |
  (i32 (clampChannel g * 255f32) << 8)  |
  (i32 (clampChannel b * 255f32))

-- Normalise a color to the value of its largest RGB component.
fun normalised_colour (r: f32) (g: f32) (b: f32): colour =
  let m = max_channel r (max_channel g b)
  in mk_colour (r / m) (g / m) (b / m)

fun rgb_of_colour (x: colour): (f32,f32,f32) =
  (f32 ((x>>16) & 0xFF) / 255f32,
   f32 ((x>>8) & 0xFF) / 255f32,
   f32 (x & 0xFF) / 255f32)

fun add_colours (x: colour) (y: colour): colour =
  let (r0,g0,b0) = rgb_of_colour x
  let (r1,g1,b1) = rgb_of_colour y
  in normalised_colour (max_channel r0 r1)
                       (max_channel g0 g1)
                       (max_channel b0 b1)

fun mix_colours (m1: f32) (m2: f32) (c1: colour) (c2: colour): colour =
  let (r1,g1,b1) = rgb_of_colour c1
  let (r2,g2,b2) = rgb_of_colour c2

  let m12 = m1 + m2
  let m1' = m1 / m12
  let m2' = m2 / m12

  let r1s = r1 * r1
  let r2s = r2 * r2

  let g1s = g1 * g1
  let g2s = g2 * g2

  let b1s = b1 * b1
  let b2s = b2 * b2

  in mk_colour (F32.sqrt (m1' * r1s + m2' * r2s))
               (F32.sqrt (m1' * g1s + m2' * g2s))
               (F32.sqrt (m1' * b1s + m2' * b2s))

fun bright (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r * 1.2f32) (g * 1.2f32) (b * 1.2f32)

fun dim (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r * 0.8f32) (g * 0.8f32) (b * 0.8f32)

fun light (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r + 0.2f32) (g + 0.2f32) (b + 0.2f32)

fun dark (c: colour): colour =
  let (r,g,b) = rgb_of_colour c
  in mk_colour (r - 0.2f32) (g - 0.2f32) (b - 0.2f32)


fun max_channel (x: f32) (y: f32): f32 =
  if x < y then y else x

-- Basic colours
val black: colour = mk_colour 0.0 0.0 0.0
val red: colour = mk_colour 1.0 0.0 0.0
val green: colour = mk_colour 0.0 1.0 0.0
val blue: colour = mk_colour 0.0 0.0 1.0
val white: colour = mk_colour 1.0 1.0 1.0
val brown: colour = mk_colour 0.49 0.19 0.11

-- Derived colours
val yellow: colour = add_colours red green
val orange: colour = add_colours yellow red
val magenta: colour = add_colours red blue
val violet: colour = add_colours magenta blue

fun grayN (d: f32): colour = mk_colour d d d
