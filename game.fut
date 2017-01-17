include step
include draw

-- A hood packed into a single scalar value.
type packed_hood = u32

fun packHood (h: hood): packed_hood =
  let (ul, ur, dl, dr) = hoodQuadrants h
  in ((u32(ul)<<24u32) | (u32(ur)<<16u32) | (u32(dl)<<8u32) | u32(dr))

fun unpackHood (ph: packed_hood): hood =
  hoodFromQuadrants (u8(ph>>24u32))
                    (u8(ph>>16u32))
                    (u8(ph>>8u32))
                    (u8(ph>>0u32))

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns 'nothing'.
fun packedWorldIndex (offset: i32) (hoods: [w][h]packed_hood) ((x,y): (i32,i32)): element =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then nothing
     else hoodQuadrant (unsafe unpackHood hoods[hx,hy]) (ix+iy*2)

fun packWorld (hoods: [w][h]hood): [w][h]packed_hood =
  map (\r -> map packHood r) hoods

fun shiftHoods (offset: i32) (hoods: [w][h]packed_hood): [w][h]hood =
  let new_offset = if offset == 0 then -1 else 0
  in map (\x -> map (\y ->
            let ul = packedWorldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+0)
            let dl = packedWorldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+1)
            let ur = packedWorldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+0)
            let dr = packedWorldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+1)
            in hoodFromQuadrants ul ur dl dr)
            (iota h)) (iota w)

type game_state = (i32,              -- generation
                   [][]packed_hood,  -- world data
                   i32,              -- world width
                   i32               -- world height
                  )

fun divRoundingUp (x: i32) (y: i32): i32 =
  (x + y - 1) / y

entry new_game (ww:i32,wh:i32): game_state =
  new_game_with (ww,wh) nothing

entry new_game_random (ww:i32,wh:i32): game_state =
 new_game_with (ww,wh) turnip

fun new_game_with (ww:i32,wh:i32) (e: element): game_state =
 let w = divRoundingUp ww 2
 let h = divRoundingUp wh 2
 in (0,
     replicate w (replicate h (packHood (hoodFromQuadrants e e e e))),
     ww, wh)

entry step_game(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32): game_state =
  let hoods' = step (gen+1) (shiftHoods (gen%2) hoods)
  in (gen+1, packWorld hoods', ww, wh)

entry render(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32): [ww][wh]i32 =
  let offset = gen % 2
  in map (\x -> map (\y -> elemColour (packedWorldIndex offset hoods (x,y)))
                      (iota wh)) (iota ww)

fun elemColour (x: element): i32 =
  if      x == steam_water
  then bright (light (light (light blue)))
  else if x == steam_condensed
  then bright (light (light (light blue)))
  else if x == oil
  then brown
  else if x == water
  then bright (bright (light blue))
  else if x == salt_water
  then bright (bright (light (light blue)))
  else if x == sand
  then dim yellow
  else if x == salt
  then grayN 0.95f32
  else if x == stone
  then grayN 0.7f32
  else if x == torch
  then bright orange
  else if x == plant
  then dim green
  else if x == spout
  then blue
  else if x == metal
  then mix_colours 0.2f32 0.8f32 blue (grayN 0.5f32)
  else if x == lava
  then bright red
  else if x == turnip
  then violet
  else if x == wall
  then grayN 0.4f32
  else if isFire x
  then mix_colours (f32 (x - fire))
                   (f32 (fire_end - x))
                   red yellow
  else black -- handles 'nothing'

entry add_element(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32)
                 (pos: (i32,i32)) (r: i32) (elem: element): game_state =
  let offset = gen % 2
  let hoods' =
    map (\x -> map (\y ->
        let (ul, ur, dl, dr) = hoodQuadrants (unpackHood hoods[x,y])
        let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
        let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
        let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
        let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
        in packHood (hoodFromQuadrants
                     (if dist ul_p pos < f32 r && ul == nothing then elem else ul)
                     (if dist ur_p pos < f32 r && ur == nothing then elem else ur)
                     (if dist dl_p pos < f32 r && dl == nothing then elem else dl)
                     (if dist dr_p pos < f32 r && dr == nothing then elem else dr)))
        (iota h)) (iota w)
  in (gen, hoods', ww, wh)

entry clear_element(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32)
                   (pos: (i32,i32)) (r: i32): game_state =
  let offset = gen % 2
  let hoods' =
    map (\x -> map (\y ->
        let (ul, ur, dl, dr) = hoodQuadrants (unpackHood hoods[x,y])
        let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
        let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
        let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
        let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
        in packHood (hoodFromQuadrants
                     (if dist ul_p pos < f32 r then nothing else ul)
                     (if dist ur_p pos < f32 r then nothing else ur)
                     (if dist dl_p pos < f32 r then nothing else dl)
                     (if dist dr_p pos < f32 r then nothing else dr)))
        (iota h)) (iota w)
  in (gen, hoods', ww, wh)


fun dist (x0:i32,y0:i32) (x1:i32,y1:i32): f32 =
  sqrt32 (f32 ((x0-x1)**2 + (y0-y1)**2))

entry insertable_elements(): []element =
 [ oil
 , water
 , salt_water
 , sand
 , salt
 , stone
 , fire
 , torch
 , plant
 , spout
 , metal
 , lava
 , turnip
 , wall ]

entry element_name(x: element): []i32 =
  if x == nothing then "nothing"
  else if x == steam_water then "steam"
  else if x == steam_condensed then "condensate"
  else if x == oil then "oil"
  else if x == water then "water"
  else if x == salt_water then "salt water"
  else if x == sand then "sand"
  else if x == salt then "salt"
  else if x == stone then "stone"
  else if isFire x then "fire"
  else if x == torch then "torch"
  else if x == plant then "plant"
  else if x == spout then "spout"
  else if x == metal then "metal"
  else if x == lava then "lava"
  else if x == turnip then "random"
  else if x == wall then "wall"
  else "unnamed element"

entry element_at (gen: i32, hoods: [w][h]packed_hood, _: i32, _: i32) (x: i32, y: i32): element =
  let offset = gen % 2
  in packedWorldIndex offset hoods (x,y)
