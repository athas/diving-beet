import "futlib/math"
import "futlib/colour"

import "step"

-- We create a module for the game to force the game state to be an abstract type.
module game: {
  type game_state

  val new_game: (i32,i32) -> game_state
  val new_game_random: (i32,i32) -> game_state

  val step: game_state -> game_state
  val render: game_state -> [][]i32

  val add_element: game_state -> (i32,i32) -> (i32,i32) -> i32 -> element -> game_state
  val clear_element: game_state -> (i32,i32) -> (i32,i32) -> i32 -> game_state

  val insertable_elements: () -> []element
  val element_name: element -> []i32
  val element_at: game_state -> (i32,i32) -> element
} = {
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

  fun new_game (ww:i32,wh:i32): game_state =
    new_game_with (ww,wh) nothing

  fun new_game_random (ww:i32,wh:i32): game_state =
    new_game_with (ww,wh) turnip

  fun new_game_with (ww:i32,wh:i32) (e: element): game_state =
    let w = divRoundingUp ww 2
    let h = divRoundingUp wh 2
    in (0,
        replicate w (replicate h (packHood (hoodFromQuadrants e e e e))),
        ww, wh)

  fun step(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32): game_state =
    let hoods' = one_step (gen+1) (shiftHoods (gen%2) hoods)
    in (gen+1, packWorld hoods', ww, wh)

  open argb

  fun render(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32): [ww][wh]i32 =
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
    then gray 0.95f32
    else if x == stone
    then gray 0.7f32
    else if x == torch
    then bright orange
    else if x == plant
    then dim green
    else if x == spout
    then blue
    else if x == metal
    then mix 0.2f32 blue 0.8f32 (gray 0.5f32)
    else if x == lava
    then bright red
    else if x == napalm
    then dark orange
    else if x == turnip
    then violet
    else if x == wall
    then gray 0.4f32
    else if isFire x
    then mix (f32 (x - fire))     red
  (f32 (fire_end - x)) yellow
    else black -- handles 'nothing'

  fun add_element(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32)
                   (from: (i32,i32)) (to: (i32,i32)) (r: i32) (elem: element): game_state =
    let offset = gen % 2
    let hoods' =
      map (\x -> map (\y ->
                      let (ul, ur, dl, dr) = hoodQuadrants (unpackHood hoods[x,y])
                      let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                      let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                      let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                      let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                      in packHood (hoodFromQuadrants
                                   (if line_dist ul_p from to < f32 r && ul == nothing then elem else ul)
                                   (if line_dist ur_p from to < f32 r && ur == nothing then elem else ur)
                                   (if line_dist dl_p from to < f32 r && dl == nothing then elem else dl)
                                   (if line_dist dr_p from to < f32 r && dr == nothing then elem else dr)))
           (iota h)) (iota w)
    in (gen, hoods', ww, wh)

  fun clear_element(gen: i32, hoods: [w][h]packed_hood, ww: i32, wh: i32)
                     (from: (i32,i32)) (to: (i32,i32)) (r: i32): game_state =
    let offset = gen % 2
    let hoods' =
      map (\x -> map (\y ->
                      let (ul, ur, dl, dr) = hoodQuadrants (unpackHood hoods[x,y])
                      let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                      let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                      let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                      let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                      in packHood (hoodFromQuadrants
                                   (if line_dist ul_p from to < f32 r then nothing else ul)
                                   (if line_dist ur_p from to < f32 r then nothing else ur)
                                   (if line_dist dl_p from to < f32 r then nothing else dl)
                                   (if line_dist dr_p from to < f32 r then nothing else dr)))
           (iota h)) (iota w)
    in (gen, hoods', ww, wh)

  fun line_dist (p: (i32,i32)) (v: (i32,i32)) (w: (i32,i32)): f32 =
    f32.sqrt (line_dist_sq (f32p p) (f32p v) (f32p w))

  fun f32p (x:i32,y:i32): (f32,f32) =
    (f32 x, f32 y)

  fun line_dist_sq (p: (f32,f32)) (v: (f32,f32)) (w: (f32,f32)): f32 =
    let l2 = dist_sq v w
    in if l2 == 0f32 then dist_sq p v
       else let t = ((#0 p - #0 v) * (#0 w - #0 v) + (#1 p - #1 v) * (#1 w - #1 v)) / l2
            let t = if t > 1f32 then 1f32
                    else if t < 0f32 then 0f32
                    else t
            in dist_sq p
  ((#0 v) + t * (#0 w - #0 v),
   (#1 v) + t * (#1 w - #1 v))

  fun dist_sq(x0:f32,y0:f32) (x1:f32,y1:f32): f32 =
    (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1)

  fun insertable_elements(): []element =
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
    , napalm
    , turnip
    , wall ]

  fun element_name(x: element): []i32 =
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
    else if x == napalm then "napalm"
    else if x == turnip then "random"
    else if x == wall then "wall"
    else "unnamed element"

  fun element_at (gen: i32, hoods: [w][h]packed_hood, _: i32, _: i32) (x: i32, y: i32): element =
    let offset = gen % 2
    in packedWorldIndex offset hoods (x,y)

}

-- I wish this boilerplate could be written a nicer way.
entry new_game (w: i32) (h: i32) = game.new_game (w,h)
entry new_game_random (w: i32) (h: i32) = game.new_game_random (w,h)

entry step (a: game.game_state) = game.step a
entry render (a: game.game_state) = game.render a

entry add_element (a: game.game_state) (b1: i32) (b2: i32) (c1: i32) (c2: i32) (d: i32) (e: element) =
  game.add_element a (b1,b2) (c1,c2) d e
entry clear_element (a: game.game_state) (b1: i32) (b2: i32) (c1: i32) (c2: i32) (d: i32) =
  game.clear_element a (b1,b2) (c1,c2) d

entry insertable_elements() = game.insertable_elements ()
entry element_name(a: element) = game.element_name a
entry element_at (a: game.game_state) (b1: i32) (b2: i32) = game.element_at a (b1,b2)
