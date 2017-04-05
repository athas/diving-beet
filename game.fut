import "futlib/math"
import "futlib/colour"

import "step"

-- We create a module for the game to force the game state to be an abstract type.
module game: {
  type game_state

  val new_game: (i32,i32) -> game_state
  val new_game_random: (i32,i32) -> game_state

  val step: game_state -> game_state
  val render: game_state -> (f32, f32) -> f32 -> (i32, i32) -> [][]i32

  val add_element: game_state -> (f32,f32) -> f32 -> (i32,i32) -> (i32,i32) -> (i32,i32) -> i32 -> element -> game_state
  val clear_element: game_state -> (f32,f32) -> f32 -> (i32,i32) -> (i32,i32) -> (i32,i32) -> i32 -> game_state

  val insertable_elements: () -> []element
  val element_name: element -> []i32
  val element_at: game_state -> (f32,f32) -> f32 -> (i32,i32) -> (i32,i32) -> element
} = {

  let shiftHoods (offset: i32) (hoods: [#w][#h]hood): [w][h]hood =
    let new_offset = if offset == 0 then -1 else 0
    in map (\x -> map (\y ->
                       let ul = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+0)
                       let dl = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+1)
                       let ur = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+0)
                       let dr = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+1)
                       in hoodFromQuadrants ul ur dl dr)
            (iota h)) (iota w)

  type game_state = (i32,       -- generation
                     [][]hood,  -- world data
                     i32,       -- world width
                     i32        -- world height
                    )

  let divRoundingUp (x: i32) (y: i32): i32 =
    (x + y - 1) / y

  let new_game_with (ww:i32,wh:i32) (e: element): game_state =
    let w = divRoundingUp ww 2
    let h = divRoundingUp wh 2
    in (0,
        replicate w (replicate h (hoodFromQuadrants e e e e)),
        ww, wh)

  let new_game (ww:i32,wh:i32): game_state =
    new_game_with (ww,wh) nothing

  let new_game_random (ww:i32,wh:i32): game_state =
    new_game_with (ww,wh) turnip

  let step(gen: i32, hoods: [#w][#h]hood, ww: i32, wh: i32): game_state =
    let hoods' = one_step (gen+1) (shiftHoods (gen%2) hoods)
    in (gen+1, hoods', ww, wh)

  open argb

  let screen_point_to_world_point ((ul_x, ul_y): (f32,f32)) (s: f32)
                                  ((sw,sh): (i32,i32)) ((ww,wh): (i32,i32))
                                  ((x,y): (i32,i32)) =
    let x' = i32 ((ul_x + s * (f32 x / f32 sw)) * f32 ww)
    let y' = i32 ((ul_y + s * (f32 y / f32 sh)) * f32 wh)
    in (x', y')

  let elemColour (x: element): i32 =
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

  let render (gen: i32, hoods: [#w][#h]hood, ww: i32, wh: i32) (ul: (f32,f32)) (s: f32) ((sw,sh): (i32,i32)): [sw][sh]i32 =
    let offset = gen % 2
    let particle_pixel (x: i32) (y: i32) =
      elemColour (worldIndex offset hoods (x,y))
    let world_pixels = map (\x -> map (particle_pixel x) (iota wh)) (iota ww)
    let ww = (shape world_pixels)[0]
    let wh = (shape world_pixels)[1]
    let screen_pixel (x: i32) (y: i32) =
      (let (x',y') = screen_point_to_world_point ul s (sw,sh) (ww,wh) (x,y)
       in if x' >= 0 && x' < ww && y' >= 0 && y' < wh
          then unsafe world_pixels[x', y']
          else 0xFFFFFFFF)
    in map (\x -> map (screen_pixel x) (iota sh)) (iota sw)



  let dist_sq(x0:f32,y0:f32) (x1:f32,y1:f32): f32 =
    (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1)


  let line_dist_sq (p: (f32,f32)) (v: (f32,f32)) (w: (f32,f32)): f32 =
    let l2 = dist_sq v w
    in if l2 == 0f32 then dist_sq p v
       else let t = ((#1 p - #1 v) * (#1 w - #1 v) + (#2 p - #2 v) * (#2 w - #2 v)) / l2
            let t = if t > 1f32 then 1f32
                    else if t < 0f32 then 0f32
                    else t
            in dist_sq p
  ((#1 v) + t * (#1 w - #1 v),
   (#2 v) + t * (#2 w - #2 v))

  let f32p (x:i32,y:i32): (f32,f32) =
    (f32 x, f32 y)

  let line_dist (p: (i32,i32)) (v: (i32,i32)) (w: (i32,i32)): f32 =
    f32.sqrt (line_dist_sq (f32p p) (f32p v) (f32p w))

  let add_element(gen: i32, hoods: [#w][#h]hood, ww: i32, wh: i32)
                 (ul: (f32,f32)) (s: f32) ((sw,sh): (i32,i32))
                 (from_rel: (i32,i32)) (to_rel: (i32,i32)) (r: i32) (elem: element): game_state =
    let from = screen_point_to_world_point ul s (sw,sh) (ww,wh) from_rel
    let to   = screen_point_to_world_point ul s (sw,sh) (ww,wh) to_rel
    let offset = gen % 2
    let hoods' =
      map (\x -> map (\y ->
                      let (ul, ur, dl, dr) = hoodQuadrants hoods[x,y]
                      let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                      let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                      let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                      let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                      in hoodFromQuadrants
                         (if line_dist ul_p from to < f32 r && ul == nothing then elem else ul)
                         (if line_dist ur_p from to < f32 r && ur == nothing then elem else ur)
                         (if line_dist dl_p from to < f32 r && dl == nothing then elem else dl)
                         (if line_dist dr_p from to < f32 r && dr == nothing then elem else dr))
           (iota h)) (iota w)
    in (gen, hoods', ww, wh)

  let clear_element(gen: i32, hoods: [#w][#h]hood, ww: i32, wh: i32)
                   (ul: (f32,f32)) (s: f32) ((sw,sh): (i32,i32))
                   (from_rel: (i32,i32)) (to_rel: (i32,i32)) (r: i32): game_state =
    let from = screen_point_to_world_point ul s (sw,sh) (ww,wh) from_rel
    let to   = screen_point_to_world_point ul s (sw,sh) (ww,wh) to_rel
    let offset = gen % 2
    let hoods' =
      map (\x -> map (\y ->
                      let (ul, ur, dl, dr) = hoodQuadrants hoods[x,y]
                      let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                      let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                      let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                      let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                      in hoodFromQuadrants
                         (if line_dist ul_p from to < f32 r then nothing else ul)
                         (if line_dist ur_p from to < f32 r then nothing else ur)
                         (if line_dist dl_p from to < f32 r then nothing else dl)
                         (if line_dist dr_p from to < f32 r then nothing else dr))
           (iota h)) (iota w)
    in (gen, hoods', ww, wh)

  let insertable_elements(): []element =
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

  let element_name(x: element): []i32 =
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

  let element_at (gen: i32, hoods: [#w][#h]hood, ww: i32, wh: i32)
                 (ul: (f32,f32)) (s: f32) ((sw,sh): (i32,i32)) (rel_pos: (i32,i32)): element =
    let (x,y) = screen_point_to_world_point ul s (sw,sh) (ww,wh) rel_pos
    let offset = gen % 2
    in worldIndex offset hoods (x,y)
}

-- I wish this boilerplate could be written a nicer way.
entry new_game (w: i32) (h: i32) = game.new_game (w,h)
entry new_game_random (w: i32) (h: i32) = game.new_game_random (w,h)

entry step (a: game.game_state) = game.step a
entry render (a: game.game_state) (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32) = game.render a (ul_x, ul_y) s (sw,sh)

entry add_element (a: game.game_state) (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32) (b1: i32) (b2: i32) (c1: i32) (c2: i32) (d: i32) (e: element) =
  game.add_element a (ul_x,ul_y) s (sw,sh) (b1,b2) (c1,c2) d e
entry clear_element (a: game.game_state) (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32) (b1: i32) (b2: i32) (c1: i32) (c2: i32) (d: i32) =
  game.clear_element a (ul_x,ul_y) s (sw,sh) (b1,b2) (c1,c2) d

entry insertable_elements() = game.insertable_elements ()
entry element_name(a: element) = game.element_name a
entry element_at (a: game.game_state) (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32) (b1: i32) (b2: i32) =
 game.element_at a (ul_x,ul_y) s (sw,sh) (b1,b2)
