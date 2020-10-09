import "lib/github.com/athas/matte/colour"

import "step"
import "world"

let shiftHoods [w][h] (offset: i32) (hoods: [w][h]hood): [w][h]hood =
  let new_offset = if offset == 0 then -1 else 0
  let f x y =
    let x = i32.i64 x
    let y = i32.i64 y
    let ul = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+0)
    let dl = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+1)
    let ur = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+0)
    let dr = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+1)
    in hoodFromQuadrants ul ur dl dr
  in tabulate_2d w h f

let divRoundingUp (x: i64) (y: i64) =
  (x + y - 1) / y

type game_state [w][h] = {generation: i32,   -- generation
                          hoods: [w][h]hood, -- world data
                          width: i64,        -- world width
                          height: i64        -- world height
                          }

-- | The un-parameterised game state type that we expose to the
-- outside world.
type~ ext_game_state = game_state [] []

let new_game_with (ww:i64,wh:i64) (e: element): ext_game_state =
  let w = divRoundingUp ww 2
  let h = divRoundingUp wh 2
  in {generation = 0,
      hoods = replicate w (replicate h (hoodFromQuadrants e e e e)),
      width = ww, height = wh}

entry new_game (ww: i64) (wh: i64): ext_game_state = new_game_with (ww,wh) nothing
entry new_game_random (ww: i64) (wh: i64): ext_game_state = new_game_with (ww,wh) turnip

entry step ({generation=gen,hoods,width=ww,height=wh}: ext_game_state): ext_game_state =
  let hoods' = one_step (gen+1) (shiftHoods (gen%2) hoods)
  in {generation=gen+1, hoods=hoods', width=ww, height=wh}

open argb

let screen_point_to_world_point ((ul_x, ul_y): (f32,f32)) (s: f32)
                                ((sw,sh): (i64,i64)) ((ww,wh): (i64,i64))
                                ((x,y): (i64,i64)) =
  let x' = t32 ((ul_x + s * ((f32.i64 x + 0.5f32) / f32.i64 sw)) * f32.i64 ww)
  let y' = t32 ((ul_y + s * ((f32.i64 y + 0.5f32) / f32.i64 sh)) * f32.i64 wh)
  in (x', y')

let elemColour (x: element): argb.colour =
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
  then mix (f32.u8 (x - fire)) red (f32.u8 (fire_end - x)) yellow
  else black -- handles 'nothing'


let dist_sq(x0:f32,y0:f32) (x1:f32,y1:f32): f32 =
  (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1)

let f32p (x:i32,y:i32): (f32,f32) =
  (r32 x, r32 y)

entry render ({generation=gen,hoods,width=ww,height=wh}: ext_game_state)
             (ul_x: f32) (ul_y: f32) (s: f32) (sw: i64) (sh: i64)
             (b1: i64) (b2: i64) (r: i32) =
  let mouse_pos = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let offset = gen % 2
  let particle_pixel (x: i64) (y: i64) =
    elemColour (worldIndex offset hoods (i32.i64 x, i32.i64 y))
  let world_pixels = map (\x -> map (particle_pixel x) (iota wh)) (iota ww)
  let screen_pixel (y: i64) (x: i64) =
    (let (x',y') = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (x,y)
     let dist_to_mouse = dist_sq (f32p (x',y')) (f32p mouse_pos)
     let on_select_border = t32 (f32.round (f32.sqrt dist_to_mouse)) == r
     in if x' >= 0 && x' < i32.i64 ww && y' >= 0 && y' < i32.i64 wh && !on_select_border
        then #[unsafe] world_pixels[x', y']
        else 0xFFFFFFFF)
  in tabulate_2d sh sw screen_pixel

let line_dist_sq (p: (f32,f32)) (v: (f32,f32)) (w: (f32,f32)): f32 =
  let l2 = dist_sq v w
  in if l2 == 0f32 then dist_sq p v
     else let t = ((p.0 - v.0) * (w.0 - v.0) + (p.1 - v.1) * (w.1 - v.1)) / l2
          let t = if t > 1f32 then 1f32
                  else if t < 0f32 then 0f32
                  else t
          in dist_sq p
                     ((v.0) + t * (w.0 - v.0),
                      (v.1) + t * (w.1 - v.1))


let line_dist (p: (i32,i32)) (v: (i32,i32)) (w: (i32,i32)): f32 =
  f32.sqrt (line_dist_sq (f32p p) (f32p v) (f32p w))

entry add_element [h][w]
                  ({generation=gen,hoods: [w][h]hood,width=ww,height=wh}: ext_game_state)
                  (ul_x: f32) (ul_y: f32) (s: f32) (sw: i64) (sh: i64)
                  (b1: i64) (b2: i64) (c1: i64) (c2: i64) (r: i32) (elem: element): ext_game_state =
  let from = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let to   = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (c1,c2)
  let offset = gen % 2
  let f x y =
    let x = i32.i64 x
    let y = i32.i64 y
    let (ul, ur, dl, dr) = hoodQuadrants hoods[x,y]
    let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
    let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
    let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
    let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
    in hoodFromQuadrants
       (if line_dist ul_p from to < r32 r && ul == nothing then elem else ul)
       (if line_dist ur_p from to < r32 r && ur == nothing then elem else ur)
       (if line_dist dl_p from to < r32 r && dl == nothing then elem else dl)
       (if line_dist dr_p from to < r32 r && dr == nothing then elem else dr)
  let hoods' = tabulate_2d w h f
  in {generation=gen, hoods=hoods', width=ww, height=wh}

entry clear_element [h][w]
                    ({generation=gen,hoods:[w][h]hood,width=ww,height=wh}: ext_game_state)
                    (ul_x: f32) (ul_y: f32) (s: f32) (sw: i64) (sh: i64)
                    (b1: i64) (b2: i64) (c1: i64) (c2: i64) (r: i32): ext_game_state =
  let from = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let to   = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (c1,c2)
  let offset = gen % 2
  let f x y =
    let x = i32.i64 x
    let y = i32.i64 y
    let (ul, ur, dl, dr) = #[unsafe] hoodQuadrants hoods[x,y]
    let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
    let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
    let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
    let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
    in hoodFromQuadrants
       (if line_dist ul_p from to < r32 r then nothing else ul)
       (if line_dist ur_p from to < r32 r then nothing else ur)
       (if line_dist dl_p from to < r32 r then nothing else dl)
       (if line_dist dr_p from to < r32 r then nothing else dr)
  let hoods' = tabulate_2d w h f
  in {generation=gen, hoods=hoods', width=ww, height=wh}


entry insertable_elements =
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

entry element_name(x: element): []u8 =
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

entry element_at ({generation=gen,hoods,width=ww,height=wh}: ext_game_state)
                 (ul_x: f32) (ul_y: f32) (s: f32) (sw: i64) (sh: i64) (b1: i64) (b2: i64) =
  let (x,y) = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let offset = gen % 2
  in worldIndex offset hoods (x,y)
