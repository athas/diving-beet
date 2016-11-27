include step
include draw

type game_state = (int,         -- generation
                   [][]MargPos, -- current Margolus mask
                   [][]MargPos, -- next Margolus mask
                   [][]element  -- state of the world
                  )

entry new_game (h:int,w:int): game_state =
 (0,
  margMaskEven (h,w), margMaskOdd (h,w),
  replicate h (replicate w nothing))

entry step_game(gen: int, cur_mask: [h][w]MargPos, next_mask: [h][w]MargPos,
                elems: [h][w]element): game_state =
  (gen + 1, next_mask, cur_mask, step gen cur_mask elems)

entry render(_: int, _: [h][w]MargPos, _: [h][w]MargPos, elems: [h][w]element): [w][h]int =
  map (fn r => map elemColour r) (transpose elems)

fun elemColour (x: element): int =
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
  then mix_colours (1.0f32 * f32 (x - fire))
                   (1.0f32 * f32 (fire_end - x))
                   red yellow
  else black -- handles 'nothing'

entry main (h:int,w:int): game_state =
  let state = new_game (h,w)
  in step_game state

entry add_element(gen: int, cur_mask: [h][w]MargPos, next_mask: [h][w]MargPos,
                  elems: [h][w]element) (pos: (int,int)) (r: int) (elem: element): game_state =
  (gen, cur_mask, next_mask,
  map (fn y => map (fn x => if elems[y,x] == nothing && int (dist (x,y) pos) < r
                            then elem
                            else elems[y,x]) (iota w)) (iota h))

entry clear_element(gen: int, cur_mask: [h][w]MargPos, next_mask: [h][w]MargPos,
                   elems: [h][w]element) (pos: (int,int)) (r: int): game_state =
  (gen, cur_mask, next_mask,
  map (fn y => map (fn x => if int (dist (x,y) pos) < r
                            then nothing
                            else elems[y,x]) (iota w)) (iota h))


fun dist (x0:int,y0:int) (x1:int,y1:int): f32 =
  sqrt32 (f32 ((x0-x1)**2 + (y0-y1)**2))
