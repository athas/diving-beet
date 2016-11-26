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
  map (fn y => map (fn x => if x == y || x == 200 || y == 200 then water else nothing) (iota w)) (iota h)
 )

entry step_game(gen: int, cur_mask: [h][w]MargPos, next_mask: [h][w]MargPos,
                elems: [h][w]element): game_state =
  (gen + 1, next_mask, cur_mask, step gen cur_mask elems)

entry render(_: int, _: [h][w]MargPos, _: [h][w]MargPos, elems: [h][w]element): [w][h]int =
  map (fn r => map elemColour r) (transpose elems)

fun elemColour (x: element): int =
  if      x == nothing
  then black
  else if x == steam_water
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

  else if isFire x
  then mix_colours (1.0f32 * f32 (x - fire))
                   (1.0f32 * f32 (fire_end - x))
                   red yellow
  else black

entry main (h:int,w:int): game_state =
  let state = new_game (h,w)
  in step_game state
