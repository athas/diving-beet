include world
include gravity
include alchemy

-- From http://stackoverflow.com/a/12996028
fun hash(x: int): int =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

fun rand_array (seed: int) (n: int) (lower: int, upper: int): [n]int =
  map (fn (i: int): int  =>
        -- We hash i+n to ensure that a random length-n array is not a
        -- prefix of a random length-(n+m) array.
        (hash (seed ^ i+n)) % (upper-lower+1) + lower) (iota n)

fun randomishInt2Array ((h,w): (int,int)) ((lower,upper): (int,int)) (gen: int): [h][w]int =
  reshape (h,w) (rand_array gen (h*w) (lower,upper))

fun step (gen: int) (mask: [h][w]MargPos) (array: [h][w]element): [h][w]element =
  let randomish = randomishInt2Array (h, w) (0,100) gen
  let envs = map (fn as bcs => map (fn a (b,c) => (alchemy a b, c)) as bcs)
                 randomish (margStencil (zip@1 array mask))
  in map (fn r0 r1 => map age r0 r1) randomish
     (map (fn r0 r1 => map mkCell r0 r1) envs
      (map (fn r => map weigh r) envs))

fun mkCell (env: env, _: MargPos) (pos: MargPos): cell =
  margQuadrant pos env

-- Mask to extract cell at quadrant 'pos'
fun margQuadrant (pos: MargPos) (env: env): cell =
  (env & (0xFFu32 << u32(8*pos))) >> u32(8*pos)

-- Break up the environment into its four components.
fun splitEnv (env: env): (cell, cell, cell, cell) =
  -- Masks for extracting 8-bit slices.
  let eight1 = 0xFFu32
  let eight2 = eight1 << 8u32
  let eight3 = eight2 << 8u32
  let eight4 = eight3 << 8u32

  let ul = (env & eight1) >> 0u32
  let ur = (env & eight2) >> 8u32
  let dl = (env & eight3) >> 16u32
  let dr = (env & eight4) >> 24u32
  in (ul, ur, dl, dr)

fun max_weight (x: weight) (y: weight): weight =
  if x < y then y else x

fun combine (ul: cell, ur: cell, dl: cell, dr: cell): cell =
  ul | (ur << 8u32) | (dl << 16u32) | (dr << 24u32)

fun combine' (ul: weight, ur: weight, dl: weight, dr: weight): weight =
  ul | (ur << 2u8) | (dl << 4u8) | (dr << 6u8)

-- Apply gravity to the cell at quadrant 'pos' in 'env' returning the
-- quadrant it should swap with
fun weigh (env: env, pos: MargPos): MargPos =
  let current = margQuadrant pos env
  let (ul', ur', dl', dr') = splitEnv env
  let heaviest = max_weight (max_weight (weight ul') (weight ur'))
                            (max_weight (weight dl') (weight dr'))

  -- Compare each cell with the heaviest, lowest bit set if >=
  let ul = u8 (weight ul' >= heaviest) | isFluid ul'
  let ur = u8 (weight ur' >= heaviest) | isFluid ur'
  let dl = u8 (weight dl' >= heaviest) | isFluid dl'
  let dr = u8 (weight dr' >= heaviest) | isFluid dr'
  let weighed1 = combine' (ul, ur, dl, dr)

  -- Apply gravity with respect to the heaviest.
  let x' = applyGravity weighed1 pos

  let x = if isWall (margQuadrant x' env) then pos else x'

  let nextHeaviest = isNextHeaviest heaviest
                     (isNextHeaviest heaviest
                      (isNextHeaviest heaviest
                       (isNextHeaviest heaviest heaviest (weight ul'))
                       (weight ur')) (weight dl')) (weight dr')

  -- Compare each cell with the second heaviest, lowest bit set if >=
  let ul2 = u8 (weight ul' >= nextHeaviest) | isFluid ul'
  let ur2 = u8 (weight ur' >= nextHeaviest) | isFluid ur'
  let dl2 = u8 (weight dl' >= nextHeaviest) | isFluid dl'
  let dr2 = u8 (weight dr' >= nextHeaviest) | isFluid dr'
  let weighed2 = combine' (ul2, ur2, dl2, dr2)

  -- Apply gravity with respect to the second heaviest
  let y' =  applyGravity weighed2  pos
  let y  = if isWall (margQuadrant y' env) then pos else y'

  -- Compose the two gravity passes
  let ydest' =  applyGravity (weighed1) y
  let ydest = if isWall (margQuadrant ydest' env) then y else ydest'

  in if      (ul' == ur' && ur' == dl' && dl' == dr')   then pos
     else if isWall current                             then pos
     else if x != pos || nextHeaviest == heaviest       then x
     else if ydest == y                                 then y
     else x

fun isNextHeaviest (heaviest: weight) (acc: weight) (x: weight): weight =
  if acc == heaviest then x else max_weight acc x

fun alchemy (i: int) (env: env): env =
  let (ul0, ur0, dl0, dr0) = splitEnv env
  -- Apply interaction among the components
  let (ul1, ur1) = applyAlchemy i ul0 ur0
  let (ur , dr2) = applyAlchemy i ur1 dr0
  let (dr , dl3) = applyAlchemy i dr2 dl0
  let (dl , ul ) = applyAlchemy i dl3 ul1
  in if ul0 == ur0 && ur0 == dl0 && dl0 == dr0
     then env
     else combine (ul, ur, dl, dr)

-- | Position of cells in a block automaton
--   0 1 0 1 ....
--   2 3 2 3 ....
--   ...
fun margMaskEven ((h,w): (int,int)): [h][w]MargPos =
  map (fn y => map (fn x => (x % 2) | ((y % 2) << 1)) (iota w)) (iota h)

fun margMaskOdd ((h,w): (int,int)): [h][w]MargPos =
  map (fn r => map (3-) r) (margMaskEven (h,w))

-- Given a Moore neighbourhood (3x3), find the Margolus neighbourhood
-- (2x2) and encode it as a number, combined with the Margolus
-- position for each cell
fun margStencil (array: [h][w](element, MargPos)): [h][w](env, MargPos) =
  map(fn y => map(fn x =>
         let (me, marg_pos) = array[y,x]
         let new =
           combine
           (if marg_pos == 0 then
              (me,
               margGet array y     (x+1),
               margGet array (y+1) x,
               margGet array (y+1) (x-1))
            else if marg_pos == 1 then
              (margGet array y     (x-1),
               me,
               margGet array (y+1) (x-1),
               margGet array (y+1) x)
            else if marg_pos == 2 then
              (margGet array (y-1) x,
               margGet array (y-1) (x+1),
               me,
               margGet array y     (x+1))
            else
              (margGet array (y-1) (x-1),
               margGet array (y-1) x,
               margGet array y     (x-1),
               me))
         in (new, marg_pos))
       (iota w)) (iota h)

fun margGet (array: [h][w](element, MargPos)) (y: int) (x: int): env =
  if y < 0 || y >= h || x < 0 || x >= w then nothing
  else let (r, _) = unsafe array[y,x] in r
