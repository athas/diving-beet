include world
include alchemy

-- Position in a Margolus neighborhood; ranges from 0-3.
type marg_pos = int

-- A Margolus neighborhood.  We will just call these 'hood's, because
-- it is more gangsta.
type hood = (u8,u8,u8,u8)

-- The following two functions should be used for all hood
-- interaction.  Never just pattern patch directly on the value!
-- Pretend it is an abstract type.
fun hoodQuadrants ((ul,ur,dl,dr): hood): (element, element, element, element) =
  (ul,ur,dl,dr)

fun hoodFromQuadrants (ul: element) (ur: element) (dl: element) (dr: element): hood =
  (ul,ur,dl,dr)

-- Return the requested quadrant from the given hood.
fun hoodQuadrant (h: hood) (i: marg_pos): element =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then ul0
  else if i == 1 then ur0
  else if i == 2 then dl0
  else                dr0

-- Return the requested quadrant from the given hood.
fun setHoodQuadrant (h: hood) (i: marg_pos) (x: element): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then hoodFromQuadrants x ur0 dl0 dr0
  else if i == 1 then hoodFromQuadrants ul0 x dl0 dr0
  else if i == 2 then hoodFromQuadrants ul0 ur0 x dr0
  else                hoodFromQuadrants ul0 ur0 dl0 x

-- Swap to quadrants in a hood.
fun swapHoodQuadrants (h: hood) (i: marg_pos) (j: marg_pos): hood =
  let x = hoodQuadrant h i
  let y = hoodQuadrant h j
  in setHoodQuadrant (setHoodQuadrant h i y) j x

-- Make sure the permutation has no duplicate entries.
fun permuteHoodQuadrants (h: hood) ((ul,ur,dl,dr): (marg_pos, marg_pos, marg_pos, marg_pos)): hood =
  hoodFromQuadrants (hoodQuadrant h ul) (hoodQuadrant h ur)
                    (hoodQuadrant h dl) (hoodQuadrant h dr)

fun indexToHood (offset: int) (i: int): (int, int) =
  if offset == 0 then (i / 2, i % 2)
  else ((i+1) / 2, (i+1) % 2)

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns 'nothing'.
fun hoodArrayIndex (offset: int) (elems: [w][h]hood) ((x,y): (int,int)): element =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then nothing
     else hoodQuadrant (unsafe elems[hx,hy]) (ix+iy*2)

-- From http://stackoverflow.com/a/12996028
fun hash(x: int): int =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

-- An array with a "random" number for every hood.
fun hoodRandoms ((w,h): (int,int)) ((lower,upper): (int,int)) (gen: int): [w][h]int =
  reshape (w,h)
  (map (fn i => (hash (gen ^ i*4)) % (upper-lower+1) + lower) (iota (w*h)))

-- Compute interactions and aging for every hood, returning a new
-- array of hoods.
fun step (gen: int) (hoods: [w][h]hood): [w][h]hood =
  let randomish = hoodRandoms (w,h) (0,100) gen
  let envs = map (fn randomish_r hoods_r => map alchemy randomish_r hoods_r)
                 randomish hoods
  in map (fn r0 r1 => map ageHood r0 r1) randomish
     (map (fn r => map gravity r) envs)

-- Age every cell within a hood.  We use our (single) random number to
-- generate four new random numbers,which are then used for the aging.
fun ageHood (seed: int) (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h in
  hoodFromQuadrants (age (hash (seed^0) % 100) ul)
                    (age (hash (seed^1) % 100) ur)
                    (age (hash (seed^2) % 100) dl)
                    (age (hash (seed^3) % 100) dr)

-- Apply alchemy within a hood.
fun alchemy (r: int) (h: hood): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if ul0 == ur0 && ur0 == dl0 && dl0 == dr0
  then h
  else -- Apply interaction among the components
       let (ul1, ur1) = applyAlchemy r ul0 ur0
       let (ur , dr2) = applyAlchemy r ur1 dr0
       let (dr , dl3) = applyAlchemy r dr2 dl0
       let (dl , ul ) = applyAlchemy r dl3 ul1
       in hoodFromQuadrants ul ur dl dr

-- Apply gravity within a hood.
fun gravity (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h

  let (ul, ur, dl, dr) =
    -- First check for fluid flow.
    if ((isFluid dl && dr == nothing) || (isFluid dr && dl == nothing)) &&
       isFluid ul && isFluid ur
    then (ul, ur, dr, dl)
    else if isFluid ul && weight ur < weight ul && dl != nothing && dr != nothing && ! isWall dl && ! isWall dr
    then (ur, ul, dl, dr)
    else if isFluid ur && weight ul < weight ur && dl != nothing && dr != nothing && ! isWall dl && ! isWall dr
    then (ur, ul, dr, dl)
    else if isFluid dl && weight ul < weight dl && weight ur < weight dl && weight dr < weight dl
    then (ul, ur, dr, dl)
    else if isFluid dr && weight ul < weight dr && weight ur < weight dr && weight dl < weight dr
    then (ul, ur, dr, dl)

    -- No fluid flow?  Let gravity do its work.
    else let (ul, dl) = checkIfDrop ul dl
         let (ur, dr) = checkIfDrop ur dr
         let (ul, dr) = checkIfDrop ul dr
         let (ur, dl) = checkIfDrop ur dl
         in (ul, ur, dl, dr)

  in hoodFromQuadrants ul ur dl dr

fun checkIfDrop (above: element) (below: element): (element, element) =
  if isWall above || isWall below || weight below >= weight above
  then (above, below)
  else (below, above)
