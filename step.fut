import "world"
import "alchemy"

-- Position in a Margolus neighborhood; ranges from 0-3.
type marg_pos = i32

-- A Margolus neighborhood.  We will just call these 'hood's, because
-- it is more gangsta.
type hood = (u8,u8,u8,u8)

-- The following two functions should be used for all hood
-- interaction.  Never just pattern patch directly on the value!
-- Pretend it is an abstract type.
let hoodQuadrants ((ul,ur,dl,dr): hood): (element, element, element, element) =
  (ul,ur,dl,dr)

let hoodFromQuadrants (ul: element) (ur: element) (dl: element) (dr: element): hood =
  (ul,ur,dl,dr)

-- Return the requested quadrant from the given hood.
let hoodQuadrant (h: hood) (i: marg_pos): element =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then ul0
  else if i == 1 then ur0
  else if i == 2 then dl0
  else                dr0

let indexToHood (offset: i32) (i: i32): (i32, i32) =
  if offset == 0 then (i / 2, i % 2)
  else ((i+1) / 2, (i+1) % 2)

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns 'nothing'.
let worldIndex [w][h] (offset: i32) (elems: [w][h]hood) ((x,y): (i32,i32)): element =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then nothing
     else hoodQuadrant (unsafe elems[hx,hy]) (ix+iy*2)

-- From http://stackoverflow.com/a/12996028
let hash(x: i32): i32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

-- An array with a "random" number for every hood.
let hoodRandoms ((w,h): (i32,i32)) ((lower,upper): (i32,i32)) (gen: i32): [w][h]i32 =
  reshape (w,h)
  (map (\i -> (hash (gen ^ i*4)) % (upper-lower+1) + lower) (iota (w*h)))

-- Age every cell within a hood.  We use our (single) random number to
-- generate four new random numbers,which are then used for the aging.
let ageHood (seed: i32) (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h in
  hoodFromQuadrants (age (hash (seed^0) % 10000) ul)
                    (age (hash (seed^1) % 10000) ur)
                    (age (hash (seed^2) % 10000) dl)
                    (age (hash (seed^3) % 10000) dr)

-- Apply alchemy within a hood.
let alchemy (r: i32) (h: hood): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if ul0 == ur0 && ur0 == dl0 && dl0 == dr0
  then h
  else -- Apply interaction among the components
       let (ul1, ur1) = applyAlchemy r ul0 ur0
       let (ur , dr2) = applyAlchemy r ur1 dr0
       let (dr , dl3) = applyAlchemy r dr2 dl0
       let (dl , ul ) = applyAlchemy r dl3 ul1
       in hoodFromQuadrants ul ur dl dr

let checkIfDrop (above: element) (below: element): (element, element) =
  if isWall above || isWall below || weight below >= weight above
  then (above, below)
  else (below, above)

-- Apply gravity within a hood.
let gravity (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h

  let (ul, ur, dl, dr) =
    -- First check for fluid flow.
    if ((isFluid dl && dr == nothing) || (isFluid dr && dl == nothing)) &&
       isFluid ul && isFluid ur
    then (ul, ur, dr, dl)
    else if isFluid ul && weight ur < weight ul && dl != nothing && dr != nothing && !(isWall dl) && !(isWall dr)
    then (ur, ul, dl, dr)
    else if isFluid ur && weight ul < weight ur && dl != nothing && dr != nothing && !(isWall dl) && !(isWall dr)
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

-- Compute interactions and aging for every hood, returning a new
-- array of hoods.
let one_step [w][h] (gen: i32) (hoods: [w][h]hood): [w][h]hood =
  let randomish = hoodRandoms (w,h) (0,10000) gen
  let envs = map (\randomish_r hoods_r -> map alchemy randomish_r hoods_r)
                 randomish hoods
  in map (\r0 r1 -> map ageHood r0 r1) randomish
     (map (\r -> map gravity r) envs)
