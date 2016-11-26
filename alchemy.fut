include world

-- The random parameter 'r' is used to decrease the frequency of some interactions.
fun applyAlchemy (r: int) (x: element) (y: element): (element, element) =
  -- water + salt = salt_water + nothing
  if      x == water && y == salt
  then (salt_water, nothing)
  else if x == salt && y == water
  then (nothing, salt_water)

  -- wall + steam = wall + condensed steam
  else if isWall x && y == steam_water
  then (x, steam_condensed)
  else if x == steam_water && isWall y
  then (steam_condensed, y)

  -- water + fire = steam + nothing
  else if x == water && isFire y
  then (steam_water, nothing)
  else if isFire x && y == water
  then (nothing, steam_water)

  -- salt water + fire = steam + salt
  else if x == salt_water && isFire y
  then (steam_water, salt)
  else if isFire x && y == salt_water
  then (salt, steam_water)

  -- oil + fire = new fire + new fire
  else if x == oil && isFire y
  then (fire, fire)
  else if isFire x && y == oil
  then (fire, fire)

  -- torch + nothing = torch + fire
  else if x == nothing && y == torch
  then (fire, torch)
  else if x == torch && y == nothing
  then (torch, fire)

  -- spout + nothing = spout + water
  else if x == nothing && y == spout
  then (water, spout)
  else if x == spout && y == nothing
  then (spout, water)

  -- fire + plant = new fire + sand
  else if r < 20 && isFire x && y == plant
  then (fire, sand)
  else if r < 20 && x == plant && isFire y
  then (sand, fire)

  -- water + plant = plant + plant
  else if x == water && y == plant
  then (plant, plant)
  else if x == plant && y == plant
  then (plant, plant)

  -- water/salt_water + metal = water/salt_water + sand
  else if x == water && y == metal
  then (water, sand)
  else if x == metal && y == water
  then (sand, water)
  else if x == salt_water && y == metal
  then (salt_water, sand)
  else if x == metal && y == salt_water
  then (sand, salt_water)

  -- lava + stone = lava + lava
  else if x == lava && y == stone
  then (lava, lava)
  else if x == stone && y == lava
  then (lava, lava)

  -- then some more

  else (x,y)