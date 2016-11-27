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
  else if x == plant && y == water
  then (plant, plant)

  -- water/salt_water + metal = water/salt_water + sand
  else if x == water && y == metal && r < 1
  then (water, sand)
  else if x == metal && y == water && r < 1
  then (sand, water)
  else if x == salt_water && y == metal && 3 < 3
  then (salt_water, sand)
  else if x == metal && y == salt_water && r < 3
  then (sand, salt_water)

  -- lava + stone = lava + lava
  else if x == lava && y == stone && r < 5
  then (lava, lava)
  else if x == stone && y == lava && r < 5
  then (lava, lava)

  -- lava + metal/sand/salt = 2 * lava
  else if x == lava && y == metal && r < 1 then (lava, lava)
  else if x == metal && y == lava && r < 1 then (lava, lava)
  else if x == lava && y == sand && r < 50 then (lava, lava)
  else if x == sand && y == lava && r < 50 then (lava, lava)
  else if x == lava && y == salt && r < 50 then (lava, lava)
  else if x == salt && y == lava && r < 50 then (lava, lava)

  -- lava + oil/plant = lava + fire
  else if x == lava && y == oil && r < 80 then (lava, fire)
  else if x == oil && y == lava && r < 80 then (fire, lava)
  else if x == lava && y == plant && r < 80 then (lava, fire)
  else if x == plant && y == lava && r < 80 then (fire, lava)

  -- water + lava = steam + stone
  else if x == water && y == lava then (steam_water, stone)
  else if x == lava && y == water then (stone, steam_water)

  -- salt_water + lava = steam + stone OR steam + salt
  else if x == salt_water && y == lava
  then if r < 20 then (steam_water, salt) else (steam_water, stone)
  else if x == lava && y == salt_water
  then if r < 20 then (salt, steam_water) else (stone, steam_water)

  else (x,y)
