import "world"

-- The random parameter 'r' is used to decrease the frequency of some interactions.
let applyAlchemy (r: i32) (x: element) (y: element): (element, element) =
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

  -- torch/napalm + nothing = torch/napalm + fire
  else if x == nothing && (y == torch || y == napalm)
  then (fire, y)
  else if (x == torch || y == napalm) && y == nothing
  then (x, fire)

  -- spout + nothing = spout + water
  else if x == nothing && y == spout
  then (water, spout)
  else if x == spout && y == nothing
  then (spout, water)

  -- fire + plant = new fire + sand OR new fire + new fire
  else if isFire x && y == plant
  then if r < 2000 then (fire, sand) else (fire, fire)
  else if x == plant && isFire y
  then if r < 2000 then (sand, fire) else (fire, fire)

  -- water + plant = plant + plant
  else if x == water && y == plant
  then (plant, plant)
  else if x == plant && y == water
  then (plant, plant)

  -- water/salt_water + metal = water/salt_water + sand
  else if x == water && y == metal && r < 100
  then (water, sand)
  else if x == metal && y == water && r < 100
  then (sand, water)
  else if x == salt_water && y == metal && r < 300
  then (salt_water, sand)
  else if x == metal && y == salt_water && r < 300
  then (sand, salt_water)

  -- lava + stone/metal/sand/salt = 2 * lava
  else if x == lava && y == stone && r < 500 then (lava, lava)
  else if x == stone && y == lava && r < 500 then (lava, lava)

  else if x == lava && y == metal && r < 100 then (lava, lava)
  else if x == metal && y == lava && r < 100 then (lava, lava)

  else if x == lava && y == sand && r < 5000 then (lava, lava)
  else if x == sand && y == lava && r < 5000 then (lava, lava)

  else if x == lava && y == salt && r < 5000 then (lava, lava)
  else if x == salt && y == lava && r < 5000 then (lava, lava)

  -- lava + oil/plant = lava + fire
  else if x == lava && y == oil && r < 8000 then (lava, fire)
  else if x == oil && y == lava && r < 8000 then (fire, lava)
  else if x == lava && y == plant && r < 8000 then (lava, fire)
  else if x == plant && y == lava && r < 8000 then (fire, lava)

  -- water + lava = steam + stone
  else if x == water && y == lava then (steam_water, stone)
  else if x == lava && y == water then (stone, steam_water)

  -- salt_water + lava = steam + stone OR steam + salt
  else if x == salt_water && y == lava
  then if r < 2000 then (steam_water, salt) else (steam_water, stone)
  else if x == lava && y == salt_water
  then if r < 2000 then (salt, steam_water) else (stone, steam_water)

  else (x,y)
