type element = u8
type weight = u8
type weight_env = u8

val nothing: element = 0u8
val steam_water: element = 1u8
val steam_condensed: element = 2u8
val oil: element = 6u8
val water: element = 7u8
val salt_water: element = 8u8
val sand: element = 9u8
val salt: element = 10u8
val stone: element = 11u8
val fire: element = 12u8
val fire_end: element = 22u8
val torch: element = 23u8
val plant: element = 24u8
val spout: element = 25u8
val metal: element = 26u8
val lava: element = 27u8
val turnip: element = 28u8
val wall: element = 29u8

val elems: []element = [ nothing
                       , steam_water
                       , steam_condensed
                       , oil
                       , water
                       , salt_water
                       , sand
                       , salt
                       , stone
                       , fire
                       , fire_end
                       , torch
                       , plant
                       , spout
                       , metal
                       , lava
                       , turnip ]

val num_elems: int = (shape(elems))[0]

fun isWall (x: element): bool =
  x == torch || x == plant || x == spout || x == metal || x == wall

fun isFire (x: element): bool =
  x >= fire && x <= fire_end

fun isFluid (x: element): bool =
  x == steam_water || x == steam_condensed || x == oil || x == water || x == salt_water || x == lava

fun weight (x: element): weight =
  if x == nothing then 2u8
  else if x == steam_water then 0u8
  else if x == steam_condensed then 0u8
  else if x == sand then u8 salt
  else if x == lava then u8 water
  else if isFire x then 0u8
  else u8 x

fun age (r: int) (x: element): element =
  if x == fire_end then nothing
  else if isFire x then if r < 50 then x + 1u8 else x
  else if x == steam_water then if r < 1 then water else steam_water
  else if x == steam_condensed then if r < 5 then water else steam_condensed
  else if x == turnip then unsafe elems[r%num_elems]
  else x
