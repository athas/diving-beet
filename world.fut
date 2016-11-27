type element = u32
type cell = u32
type env = u32
type weight = u8
type weight_env = u8

-- Position in a Margolus neighborhood; ranges from 0-3.
type MargPos = int

val nothing: element = 0u32
val steam_water: element = 1u32
val steam_condensed: element = 2u32
val oil: element = 6u32
val water: element = 7u32
val salt_water: element = 8u32
val sand: element = 9u32
val salt: element = 10u32
val stone: element = 11u32
val fire: element = 12u32
val fire_end: element = 22u32
val torch: element = 23u32
val plant: element = 24u32
val spout: element = 25u32
val metal: element = 26u32
val lava: element = 27u32
val turnip: element = 28u32
val wall: element = 29u32

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

fun isFluid (x: element): weight =
  if x == steam_water || x == steam_condensed || x == oil || x == water || x == salt_water || x == lava
  then 2u8
  else 0u8

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
  else if isFire x then if r < 50 then x + 1u32 else x
  else if x == steam_water then if r < 1 then water else steam_water
  else if x == steam_condensed then if r < 5 then water else steam_condensed
  else if x == turnip then unsafe elems[(r*num_elems) / 110]
  else x
