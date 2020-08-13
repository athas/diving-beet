type element = u8
type weight = u8
type weight_env = u8

let nothing: element = 0u8
let steam_water: element = 1u8
let steam_condensed: element = 2u8
let oil: element = 6u8
let water: element = 7u8
let salt_water: element = 8u8
let sand: element = 9u8
let salt: element = 10u8
let stone: element = 11u8
let fire: element = 12u8
let fire_end: element = 22u8
let torch: element = 23u8
let plant: element = 24u8
let spout: element = 25u8
let metal: element = 26u8
let lava: element = 27u8
let turnip: element = 28u8
let wall: element = 29u8
let napalm: element = 30u8

let elems: []element = [ nothing
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
                       , napalm
                       , turnip ]

let num_elems: i32 = length elems

let isWall (x: element): bool =
  x == torch || x == plant || x == spout || x == metal || x == wall

let isFire (x: element): bool =
  x >= fire && x <= fire_end

let isFluid (x: element): bool =
  x == steam_water || x == steam_condensed || x == oil ||
  x == water || x == salt_water ||
  x == lava || x == napalm

let weight (x: element): weight =
  if x == nothing then 2u8
  else if x == steam_water then 0u8
  else if x == steam_condensed then 0u8
  else if x == sand then salt
  else if x == lava then water
  else if x == napalm then water - 1u8
  else if isFire x then 0u8
  else x

let age (r: i32) (x: element): element =
  if x == fire_end then nothing
  else if isFire x then if r < 5000 then x + 1u8 else x
  else if x == steam_water then if r < 100 then water else steam_water
  else if x == steam_condensed then if r < 500 then water else steam_condensed
  else if x == napalm then if r < 10 then nothing else napalm
  else if x == turnip then #[unsafe] elems[r%num_elems]
  else x
