type element = u8
type weight = u8
type weight_env = u8

def nothing : element = 0u8
def steam_water : element = 1u8
def steam_condensed : element = 2u8
def oil : element = 6u8
def water : element = 7u8
def salt_water : element = 8u8
def sand : element = 9u8
def salt : element = 10u8
def stone : element = 11u8
def fire : element = 12u8
def fire_end : element = 22u8
def torch : element = 23u8
def plant : element = 24u8
def spout : element = 25u8
def metal : element = 26u8
def lava : element = 27u8
def turnip : element = 28u8
def wall : element = 29u8
def napalm : element = 30u8

def elems : []element =
  [ nothing
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
  , turnip
  ]

def num_elems = i32.i64 (length elems)

def isWall (x: element) : bool =
  x == torch || x == plant || x == spout || x == metal || x == wall

def isFire (x: element) : bool =
  x >= fire && x <= fire_end

def isFluid (x: element) : bool =
  x == steam_water || x == steam_condensed || x == oil
  || x == water
  || x == salt_water
  || x == lava
  || x == napalm

def weight (x: element) : weight =
  if x == nothing
  then 2u8
  else if x == steam_water
  then 0u8
  else if x == steam_condensed
  then 0u8
  else if x == sand
  then salt
  else if x == lava
  then water
  else if x == napalm
  then water - 1u8
  else if isFire x
  then 0u8
  else x

def age (r: i32) (x: element) : element =
  if x == fire_end
  then nothing
  else if isFire x
  then if r < 5000 then x + 1u8 else x
  else if x == steam_water
  then if r < 100 then water else steam_water
  else if x == steam_condensed
  then if r < 500 then water else steam_condensed
  else if x == napalm
  then if r < 10 then nothing else napalm
  else if x == turnip
  then #[unsafe] elems[r % num_elems]
  else x
