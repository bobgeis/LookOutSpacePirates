module Asteroids2 where

{-| 
LOOK OUT! SPACE PIRATES!

Player Controls:

    arrow keys: turn left & right and accelerate forward & backward.
    shift key: launch torpedoes at the current hostile target.
    R key: target the nearest hostile

This is a small game modeled after LOOK OUT! SPACE ROCKS! (asteroids).
This particular mini game is exploring how combat might work.
The ultimate goal would be to make a more complicated 2D space trader game
playable in the browser.  We'll see how far we get before losing steam.  


planet pic
    http://commons.wikimedia.org/wiki/File:3D_Neptune.png
    Licensed under Creative Commons Attribution-Share Alike 4.0 International
    by user Jcpag2012


-}

{-|
Some things learned:

1) It may be possible to  improve rendering speeds:
https://groups.google.com/forum/#!topic/elm-discuss/xHfioP_anUY
this could be important for future developments.
eg: if we wanted to generate new star fields at run time.

2) Having vessels target each other is tricky.  You might like them to each
have a reference to the vessel they are targeting, but after an update that
reference would be to the version of the target that existed last tick, 
not the current version!  
Instead we are going to try the method used in the elm architecture article:
https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture
where the records that need to be identified have an integer "id".  We'll
try keeping them all in a Dict.Dict structure with the IDs as keys, and see
how that goes. 

3) Perhaps because js is single threaded, but if you leave the game running 
in a browser tab but click to a different tab, then it won't update (tick)
until you click back to the game tab, BUT it will keep track of how much
time has elapsed since the last tick!  This means that it will tick 
immediately on return with a very large tick time, so things like moveObj
will have huge time arguments.  So a pirate that was orbiting the player
in a somewhat stable fashion will have flown way way off the screen (because
only linear ballistic motion is modeled)!

4) There are a lot of useful functions in List and Dict.  
Fold is still the king, but the others are useful when you know what you want.
Examples:
    Dict.fromList: make a list of (key,value) tuples into a dict!
    Dict.partition: get separate active from inactive vessels!
    Dict.union: a way to add new or updated vessels to the vessels dict
    List.filterMap: Maybe useful!
    List.sortBy: sort hostiles by distance!
    List.unzip: make a list of tuples a tuple of lists! vuseful!
Some of the code below may benefit from being refactored to use these


-}


-- core imports
import Signal exposing ( (<~) , (~) )
import Graphics.Collage as Collage
import Graphics.Element as Element
import Color 
import Text
import Time
import Window
import Keyboard
import Char
import Mouse
import List
import Dict
import Maybe 
import Random       

import Debug  
{-|
use Debug.watch to observe variables of interest
call: Debug.watch "foo = " foo 
anywhere in order to see the value of foo in the elm reactor debug mode.
-}

{-|
Four Sections:

Signals
Model
Update
View

-}




-- SIGNALS

main = view <~ Window.dimensions ~ foldGame

foldGame : Signal Game
foldGame = Signal.foldp updateGame startGame allInputs

allInputs : Signal Input 
allInputs = Signal.sampleOn tick <|
            Input <~ tick
                   ~ Keyboard.arrows 
                   ~ Keyboard.space
                   ~ Keyboard.shift
                   ~ Keyboard.ctrl
                   ~ Time.every (Time.second * 10)
                   ~ Keyboard.isDown (Char.toCode 'R')

tick : Signal Time.Time
tick = Time.inSeconds <~ Time.fps 35



-- MODEL

type alias Input = 
    { tick : Time.Time              -- time since the last tick
    , arrows : {x:Int,y:Int}        -- arrow keys 
    , space : Bool                  -- T/F space is pressed
    , shift : Bool                  -- T/F shift is pressed
    , ctrl : Bool                   -- T/F control is pressed
    , time : Time.Time              -- time in seconds, updated every 10s
    , rKey : Bool
    }

-- constants / magic numbers

-- the size of the game board (px)
(halfW,halfH) = (400,300)
(gameW,gameH) = (halfW*2,halfH*2)
tileL = (max halfW halfH)*2

-- faction enums
playerFact = 1
pirateFact = 2
merchantFact = 3
-- faction dicts
playerDict = 
    [ (playerFact , False)
    , (pirateFact , True)
    , (merchantFact , False)
    ] |> Dict.fromList
pirateDict = 
    [ (playerFact , True)
    , (pirateFact , False)
    , (merchantFact , True)
    ] |> Dict.fromList
merchantDict = 
    [ (playerFact , False)
    , (pirateFact , True)
    , (merchantFact , False)
    ] |> Dict.fromList

hostilityDict =
    [ (playerFact , playerDict)
    , (pirateFact , pirateDict)
    , (merchantFact , merchantDict)
    ] |> Dict.fromList




type alias Game = 
    { playerID : Int                -- the ID of the player's vessel
    , camera : (Float,Float)        -- The coordinates of the camera
    , nextID : Int                  -- the ID for the next vessel to spawn
    , vessels : Dict.Dict Int Vessel -- Dict containing all the vessels
    , time : Time.Time              -- so we know when the timer is up
    , ephemera : Children
    , booms : List Boom             -- explosions
    , flashes : List Flash          -- FTL flashes
    , beams : List Beam             -- the beam effects
    }


startGame : Game
startGame =
    { playerID = 1
    , camera = (0,0)
    , nextID = 4
    , vessels = startingVessels
    , time = 0
    , ephemera = initChildren 
    , booms = []
    , flashes = []
    , beams = []
    }



type alias Object a =                -- a space object
    { a | x:Float , y:Float          -- position (px)
        , vx: Float , vy:Float       -- linear velocity (px/sec)
        , a: Float                   -- angle (rad) 
        , r:Float                    -- radius(px)
        , drag:Float                 -- drag (ratio lost /sec)
        }
  

-- space vessels
type alias Vessel = Object      
    { image:String                   --        
    , id:Int                         -- identifier in the Dict Vessel
    , thrust:Float                   -- 
    , retro:Float
    , turn:Float
    , state:VesselState
    , faction:Int
    , controls:ControlState
    , seekedBy:List Torpedo
    , children:Children
    , beam:BeamWpn
    , torp:TorpWpn
    , shields:Shields
    , blinks:Blinks
    , flares:Flares
    , drive:Drive
    }             


initPlayer : Vessel
initPlayer =
    { x=0,y=0,a=pi, vx=100, vy=100
    , r=10, drag=0.3 
    , image="images/player2.png" 
    , id=1
    , thrust=100
    , retro=-50
    , turn= degrees 100
    , faction=playerFact
    , controls = {initControls| brain <- Player}
    , state = VesselActive
    , seekedBy=[]
    , children = initChildren
    , beam = playerBmWpn
    , torp = playerTpWpn
    , shields = playerShields
    , blinks = playerBlinks
    , flares = playerFlares
    , drive = playerDrive
    }


initPirate : Vessel
initPirate = 
    newPirate (0,0) 2

newPirate : (Float,Float) -> Int -> Vessel
newPirate (x,y) id =
    { x=x,y=y,a=0, vx=0, vy=0
    , r=8, drag=0.3
    , thrust=90, retro=-50, turn= degrees 100
    , seekedBy=[]
    , image="images/pirate1.png" , id=id
    , faction=pirateFact
    , controls = {initControls| brain <- Attacker}
    , state = VesselActive
    , children = initChildren
    , beam = noBmWpn
    , torp = pirateTpWpn
    , shields = pirateShields
    , blinks = pirateBlinks
    , flares = noFlares
    , drive = pirateDrive
    }



newTransport : (Float,Float) -> Int -> Vessel
newTransport (x,y) id =
    { x=x,y=y,a=0, vx=0, vy=0
    , r=8, drag=0.3
    , thrust=60, retro=-30, turn= degrees 80
    , seekedBy=[]
    , image="images/civ1.png" , id=id
    , faction=merchantFact
    , controls = {initControls| brain <- Runner}
    , state = VesselActive
    , children = initChildren
    , beam = transportBmWpn
    , torp = noTpWpn
    , shields = transportShields
    , blinks = transportBlinks
    , flares = transportFlares
    , drive = transportDrive
    }    


startingVessels : Dict.Dict Int Vessel
startingVessels = 
    [ (1 , initPlayer)
    , (2 , newPirate (-100,-100) 2)
    , (3 , newTransport (100,100) 3)
    ] |> Dict.fromList

-- a component represent a vessel's propulsion. a motor, engine, or propulsor
type alias Drive =
    {thrust:Float
    ,retro:Float
    ,turn:Float
    }

playerDrive : Drive 
playerDrive =
    { thrust = 100 , retro = -50 , turn = degrees 100 }

pirateDrive : Drive 
pirateDrive =
    { thrust = 80 , retro = -50 , turn = degrees 90 }

transportDrive : Drive 
transportDrive =
    { thrust = 60 , retro = -30 , turn = degrees 80 }


-- children are effects that a vessel can spawn
type alias Children = 
    { booms:List Boom
    , flashes:List Boom
    , beams:List Beam
    }

initChildren : Children
initChildren = 
    { booms = [] , flashes = [] , beams = [] }  



-- how has the vessel set its controls?
type alias ControlState =
    { x:Float                   -- turn left (+1) or right (-1)
    , y:Float                   -- thrust forward (+1) or retro (-1)
    , fireT:Bool                -- attempting to fire torpedoes
    , fireB:Bool                -- attempting to fire beams
    , tarID:Maybe Int           -- id of target
    --, allyID:Maybe Int        -- id of an ally?
    , brain:Brain               -- brain
    }

initControls : ControlState
initControls = 
    { x=0 , y=0              -- turn left/right and thrust/retro
    , fireT=False 
    , fireB=False
    , tarID=Nothing 
    , brain=Attacker
    }    

-- the state of the vessel
type VesselState = VesselActive
                 | VesselInactive
                 | VesselDead 
                 | VesselJumped

-- determines how a vessel chooses behaviors
type Brain = Player 
           | Attacker 
           | Runner 



-- this is some quantity that has a current and maxiumum and regenerates
-- at some rate.  for example: shields, blinks, beam and torpedo energy.
type alias Regenerator a = 
    {a| cur:Float                   -- current amount of energy
    , max:Float                     -- maximum amount of energy
    , reg:Float                     -- regeneration rate (nrg/sec)
    }

stepReg : Time.Time -> Regenerator a -> Regenerator a
stepReg t reg = {reg| cur <- reg.cur + reg.reg*t |> min reg.max }    

type alias Shields =
    Regenerator 
    { def:Float                     -- deflection: amt of dmg ignored/hit
    }

playerShields : Shields 
playerShields =
    { def = 5 } 
    |> Regenerator 150 150 5

pirateShields : Shields
pirateShields =
    { def = 2 } 
    |> Regenerator 60 60 2

transportShields : Shields
transportShields = 
    { def = 3 } 
    |> Regenerator 70 70 2

stepShields : Time.Time -> Vessel -> Vessel
stepShields t vessel =
    {vessel| shields <- vessel.shields |> stepReg t }

applyDmg : Float -> Vessel -> Vessel
applyDmg dmg vessel = 
    let 
    shields = vessel.shields
    harm = dmg - shields.def |> max 0 
    shields' = {shields| cur <- shields.cur - harm }
    in
    {vessel| shields <- shields' }

-- this is something that activates w a cost. it may have a delay 
-- bt successive activations in addition to the cost.
type alias Activator a = 
    {a| del:Float               -- delay in sec bt activations
    , rel:Float                 -- the time to reload the next activation
    , cost:Float                -- cost in nrg / activation
    }

stepAct : Time.Time -> Activator a -> Activator a
stepAct t act = {act| rel <- act.rel - t |> max 0 }

type alias ActReg a =
    Regenerator (Activator a)    

stepActReg : Time.Time -> ActReg a -> ActReg a
stepActReg t ar = stepReg t ar |> stepAct t  

canActivate : ActReg a -> Bool
canActivate ar =
    if | ar.rel > 0 -> False  
       | ar.cost > ar.cur -> False
       | otherwise -> True 

activate : ActReg a -> ActReg a
activate ar =
    {ar| rel <- ar.del , cur <- ar.cur - ar.cost }    

type alias Blinks =
    ActReg {dist:Float}

playerBlinks : Blinks
playerBlinks =
    {dist=40} 
    |> Regenerator 2 2 0.5
    |> Activator 1 1 1

pirateBlinks : Blinks
pirateBlinks =
    {dist=40} 
    |> Regenerator 1 1 0.15
    |> Activator 1 1 1

transportBlinks : Blinks
transportBlinks = 
    {dist=40}
    |> Regenerator 1 1 0.1
    |> Activator 1 1 1

stepBlinks : Time.Time -> Vessel -> Vessel
stepBlinks t vessel = 
    {vessel| blinks <- vessel.blinks |> stepActReg t }

applyBlink : Vessel -> Vessel 
applyBlink vessel = 
    let
    vec = quickRange (vessel.x * vessel.y * vessel.a) 
          |> (*) pi |> getVec 
    (x',y') = ( vessel.x + vec.x * vessel.blinks.dist
              , vessel.y + vec.y * vessel.blinks.dist )
    (flash1,flash2) = ( createFlash (vessel.x , vessel.y) vessel.r
                      , createFlash (x',y') vessel.r )
    children = vessel.children
    children' = {children| flashes <- flash1 :: flash2 :: children.flashes}
    in
    {vessel| x<-x', y<-y', blinks <- vessel.blinks |> activate
            , children <- children' }

type alias Flares =
    ActReg {number:Int}

playerFlares : Flares
playerFlares = 
    {number=1}
    |> Regenerator 2 2 0.5
    |> Activator 1 1 1

noFlares : Flares
noFlares =
    {number=0}
    |> Regenerator -1 -1 0
    |> Activator 1 1 1

transportFlares : Flares
transportFlares = 
    {number=1}
    |> Regenerator 2 2 0.5
    |> Activator 1 1 1


type alias TorpWpn =
    ActReg { dmg:Float              -- how much damage the torpedo does
           , life:Float             -- how long the torpedo lasts (sec)
           , vel:Float              -- the torpedo's muzzle speed
           , acc:Float              -- the torpedo's acceleration
           }

noTpWpn : TorpWpn
noTpWpn =
    {dmg=0,life=0,vel=0,acc=0}
    |> Regenerator 0 0 0
    |> Activator 1 1 1

playerTpWpn : TorpWpn               
playerTpWpn = 
    { dmg = 30 , life = 1.0 , vel = 100 , acc = 800 }
    |> Regenerator 5 5 1
    |> Activator 0.3 0.3 1

pirateTpWpn : TorpWpn
pirateTpWpn =
    { dmg = 30 , life = 0.8 , vel = 100 , acc = 800 }
    |> Regenerator 1 1 1
    |> Activator 1 1 1

stepTorpWpn : Time.Time -> Vessel -> Vessel
stepTorpWpn t vessel = 
    {vessel| torp <- vessel.torp |> stepActReg t }

type alias BeamWpn =
    ActReg { dmg:Float      -- how much damage it does
           , arc:Float      -- max angle to fire (radians)
           , range:Float    -- max distance to fire
           }

-- what you give a vessel that can't shoot beams
-- or should all vessels have Maybe BeamWpn instead?
noBmWpn : BeamWpn
noBmWpn =
    {dmg=0,arc=0,range=0}
    |> Regenerator 0 0 0    -- this should prevent it from ever firing
    |> Activator 1 1 1

playerBmWpn : BeamWpn
playerBmWpn =
    {dmg = 10 , arc= pi/2 , range = 200}
    |> Regenerator 5 5 1
    |> Activator 0.3 0.3 1

transportBmWpn : BeamWpn
transportBmWpn = 
    {dmg = 10 , arc = pi , range = 150}
    |> Regenerator 10 10 1
    |> Activator 1.5 1.5 3    

stepBeamWpn : Time.Time -> Vessel -> Vessel
stepBeamWpn t vessel = 
    {vessel| beam <- vessel.beam |> stepActReg t }


-- a homing missile
type alias Torpedo = Object
    { age:Float
    }

torpMaxAge = 0.9
torpSpeed = 200
torpAcc = 500
torpDrag = 0.9
torpReload = 1
torpDamage = 30
torpDetRadius = 12

-- a beam
type alias Beam =
    { start : (Float,Float)
    , end : (Float,Float)
    , age : Float 
    }

beamMaxAge = 0.10              -- how long a beam effect is visible

-- an explosion
type alias Boom = Object
    { age: Float }

boomMaxAge = 0.15              -- explosions last this long
boomGrowthRate = 120            -- explosions grow this fast 
boomInitRadius = 12             -- explosions start this big

-- an FTL flash
type alias Flash = Object 
    { age : Float }

flashMaxAge = 0.3              -- flashes last this long
flashShrinkRate = 20            -- flashes shrink this fast 
flashInitRadius = 20             -- flashes start this big






-- UPDATE

-- useful functions:

-- get me a probility stat
quickProb : Float -> Float 
quickProb float = 
    floor float |> Random.initialSeed |>
    Random.generate (Random.float 0 1) |> fst      

-- like quickProb but bt -1 and 1
quickRange : Float -> Float
quickRange float = 
    floor float |> Random.initialSeed |>
    Random.generate (Random.float -1 1) |> fst    

-- given an id, get the vessel out of the game's dict, MAYBE
getVessel : Game -> Int -> Maybe Vessel
getVessel game id = Dict.get id game.vessels 

-- given a maybe id, maybe get a vessel out of the game's dict
maybeGetVessel : Game -> Maybe Int -> Maybe Vessel
maybeGetVessel game maybeID = 
    case maybeID of
        Nothing -> Nothing
        Just id -> getVessel game id 


-- are these two vessels hostile?
isHostile : Vessel -> Vessel -> Bool
isHostile ves1 ves2 =
    case Dict.get ves1.faction hostilityDict of
    Nothing -> False
    Just dict -> 
        case Dict.get ves2.faction dict of
        Nothing -> False
        Just x -> x

-- get the nearest hostile to the vessel given
-- right now it just gets A hostile, not the nearest!!
getNearestHostile : Game -> Vessel -> Maybe Vessel
getNearestHostile game vessel =
    let
    vessels = Dict.values game.vessels 
    hostiles = List.filter (isHostile vessel) vessels 
    sorted = List.sortBy (distance vessel) hostiles
    in
    case sorted of 
        [] -> Nothing
        x::xs -> Just x

-- wrap angles to +pi -pi
wrapAngle : Float -> Float
wrapAngle a =
    if | a > pi -> a - 2*pi
       | a < -pi -> a + 2*pi
       | otherwise -> a    

-- move and rotate object for time t
moveObj : Time.Time -> Object a -> Object a
moveObj t obj =                    
    { obj   | x <- obj.x + t*obj.vx
            , y <- obj.y + t*obj.vy
            }

-- slow the object by its drag
dragObj : Time.Time -> Object a -> Object a
dragObj t obj =
    let damp = 1 - obj.drag*t
    in { obj | vx <- obj.vx * damp , vy <- obj.vy * damp }

-- accelerate object, obj, by some acceleration, acc, for some time, t
accObj : Time.Time -> Float -> Object a -> Object a
accObj t acc obj =
    let vec = getVec obj.a 
        vxDel = vec.x*t*acc 
        vyDel = vec.y*t*acc 
    in { obj | vx <- obj.vx + vxDel , vy <- obj.vy + vyDel }

-- rotate object by an angular velocity for some time
rotObj : Time.Time -> Float -> Object a -> Object a 
rotObj t va obj = 
    { obj | a <- obj.a + va * t |> wrapAngle }

-- distance bt two objects
distance : Object a -> Object b -> Float
distance obj1 obj2 = 
    (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2 |> sqrt 

-- are these two objects colliding?
isColliding : Object a -> Object b -> Bool
isColliding obj1 obj2 =             
    (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2 
    <= (obj1.r + obj2.r)^2            

-- the angle object 1 needs to turn to, to face object 2
faceObj : Object a -> Object b -> Float
faceObj obj1 obj2 =
    atan2 (obj2.y - obj1.y) (obj2.x-obj1.x)

-- given a vessel and its target, should the vessel turn left or right?
turnTowards : Object a -> Object b -> Float
turnTowards obj1 obj2 =
    if ( (faceObj obj1 obj2) - obj1.a |> wrapAngle ) > 0
    then 1 else -1

-- get unit vector for an angle
-- angle should already be in elm's internal unit (radians)
getVec : Float -> {x:Float,y:Float}
getVec ang = { x= cos ang , y = sin ang }

-- get angle for a vector
-- the angle will be in radians bt pi and -pi
getAng : {x:Float,y:Float} -> Float
getAng {x,y} = atan2 y x 


-- the main update functions
updateGame : Input -> Game -> Game
updateGame input game =
    game
    |> updateVesselsControls input          -- update control states
    |> updateVessels input                  -- update the vessels
    |> updateEphemera input                 -- update old ephemera
    |> updateTorps input                    -- update the torpedoes
    |> fireTorps input                      -- launch new torpedoes
    |> fireBeams input                      -- fire new beams
    |> collectEphemera                      -- collect new ephemera 
    |> updateCamera                         -- update the camera position
    |> cullVessels                          -- remove dead vessels
    |> timerSpawns input                    -- maybe spawn stuff

updateVesselsControls : Input -> Game -> Game
updateVesselsControls input game = 
    let
    vessels' = Dict.map (updateControls input game) game.vessels
    in
    {game| vessels <- vessels' }


updateControls : Input -> Game -> Int -> Vessel -> Vessel
updateControls input game id vessel =
    case vessel.controls.brain of
        Player -> updatePlayerControls input game vessel
        Attacker -> updateAttackerControls input game vessel
        Runner -> updateRunnerControls input game vessel


updatePlayerControls : Input -> Game -> Vessel -> Vessel
updatePlayerControls input game vessel = 
    let
    tarVessel = 
        if input.rKey == True then getNearestHostile game vessel else
        ensureTarget game vessel vessel.controls.tarID
    x = input.arrows.x |> toFloat 
    y = input.arrows.y |> toFloat
    (fireT,fireB,tarID) = case tarVessel of
        Nothing -> (False,False,Nothing)
        Just tar -> (input.shift,input.space,Just tar.id)
    controls = vessel.controls 
    controls' = {controls| x<- -x , y<-y 
                , fireT <- fireT , fireB <- fireB
                , tarID <- tarID
                }
    in
    {vessel| controls <- controls' }

updateAttackerControls : Input -> Game -> Vessel -> Vessel
updateAttackerControls input game vessel = 
    let
    tarVessel = ensureTarget game vessel vessel.controls.tarID
    (fireT,fireB,x,y,tarID) = 
        case tarVessel of
            Nothing -> (False,False,0,0,Nothing)
            Just tar -> ( True,True
                        , turnTowards vessel tar,1
                        , Just tar.id)
    controls = vessel.controls 
    controls' = {controls| x<- x , y<-y 
                , fireT <- fireT , fireB <- fireB
                , tarID <- tarID
                }
    in
    {vessel| controls <- controls' }

updateRunnerControls : Input -> Game -> Vessel -> Vessel
updateRunnerControls input game vessel = 
    let
    tarVessel = ensureTarget game vessel vessel.controls.tarID
    (fireT,fireB,x,y,tarID) = 
        case tarVessel of
            Nothing -> (False,False,0,1,Nothing)
            Just tar -> ( True,True
                        , turnTowards vessel tar,1
                        , Just tar.id)
    controls = vessel.controls 
    controls' = {controls| x<- -x , y<-y 
                , fireT <- fireT , fireB <- fireB
                , tarID <- tarID
                }
    in
    {vessel| controls <- controls' }

ensureTarget : Game -> Vessel -> Maybe Int -> Maybe Vessel
ensureTarget game vessel target = 
    case target of 
        Nothing -> getNearestHostile game vessel
        Just id -> getVessel game id 



updateVessels : Input -> Game -> Game
updateVessels input game = 
    {game| vessels <- Dict.map (updateVessel input game) game.vessels}

updateVessel : Input -> Game -> Int -> Vessel -> Vessel
updateVessel input game id vessel =
    let
    t = input.tick
    controls = vessel.controls 
    acc = if | controls.y > 0 -> vessel.thrust 
             | controls.y < 0 -> vessel.retro
             | True -> 0
    va = controls.x * vessel.turn 
    in
    vessel |> rotObj t va |> accObj t acc |> moveObj t |> dragObj t 
           |> stepVesselState |> stepShields t |> stepBlinks t
           |> stepBeamWpn t |> stepTorpWpn t 
           |> clearEphemera 

stepVesselState : Vessel -> Vessel
stepVesselState vessel =
    let
    state' = if | vessel.shields.cur < 0 -> VesselDead
                | otherwise -> VesselActive 
    in 
    {vessel| state <- state' }

clearEphemera : Vessel -> Vessel 
clearEphemera vessel =
    {vessel| children <- initChildren }


updateTorps : Input -> Game -> Game
updateTorps input game = 
    let
    vessels' = Dict.map (updateTorpSeekingVessel input.tick) game.vessels  
    in 
    {game| vessels <- vessels' 
         }

updateTorpSeekingVessel : Time.Time -> Int -> Vessel -> Vessel
updateTorpSeekingVessel t id vessel =
    let
    children = vessel.children
    seekedBy' = List.filterMap (moveTorp t vessel) vessel.seekedBy
    (seekedBy'',dmg,booms') =
        List.foldl (collideTorp vessel) ([],0,[]) seekedBy' 
    children' = {children| booms <- children.booms ++ booms' }
    vessel' = {vessel| children <- children' , seekedBy<-seekedBy''}
    in 
    if | dmg == 0 -> vessel' 
       | canActivate vessel'.blinks -> vessel' |> applyBlink
       | otherwise -> vessel' |> applyDmg dmg 



moveTorp : Time.Time -> Vessel -> Torpedo -> Maybe Torpedo
moveTorp t vessel torp = 
    let 
    age' = torp.age + t
    in
    if age' > torpMaxAge then Nothing else
    let
    a' = faceObj torp vessel 
    in
    { torp | age <- age' , a <- a'} |> accObj t torpAcc |> moveObj t 
    |> dragObj t |> Just


collideTorp : Vessel -> Torpedo -> (List Torpedo , Float , List Boom) 
    -> (List Torpedo , Float , List Boom)
collideTorp vessel torp ( list , damage , booms ) = 
    if isColliding vessel torp 
    then ( list ,  torpDamage + damage , (createBoom torp) :: booms ) 
    else ( torp :: list , damage , booms )

blinkJump : (Float,Float) -> (Float,Float)
blinkJump (x,y) = 
    let
    vec = (quickRange (x*y*4))*pi |> getVec
    in
    (x+vec.x*80,y+vec.y*80)


createFlash : (Float,Float) -> Float -> Flash
createFlash (x,y) r =
    {x=x,y=y,r=r+2,a=0,vx=0,vy=0,drag=0,age=0}

createBoom : Object a -> Boom
createBoom obj = 
    { x = obj.x , y = obj.y , a=0 , vx=0 , vy=0
    , r = boomInitRadius , drag = 0 , age=0}

    


fireTorps : Input -> Game -> Game
fireTorps input game =
    let
    (shooters, newTorps) = 
        Dict.values game.vessels 
        |> List.filterMap (fireTorp game) 
        |> List.unzip 
    vessels' = Dict.union (Dict.fromList shooters) game.vessels
    vessels'' = List.foldl attachTorpedoes vessels' newTorps
    in
    {game| vessels <- vessels'' }       

fireTorp : Game -> Vessel -> Maybe ((Int,Vessel),(Int,Torpedo))
fireTorp game vessel = 
    let
    torpWpn = vessel.torp
    canFire vessel = 
        if | vessel.controls.fireT |> not -> False
           | canActivate torpWpn -> True
           | otherwise ->  False
    reallyFireTorp vessel target =
        ( (vessel.id , {vessel| torp <- activate torpWpn } )
        , ( target.id , torpSpawn vessel )
        )
    in
    if canFire vessel |> not then Nothing else 
    case maybeGetVessel game vessel.controls.tarID of
        Nothing -> Nothing
        Just target -> reallyFireTorp vessel target |> Just       

torpSpawn : Vessel -> Torpedo
torpSpawn shooter =
    let
    vec = getVec shooter.a  
    in
    { x = shooter.x , y = shooter.y 
    , vx = shooter.vx + vec.x * torpSpeed
    , vy = shooter.vy + vec.y * torpSpeed
    , a = 0 , r = torpDetRadius , drag = torpDrag
    , age = 0}

attachTorpedoes : (Int , Torpedo) 
                -> Dict.Dict Int Vessel 
                -> Dict.Dict Int Vessel 
attachTorpedoes (id , torp) vessels = 
    case Dict.get id vessels of
        Nothing -> vessels
        Just vessel ->
            let vessel' = {vessel| seekedBy <- torp :: vessel.seekedBy }
            in Dict.insert id vessel' vessels



fireBeams : Input -> Game -> Game
fireBeams input game = 
    let 
    (shooters,beams) = Dict.values game.vessels 
               |> List.filterMap (fireBeam game) |> List.unzip
    vessels' = Dict.union (Dict.fromList shooters) game.vessels
    vessels'' = List.foldl attachBeams vessels' beams 
    in 
    {game| vessels <- vessels'' }

fireBeam : Game -> Vessel -> Maybe ((Int,Vessel),(Int,Float,(Beam,Beam)))
fireBeam game vessel = 
    let
    beamWpn = vessel.beam
    canFire vessel = 
        if | vessel.controls.fireB |> not -> False
           | canActivate beamWpn -> True
           | otherwise ->  False
    canHit vessel target = 
        if | distance vessel target > beamWpn.range -> False
           | ((faceObj vessel target) - vessel.a |> abs) > beamWpn.arc -> False
           | otherwise -> True         
    reallyFireBeam vessel target = 
        ( (vessel.id , {vessel| beam <- activate beamWpn } )
        , ( target.id , beamWpn.dmg ,createBeam vessel target)
        )
    in
    if canFire vessel |> not then Nothing else
    case maybeGetVessel game vessel.controls.tarID of
        Nothing -> Nothing
        Just target -> if canHit vessel target |> not then Nothing else
            reallyFireBeam vessel target |> Just     

createBeam : Object a -> Object b -> (Beam,Beam)
createBeam obj1 obj2 =
    let
    hit = {start=(obj1.x,obj1.y),end=(obj2.x,obj2.y),age=0}
    miss = { start = (obj1.x,obj1.y)
           , end = ( obj1.x + 3 * (obj2.x-obj1.x) 
                   , obj1.y + 3 * (obj2.y-obj1.y) ) 
           , age = 0}
    in
    (hit,miss)

attachBeams : (Int , Float , (Beam,Beam))
                -> Dict.Dict Int Vessel
                -> Dict.Dict Int Vessel 
attachBeams (id , dmg , (hit,miss)) vessels = 
    case Dict.get id vessels of
    Nothing -> vessels
    Just vessel -> 
        let 
        children = vessel.children 
        vessel' = 
            if canActivate vessel.blinks 
            then 
                let
                children' = 
                    {children| beams <- miss :: children.beams }
                in
                {vessel| children <- children'} |> applyBlink
            else
                let
                children' = 
                    {children| beams <- hit :: children.beams 
                    , booms <- (createBoom vessel) :: children.booms }
                in
                {vessel| children <- children'} |> applyDmg dmg 
        in Dict.insert id vessel' vessels



updateEphemera : Input -> Game -> Game
updateEphemera input game =  
    let 
    t = input.tick
    eph = game.ephemera
    beams' = List.filterMap updateBeam eph.beams
    updateBeam beam = if beam.age > beamMaxAge then Nothing else
        {beam| age <- beam.age+t } |> Just 
    booms' = List.filterMap updateBoom eph.booms
    updateBoom boom = if boom.age > boomMaxAge then Nothing else
        {boom| age <- boom.age+t , r <- boom.r + t*boomGrowthRate} |> Just
    flashes' = List.filterMap updateFlash eph.flashes
    updateFlash flash = if flash.age > flashMaxAge then Nothing else
        {flash| age <- flash.age+t , r <- flash.r - t*flashShrinkRate}|> Just
    eph' = {eph| beams <- beams' , booms <- booms' , flashes <- flashes' }
    in 
    {game| ephemera <- eph' }



collectEphemera : Game -> Game
collectEphemera game = 
    let 
    ephemera = game.ephemera
    children = Dict.values game.vessels |> List.map .children
    newBooms = List.concatMap .booms children
    newFlashes = List.concatMap .flashes children
    newBeams = List.concatMap .beams children
    ephemera' = {ephemera| booms <- ephemera.booms ++ newBooms 
                , flashes <- ephemera.flashes ++ newFlashes
                , beams <- ephemera.beams ++ newBeams 
                }
    in
    {game| ephemera <- ephemera' }


updateCamera : Game -> Game
updateCamera game = 
    let
    camera' = case getVessel game game.playerID of
                Nothing -> game.camera
                Just player -> (player.x , player.y)
    in 
    {game| camera <- camera'}



cullVessels : Game -> Game
cullVessels game =
    let
    f id vessel = vessel.state == VesselDead 
    (culled,vessels') = Dict.partition f game.vessels
    newBooms = Dict.values culled |> List.map createBoom 
              -- |> List.append game.booms
    eph = game.ephemera
    eph' = {eph| booms <- newBooms ++ eph.booms }
    in 
    {game| vessels <- vessels' , ephemera <- eph' }



timerSpawns : Input -> Game -> Game
timerSpawns input game = 
    if game.time == input.time then game else
    let 
    (nextID',newVessels) = maybeSpawnTransport game game.nextID []  
    (nextID'',newVessels') = maybeSpawnPirate game nextID' newVessels 
    in 
    {game| time <- input.time 
        , nextID <- nextID''
        , vessels <- Dict.fromList newVessels' |> Dict.union game.vessels}

maybeSpawnTransport : Game -> Int -> List (Int,Vessel) 
            -> (Int,List (Int,Vessel))
maybeSpawnTransport game nextID newVessels = 
    let
    prob = quickProb (game.time * 7)
    in
    if prob > 0.5
    then (nextID,newVessels)
    else let 
    nextID' = nextID+1
    newVessel = newTransport
                ( quickRange (game.time * 5)
                , quickRange (game.time * 3)
                ) 
                nextID'
    in
    (nextID',(nextID',newVessel)::newVessels)            

maybeSpawnPirate : Game -> Int -> List (Int,Vessel) 
            -> (Int,List (Int,Vessel))
maybeSpawnPirate game nextID newVessels = 
    let
    prob = quickProb (game.time * 17)
    in
    if prob > 0.7
    then (nextID,newVessels)
    else let 
    nextID' = nextID+1
    newVessel = newPirate 
                ( quickRange (game.time * 5)
                , quickRange (game.time * 3)
                ) 
                nextID'
    in
    (nextID',(nextID',newVessel)::newVessels)  











-- VIEW

view : (Int,Int) -> Game -> Element.Element
view (w,h) game =
    Element.container w h Element.middle <|
    Collage.collage gameW gameH
    [ viewSky                               -- draw the black bg
    , viewStarsImg game                     -- draw the starfield
    , viewPlanet game                       -- draw the planet
    , viewTorpedoes game                    -- draw torps
    , viewBeams game                        -- draw beams
    , viewVessels game                      -- draw vessels
    , viewFlashes game                      -- draw FTL flashes
    , viewBooms game                        -- draw explosions
    , viewText game                         -- draw data text
    , viewPanes game                    
    ]

viewSky : Collage.Form
viewSky = Collage.rect gameW gameH |> Collage.filled Color.black

viewStarsImg game =
    let
    (cx,cy) = game.camera
    img = "images/test2.png"
    d = gameW * 5
    (bx,by) = (cx/d,cy/d)
    tiles = 
        [ ( floor bx |> toFloat , floor by |> toFloat ) 
        , ( floor bx |> toFloat , ceiling by |> toFloat )
        , ( ceiling bx |> toFloat , floor by |> toFloat ) 
        , ( ceiling bx |> toFloat , ceiling by |> toFloat )
        ]
    drawTile (x,y) = Element.image d d img |> Collage.toForm 
                    |> Collage.move (d*x-cx,d*y-cy)
    in
    List.map drawTile tiles |> Collage.group 

viewPlanet game = 
    let
    (cx,cy) = game.camera
    img = "images/3D_Neptune2.png"
    d = 100
    in
    Element.image d d img |> Collage.toForm |> Collage.move (-cx,-cy)

viewBeams game = 
    let
    (cx,cy) = game.camera
    beams = Dict.values game.vessels |> List.map .children 
            |> List.concatMap .beams
    drawBeam beam = 
        let
        ageRatio = beam.age / beamMaxAge
        (color,width) = if | ageRatio > 0.75 -> (Color.darkBlue , 1)
                           | ageRatio > 0.5 -> (Color.lightBlue , 2)
                           | ageRatio > 0.25 -> (Color.white , 3)
                           | otherwise -> (Color.lightBlue , 2) 
        lineStyle = Collage.solid color  
        in
        Collage.segment beam.start beam.end 
                    |> Collage.traced {lineStyle| width <- width}
    in
    game.ephemera.beams |> List.map drawBeam |> Collage.group 
    |> Collage.move (-cx,-cy)


viewTorpedoes game =
    let 
    (cx,cy) = game.camera
    torpAll = Dict.values game.vessels |> List.concatMap .seekedBy  
    torpDraw torp =  
        let
        ageMod = (torp.age * 10 |> floor) % 4
        (color,radius) 
            = if | ageMod == 0 -> (Color.white , 2 )
                 | ageMod == 1 -> (Color.lightBlue , 2.5 )
                 | ageMod == 2 -> (Color.darkBlue , 3 )
                 | ageMod == 3 -> (Color.lightBlue , 2.5 )
        in
        Collage.circle radius
        |> Collage.filled color
        |> Collage.move (torp.x - cx , torp.y - cy)
    in 
    torpAll |> List.map torpDraw |> Collage.group


viewVessels game =
    Dict.values game.vessels 
    |> List.map (viewVessel game)
    |> Collage.group

viewVessel game vessel = 
    let
    (cx,cy) = game.camera
    x = vessel.x - cx
    y = vessel.y - cy
    d = floor (vessel.r * 2)
    in
    Element.image d d vessel.image |> Collage.toForm
    |> Collage.rotate (vessel.a - degrees 90)
    |> Collage.move (x,y) 



viewBooms game = 
    let 
    (cx,cy) = game.camera
    boomDraw boom = 
        let 
        ageRatio = boom.age / boomMaxAge
        color = if | ageRatio < 0.2 -> Color.lightBlue
                   | ageRatio < 0.5 -> Color.white 
                   | ageRatio < 0.8 -> Color.lightRed 
                   | otherwise -> Color.darkRed 
        in 
        Collage.circle boom.r |> Collage.filled color 
        |> Collage.move ( boom.x - cx , boom.y - cy )
    in
    game.ephemera.booms |> List.map boomDraw |> Collage.group


viewFlashes game = 
    let 
    (cx,cy) = game.camera
    flashDraw flash =
        let
        ageRatio = flash.age / flashMaxAge
        color = if | ageRatio < 0.4 -> Color.white
                   | ageRatio < 0.8 -> Color.lightBlue
                   | otherwise -> Color.darkBlue
        in
        Collage.circle flash.r |> Collage.filled color
        |> Collage.move ( flash.x - cx , flash.y - cy )
    in
    game.ephemera.flashes |> List.map flashDraw |> Collage.group

-- given a string message, make an element of it    
drawText : String -> Element.Element
drawText message =
    Element.centered 
    ( Text.fromString message |> Text.color Color.lightBlue) 

viewText game = 
    case getVessel game game.playerID of
    Just player -> Element.empty |> Collage.toForm
    Nothing -> 
        " You have died. So it goes. " |> drawText     
        |> Element.color Color.darkBlue |> Element.opacity 0.8
        |> Collage.toForm 

viewPanes game = 
    case getVessel game game.playerID of
    Nothing -> Element.empty |> Collage.toForm 
    Just player -> 
    let
    pxy = (150-halfW,halfH-10)
    txy = (150-halfW,10-halfH)
    playerPane = drawVesselPane player pxy
    targetPane =
        case maybeGetVessel game player.controls.tarID of
        Nothing -> Element.empty |> Collage.toForm
        Just target -> drawVesselPane target txy
    in
    [ playerPane , targetPane ] |> Collage.group


drawVesselPane vessel xy =
    [ Element.image 20 20 vessel.image
    , drawText (" Shields: " ++ toString (floor vessel.shields.cur))
    , drawText (" Blinks: " ++ toString (floor vessel.blinks.cur))
    , drawText (" Beams: " ++ toString (floor vessel.beam.cur))
    , drawText (" Torps: " ++ toString (floor vessel.torp.cur))
    ] 
    |> Element.flow Element.right 
    |> Element.color Color.darkBlue 
    |> Element.opacity 0.8 
    |> Collage.toForm 
    |> Collage.move xy






