module Asteroids2 where

{-| 
Doc goes here 

nice milky way pic:
    http://apod.nasa.gov/apod/image/0510/allskymilkyway_brunier_big.jpg
    ^this is an Astronomy pic of the day, so usable!

and:
    http://www.sidleach.com/summer_milky_way.jpg
    ^this was taken by Sid Leach in 2004, unsure of C

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
import Mouse
import List
import Maybe 
import Random

import Debug


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

tick : Signal Time.Time
tick = Time.inSeconds <~ Time.fps 35



-- MODEL

type alias Input = 
    { tick : Time.Time 
    , arrows : {x:Int,y:Int}
    , space : Bool
    , shift : Bool
    , ctrl : Bool
    , time : Time.Time
    }

-- constants / magic numbers

-- the size of the game board (px)
(halfW,halfH) = (400,300)
(gameW,gameH) = (halfW*2,halfH*2)
tileL = (max halfW halfH)*2

type alias Game = 
    { player : Vessel               -- The player's vessel
    , pirate : Vessel               -- pirate vessel
    , camera : (Float,Float)        -- The coordinates of the camera
    , stars : List (Float,Float)
    , booms : List Boom             -- explosions
    }

startGame : Game
startGame =
    { player = initPlayer
    , pirate = initPirate
    , camera = (0,0)
    , stars = createStars (0,0)
    , booms = []
    }
{-
type alias Star =                           -- a background star
    { x:Float , y:Float , z:Float           -- position and depth
    , color:Color.Color , r:Float }         -- color and radius
    -}

type alias Object a =                -- a space object
    { a | x:Float , y:Float          -- position (px)
        , vx: Float , vy:Float       -- linear velocity (px/sec)
        , a: Float                   -- angle (rad) 
        --, va:Float                   -- angvel (rad/sec)
        , r:Float                    -- radius(px)
        , drag:Float                 -- drag (ratio lost /sec)
        }

type alias HasDrive a =
    { a | thrust:Float
        , retro:Float
        , turn:Float
        }

type alias HasOffense a =
    { a | torpReload:Float
        , beamReload:Float
        }

type alias HasDefense a =
    { a | shields:Float 
        , shieldMax:Float
        , seekedBy:List Torpedo
        --, blinks:Float 
        --, blinkMax:Float 
        }    

-- space vessels
type alias Vessel = Object 
    (HasOffense (HasDefense (HasDrive                  
    ({ image:String                          -- image name string
    }))))
    {-
    { shield:Float                          -- shields
    , thrust:Float                          -- forward accel (px/sec/sec)
    , retro:Float                           -- reverse accel (px/sec/sec)
    , turn:Float                            -- turn rate (deg/sec)
    , image:String                          
    }   -}                     

initPlayer : Vessel
initPlayer =
    { x=0,y=0,a=degrees 90, vx=100, vy=100
    , r=10, drag=0.3 
    , thrust=200, retro=-50, turn= degrees 180
    , torpReload=0 , beamReload=0
    , shields=100 , shieldMax=100 , seekedBy=[]
    , image="images/player2.png" }

initPirate : Vessel
initPirate = 
    { x=0,y=0,a=degrees -90, vx=-100, vy=-100
    , r=8, drag=0.3
    , thrust=200, retro=-50, turn= degrees 180
    , torpReload=0 , beamReload=0
    , shields=100 , shieldMax=100 , seekedBy=[]
    , image="images/pirate1.png" }

initTransport : Vessel
initTransport = 
    { x=0 , y=0 , a= degrees 180 , vx= 100 , vy=100
    , r=10 , drag=0.3
    , thrust=100 , retro=-50 , turn = degrees 90
    , torpReload=0 , beamReload=0
    , shields=200 , shieldMax=200 , seekedBy=[]
    , image="images/civ1.png"
    }

-- a homing missile
type alias Torpedo = Object
    { age:Float
    }

torpMaxAge = 1.8
torpSpeed = 200
torpAcc = 1000
torpDrag = 0.1
torpReload = 1
torpDamage = 30
torpDetRadius = 12

-- an explosion
type alias Boom = Object
    { age: Float }

boomMaxAge = 0.15              -- explosions last this long
boomGrowthRate = 120            -- explosions grow this fast 
boomInitRadius = 12             -- explosions start this big

createStars : (Int,Int) -> List (Float,Float)
createStars xy =
    let
    makePt = Random.pair (Random.float -tileL tileL) 
                         (Random.float -tileL tileL) 
    makePts = Random.list 100 makePt 
    findTile (x,y) = 0
    seed = Random.initialSeed (findTile xy)
    (stars,_) = Random.generate makePts seed
    in
    stars

-- a beam
type alias Beam =
    { start : (Float,Float)
    , end : (Float,Float)
    , age : Float 
    }


-- UPDATE

-- useful functions:

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

-- rotate object by angular velocity for some time
rotObj : Time.Time -> Float -> Object a -> Object a 
rotObj t va obj = 
    { obj | a <- obj.a + va * t |> wrapAngle }

-- are these two objects colliding?
isColliding : Object a -> Object b -> Bool
isColliding obj1 obj2 =             
    (obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2 
    <= (obj1.r + obj2.r)^2            

-- the angle object 1 needs to turn to, to face object 2
faceObj : Object a -> Object b -> Float
faceObj obj1 obj2 =
    atan2 (obj2.y - obj1.y) (obj2.x-obj1.x)

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
    |> updatePlayer input                   -- update the player
    |> updatePirate input                   -- update the pirate
    |> updateBooms input                    -- update explosions
    |> updateTorps input                    -- update the torpedoes
    |> updatePlayerAttack input             -- maybe fire player weapons
    |> updatePirateAttack input             -- maybe fire pirate weapons
    |> updateCamera                         -- update the camera position


updatePlayer : Input -> Game -> Game
updatePlayer input game =
    let
    t = input.tick
    player = game.player
    acc = case input.arrows.y of
            1 -> player.thrust
            (-1) -> player.retro
            0 -> 0
    va = case input.arrows.x of
            1 -> -player.turn 
            (-1) -> player.turn 
            0 -> 0
    player' = player |> rotObj t va |> accObj t acc |> moveObj t |> dragObj t
    in
    { game  | player <- player'
            }

updatePirate : Input -> Game -> Game
updatePirate input game =
    let
    t = input.tick
    pirate = game.pirate
    ang2tar = (faceObj pirate game.player) - pirate.a |> wrapAngle
    acc = pirate.thrust
    va = if ang2tar > 0 then pirate.turn else -pirate.turn
    pirate' = pirate |> rotObj t va |> accObj t acc |> moveObj t |> dragObj t
    in
    {game| pirate <- pirate' }

updateBooms : Input -> Game -> Game
updateBooms input game = 
    let
    t = input.tick
    booms' = List.filterMap updateBoom game.booms
    updateBoom boom = if boom.age > boomMaxAge then Nothing else
        {boom| age <- boom.age+t , r <- boom.r + t*boomGrowthRate} |> Just
    in
    {game| booms <- booms'}

updateTorps : Input -> Game -> Game
updateTorps input game = 
    let 
    t = input.tick
    player' = game.player |> moveTorps t
    (playerTorps,playerDmg,playerBooms) = 
        List.foldl (collideTorp player') ([],0,[]) player'.seekedBy
    player'' = {player'| seekedBy <- playerTorps}    
    pirate' = game.pirate |> moveTorps t 
    (pirateTorps,pirateDmg,pirateBooms) = 
        List.foldl (collideTorp pirate') ([],0,[]) pirate'.seekedBy
    pirate'' = {pirate'| seekedBy <- pirateTorps}
    booms' = List.concat [game.booms , playerBooms , pirateBooms] 
    in 
    { game | player <- player'' , pirate <- pirate'' , booms <- booms'}

moveTorps : Time.Time -> Vessel -> Vessel
moveTorps t vessel =  
    {vessel| seekedBy <- List.filterMap (moveTorp t vessel) vessel.seekedBy}

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

createBoom : Object a -> Boom
createBoom obj = 
    { x = obj.x , y = obj.y , a=0 , vx=0 , vy=0
    , r = boomInitRadius , drag = 0 , age=0}

updatePlayerAttack : Input -> Game -> Game
updatePlayerAttack input game = 
    let
    t = input.tick
    player = game.player
    pirate = game.pirate
    torpReload' = case player.torpReload of
        0 -> if input.shift then torpReload else 0
        x -> max 0 (x-t)
    seekedBy' = if torpReload' == torpReload |> not then pirate.seekedBy else
        (torpSpawn player pirate) :: pirate.seekedBy
    player' = 
        {player| torpReload <- torpReload' }
    pirate' =
        {pirate| seekedBy <- seekedBy' }
    in
    {game| player <- player' , pirate <- pirate' }

updatePirateAttack : Input -> Game -> Game
updatePirateAttack input game =
    let
    t = input.tick
    player = game.player
    pirate = game.pirate
    torpReload' = case pirate.torpReload of
        0 -> torpReload 
        x -> max 0 (x-t)
    seekedBy' = if torpReload' == torpReload |> not then player.seekedBy else
        (torpSpawn pirate player ) :: player.seekedBy
    player' = 
        {player| seekedBy <- seekedBy' }
    pirate' =
        {pirate| torpReload <- torpReload' }
    in
    {game| player <- player' , pirate <- pirate' }


torpSpawn : Vessel -> Vessel -> Torpedo
torpSpawn shooter target =
    let
    a = faceObj shooter target
    a' = shooter.a 
    vec = getVec a' 
    in
    { x = shooter.x , y = shooter.y 
    , vx = shooter.vx + vec.x * torpSpeed
    , vy = shooter.vy + vec.y * torpSpeed
    , a = shooter.a , r = torpDetRadius , drag = 0
    , age = 0}

updateCamera : Game -> Game
updateCamera game = 
    let
    camera' = (.x game.player , .y game.player)
    in 
    { game | camera <- camera'}









-- VIEW

view : (Int,Int) -> Game -> Element.Element
view (w,h) game =
    Element.container w h Element.middle <|
    Collage.collage gameW gameH
    [ viewSky
    , viewStarsImg game
    --, viewStars game
    , viewTorpedoes game
    , viewPirate game
    , viewPlayer game
    , viewBooms game 
    ]

viewSky : Collage.Form
viewSky = Collage.rect gameW gameH |> Collage.filled Color.black

viewStarsImg game =
    let
    (cx,cy) = game.camera
    img = "images/test2.png"
    --d = 100
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
    {-}
    [ Element.image d d img |> Collage.toForm |> Collage.move (-cx,-cy)
    , Element.image d d img |> Collage.toForm |> Collage.move (d/2-cx,d/2-cy)
    ] |> Collage.group
    -}

viewStars game = 
    let
    (cx,cy) = game.camera
    star (x,y) = Collage.circle 1 |> Collage.filled Color.white 
                |> Collage.move (x-cx,y-cy)
    --onScreen (x,y) = abs (x-cx) < halfW || abs (y-cy) < halfH 
    --maybeStar xy = if onScreen xy then star xy |> Just else Nothing         
    in
    game.stars |>
    --List.filterMap maybeStar |>
    List.map star |>
    Collage.group    

viewTorpedoes game =
    let 
    (cx,cy) = game.camera
    torpAll = List.concatMap .seekedBy [game.player , game.pirate]
    torpDraw torp =  
        let
        ageMod = (torp.age * 10 |> floor) % 3
        (color,radius) 
            = if | ageMod == 0 -> (Color.white , 2 )
                 | ageMod == 1 ->  (Color.lightBlue , 2.5 )
                 | ageMod == 2 ->  (Color.darkBlue , 3 )
        in
        Collage.circle radius
        |> Collage.filled color
        |> Collage.move (torp.x - cx , torp.y - cy)
    in 
    torpAll |> List.map torpDraw |> Collage.group


viewVessel game vessel = 
    let
    player = game.player
    (cx,cy) = game.camera
    x = vessel.x - cx
    y = vessel.y - cy
    d = floor (vessel.r * 2)
    in
    Element.image d d vessel.image |> Collage.toForm
    |> Collage.rotate (vessel.a - degrees 90)
    |> Collage.move (x,y) 

viewPlayer game = viewVessel game game.player 

viewPirate game = viewVessel game game.pirate


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
    game.booms |> List.map boomDraw |> Collage.group







