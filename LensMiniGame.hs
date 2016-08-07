{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, LambdaCase, RecordWildCards, FlexibleContexts, TupleSections #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Data.Vector ((!),(//))
import qualified Data.Vector as V

--import Control.Lens
import Lens.Micro.Platform

import Data.Maybe
import Text.Printf
import Debug.Trace

{-
  minigame
    done - shoot bullets with limited lifetime
    done - respawn player
    done - pick up health, weapon and ammo powerups
    done - respawn items
    done - touch lava that cause damage once per second since the first touch
    done - control player's acceleration instead of position, also add friction
    done - don't pickup items when the inventory is full (filter collision by entity state, i.e. they collide when they accepts the event)
    done - randomize spawn time
    done - drop inventory on death
    done - animated visual only elements (i.e. particles on collision)
    full q3 inventory
    * count deaths and kills (persistent data support)
        higher level rules:
          time limit
          frag/ctf score limit
          count score
          keep track of statistics
        idea: emit frags on deathmatch kills, containing the necessary info
              emit flag scores / events
          question: does this break state locality? (this is ad hoc, but it's ok to me)
    * teleport (target support)
    * teleport telefrag with killbox
    jump pad
    door
    button + movable

  goals:
    rule based
    compositional with reusable small components
    intuitive and easy to use
    simple and efficient operational semantics and implementation (should be easy to imagine the compilation/codegen)

  events:
    collision between entities and client

  design ideas:
    transfer function can emit events (data for higher level rules)

  rule hierarchy
    #1 - game mode
      high score tracking
      count frags / flag scores
      measure time
    #2 - action rules
      kills, teleports, collisions, etc.
-}
{-
  random missing features:
    character animations
    weapon change animation
    weapon animations
      shoot
      explosion
      weapon idle/etc
-}
{-
  interactions to handle
    item on a mover - problem: relative position
    killbox         - problem: many things in one interaction
    teleport target - problem: target = referencex
-}
{-
  quake 3 inventory
    weapons
      gauntlet
      machinegun
      shotgun
      grenade launcher
      rocket launcher
      lightning
      railgun
      plasmagun
      bfg
      grappling hook
    ammos
      for each weapon
    armors
      armor shard   5
      armor combat  50
      armor body    100
    health
      health small  5
      health        25
      health large  50
      health mega   100
    powerup
      quad    (quad damage)
      enviro  (battle suit)
      haste   (speed)
      invis   (invisibility)
      regen   (regeneration)
      flight
    holdable
      teleporter
      medkit
    team
      redflag
      blueflag
-}

type Vec2 = Vector

_x = _1
_y = _2

-- entities for game logic

data Player
  = Player
  { _pPosition    :: Vec2
  , _pFVelocity   :: Float
  , _pSVelocity   :: Float
  , _pAngle       :: Float
  , _pHealth      :: Int
  , _pAmmo        :: Int
  , _pArmor       :: Int
  , _pShootTime   :: Float
  , _pDamageTimer :: Float
  , _pName        :: String
  , _pId          :: Int
  } deriving Show

data Bullet
  = Bullet
  { _bPosition    :: Vec2
  , _bDirection   :: Vec2
  , _bDamage      :: Int
  , _bLifeTime    :: Float
  } deriving Show

data Weapon
  = Weapon
  { _wPosition    :: Vec2
  , _wDropped     :: Bool
  } deriving Show

data Ammo
  = Ammo
  { _aPosition    :: Vec2
  , _aQuantity    :: Int
  , _aDropped     :: Bool
  } deriving Show

data Armor
  = Armor
  { _rPosition    :: Vec2
  , _rQuantity    :: Int
  , _rDropped     :: Bool
  } deriving Show

data Health
  = Health
  { _hPosition   :: Vec2
  , _hQuantity   :: Int
  } deriving Show

data Spawn
  = Spawn
  { _sSpawnTime :: Float
  , _sEntity    :: Entity
  } deriving Show

data Lava
  = Lava
  { _lPosition :: Vec2
  , _lDamage   :: Int
  } deriving Show

data Teleport
  = Teleport
  { _tPosition  :: Vec2
  , _tTarget    :: String
  } deriving Show

data Target
  = Target
  { _ttPosition   :: Vec2
  , _ttTargetName :: String
  } deriving Show

data Entity
  = EPlayer   Player
  | EBullet   Bullet
  | EWeapon   Weapon
  | EAmmo     Ammo
  | EArmor    Armor
  | EHealth   Health
  | ELava     Lava
  | ETeleport Teleport
  | ETarget   Target
  | PSpawn    Spawn
  deriving Show

concat <$> mapM makeLenses [''Player, ''Bullet, ''Weapon, ''Ammo, ''Armor, ''Spawn, ''Health, ''Lava, ''Teleport, ''Target]

-- visuals for game graphics

data Particle
  = Particle
  { _vpPosition   :: Vec2
  , _vpDirection  :: Vec2
  , _vpLifeTime   :: Float
  } deriving Show

data Visual
  = VParticle Particle
  deriving Show

concat <$> mapM makeLenses [''Particle]

type Time = Float
type DTime = Float

collide :: [Entity] -> [(Int,Int)]
collide ents = x where
  ients = zip [0..] ents
  x = [ (i1,i2)
      | (i1,e1) <- ients
      , (i2,e2) <- ients
      , i1 < i2
      , (p1,r1) <- maybeToList (brush e1)
      , (p2,r2) <- maybeToList (brush e2)
      , magV (p1 - p2) < r1 + r2
      ]
  brush = \case
    EPlayer a   -> Just (a^.pPosition, 20)
    EBullet a   -> Just (a^.bPosition, 2)
    EWeapon a   -> Just (a^.wPosition, 10)
    EAmmo a     -> Just (a^.aPosition, 8)
    EArmor a    -> Just (a^.rPosition, 10)
    EHealth a   -> Just (a^.hPosition, 10)
    ELava a     -> Just (a^.lPosition, 50)
    ETeleport a -> Just (a^.tPosition, 20)
    _ -> Nothing

type EM s a = ReaderT s (StateT s (MaybeT (WriterT ([Entity],[Visual]) (Rand PureMT)))) a -- entity update monad (mutable state + collect new entities/visuals)
type VM s a = ReaderT s (StateT s (MaybeT (WriterT ([Visual]) (Rand PureMT)))) a -- visual item update monad + collect new visual items
type CM a = WriterT ([Entity],[Visual]) (Rand PureMT) a -- monad for collect new entites or visuals

die = fail "die"

respawn t f = do
  s <- get
  t' <- getRandomR (t + 5, t + 10)
  addEntities [PSpawn $ Spawn t' (f s)]
  die

--update :: Monoid w => (s -> e) -> s -> ReaderT s (StateT s (MaybeT (WriterT w (Rand PureMT)))) a -> CM (Maybe e)
update f a m = fmap f <$> runMaybeT (execStateT (runReaderT m a) a)

collect :: Monoid w => PureMT -> WriterT w (Rand PureMT) a -> ((a,w),PureMT)
collect randGen m = runIdentity $ runRandT (runWriterT m) randGen

{-
  collect collided entities
  transform entities in interaction, collect newly generated entities
  step entities, also collect generated entities
  append generated entities
-}
updateEntities :: PureMT -> Input -> [Entity] -> (PureMT,[Entity],[Visual])
updateEntities randGen input@Input{..} ents = (randGen',catMaybes (V.toList nextEnts) ++ newEnts,newVisuals) where

  entityVector :: V.Vector (Maybe Entity)
  entityVector = V.fromList $ map Just ents

  collisions :: [(Int,Int)] -- post process collisions into interaction events
  collisions = collide ents

  ((nextEnts,(newEnts,newVisuals)),randGen') = collect randGen $ do
    let go entV (i1,i2) = case (entV ! i1, entV ! i2) of
          (Just e1, Just e2) -> interact True (e1,e2) >>= \(e1',e2') -> return (entV // [(i1,e1'),(i2,e2')])
          _ -> return entV
    v <- foldM go entityVector collisions -- handle interactions
    mapM (maybe (return Nothing) step) v  -- step each entity

  step :: Entity -> CM (Maybe Entity)
  step = \case
    EPlayer a -> update EPlayer a $ stepPlayer input
    EBullet a -> update EBullet a $ stepBullet time dtime
    PSpawn  a -> update PSpawn a $ stepSpawn time dtime
    e -> return $ Just e

  interact :: Bool -> (Entity,Entity) -> CM (Maybe Entity,Maybe Entity) -- TODO: generalize pairs to interaction event types
                                                                        --        e.g. player-item, player-teleport, killbox-players
  interact swap = \case

    (EPlayer p,EHealth a) -> do -- collects newly create entities also handles random seed
                              let c = p^.pHealth >= 200
                              (,) <$> update EPlayer p (unless c $ pHealth += a^.hQuantity) -- can: die or create new entities
                                  <*> update EHealth a (unless c $ respawn time EHealth)

    (EPlayer p,EBullet a) -> (,) <$> update EPlayer p (pHealth -= a^.bDamage)
                                 <*> update EBullet a die

    (EPlayer p,EArmor a)  -> (,) <$> update EPlayer p (pArmor += a^.rQuantity)
                                 <*> update EArmor a (if a^.rDropped then die else respawn time EArmor)

    (EPlayer p,EAmmo a)   -> (,) <$> update EPlayer p (pAmmo += a^.aQuantity)
                                 <*> update EAmmo a (if a^.aDropped then die else respawn time EAmmo)

    (EPlayer p,EWeapon a) -> (,) <$> update EPlayer p (pAmmo += 10)
                                 <*> update EWeapon a (if a^.wDropped then die else respawn time EWeapon)

    (EPlayer p,ELava a)   -> (,) <$> update EPlayer p (do {tick <- oncePerSec; when tick (pHealth -= a^.lDamage)})
                                 <*> update ELava a (return ())

    (EPlayer p,ETeleport a) -> (,) <$> update EPlayer p (pPosition .= (0,0)) -- TODO: lookup target, get the position + implement telefrag (killbox)
                                   <*> update ETeleport a (return ())

    (EBullet a,b)         -> (,Just b) <$> update EBullet a die

    (a,b) | swap -> interact False (b,a)
          | otherwise -> return (Just a,Just b)

  oncePerSec = do
    t <- use pDamageTimer
    if t > time then return False else do
      pDamageTimer .= time + 1
      return True

initialPlayer = Player
  { _pPosition    = (0,0)
  , _pFVelocity   = 0
  , _pSVelocity   = 0
  , _pAngle       = 0
  , _pHealth      = 100
  , _pAmmo        = 100
  , _pArmor       = 0
  , _pShootTime   = 0
  , _pDamageTimer = 0
  , _pName        = "Bones"
  , _pId          = 0
  }

addEntities ents = tell (ents,[])
addVisuals vis = tell ([],vis)

stepSpawn :: Time -> DTime -> EM Spawn ()
stepSpawn t dt = do
  spawnTime <- view sSpawnTime
  unless (t < spawnTime) $ do
    ent <- view sEntity
    addEntities [ent]
    die

stepPlayer :: Input -> EM Player ()
stepPlayer input@Input{..} = do
  -- acceleration according input
  pAngle += rightmove * dtime
  angle <- use pAngle
  let direction = unitVectorAtAngle $ degToRad angle
  pFVelocity += forwardmove * dtime
  pSVelocity += sidemove * dtime
  -- friction
  len <- use pFVelocity
  sideLen <- use pSVelocity
  let friction = 150
  pFVelocity %= (*) (max 0 $ (len - dtime * friction * signum len) / len)
  pSVelocity %= (*) (max 0 $ (sideLen - dtime * friction * signum sideLen) / sideLen)

  -- move
  pFVelocity %= max (-200) . min 200
  pSVelocity %= max (-200) . min 200
  forwardVelocity <- use pFVelocity
  sideVelocity <- use pSVelocity

  pPosition += mulSV (dtime * forwardVelocity) direction
  let strafeDirection = unitVectorAtAngle $ degToRad (angle - 90)
  pPosition += mulSV (dtime * sideVelocity) strafeDirection
  -- shoot
  shootTime <- view pShootTime
  when (shoot && shootTime < time) $ do
    pos <- use pPosition
    addEntities [EBullet $ Bullet (pos + mulSV 30 direction) (mulSV 500 direction) 1 2]
    pShootTime .= time + 0.1

  pHealth %= min 200
  -- death
  health <- use pHealth
  unless (health > 0) $ do
    [x1,y1,x2,y2] <- replicateM 4 $ getRandomR (-50,50)
    pos <- use pPosition
    ammo <- use pAmmo
    armor <- use pArmor
    addEntities
      [ PSpawn $ Spawn (time + 2) $ EPlayer initialPlayer
      , EAmmo  $ Ammo (pos + (x1,y1)) (ammo) True
      , EArmor $ Armor (pos + (x2,y2)) (armor) True
      ]
    addVisuals [VParticle $ Particle pos (mulSV 400 $ unitVectorAtAngle (pi / 50 * i)) 1 | i <- [0..100]]
    die

stepBullet :: Time -> DTime -> EM Bullet ()
stepBullet t dt = do
  (u,v) <- use bDirection
  bPosition += (dt * u, dt * v)
  -- die on spent lifetime
  bLifeTime -= dt
  lifeTime <- use bLifeTime
  when (lifeTime < 0) die

updateVisuals :: PureMT -> Time -> DTime -> [Visual] -> (PureMT,[Visual])
updateVisuals randGen time dtime visuals = (randGen',catMaybes visuals' ++ newVisuals) where
  ((visuals',newVisuals),randGen') = collect randGen $ mapM stepVisual visuals

  stepVisual = \case
    VParticle a -> update VParticle a $ stepParticle time dtime
    e -> return $ Just e

stepParticle :: Time -> DTime -> VM Particle ()
stepParticle t dt = do
  (u,v) <- use vpDirection
  vpPosition += (dt * u, dt * v)
  -- die on spent lifetime
  vpLifeTime -= dt
  lifeTime <- use vpLifeTime
  when (lifeTime < 0) die

-----

data Input
  = Input
  { forwardmove :: Float
  , rightmove   :: Float
  , sidemove    :: Float
  , shoot       :: Bool
  , dtime       :: Float
  , time        :: Float
  } deriving Show

data World
  = World
  { _wEntities  :: [Entity]
  , _wVisuals   :: [Visual]
  , _wInput     :: Input
  , _wRandomGen :: PureMT
  } deriving Show

makeLenses ''World

inputFun e w = w & wInput .~ i' where
  f Down = 300
  f Up = -300

  i@Input{..} = w^.wInput
  i' = case e of
    EventKey (Char 'w') s _ _ -> i {forwardmove = forwardmove + f s}
    EventKey (Char 's') s _ _ -> i {forwardmove = forwardmove - f s}
    EventKey (Char 'd') s _ _ -> i {rightmove = rightmove - f s}
    EventKey (Char 'a') s _ _ -> i {rightmove = rightmove + f s}
    EventKey (Char 'e') s _ _ -> i {sidemove = sidemove + f s}
    EventKey (Char 'q') s _ _ -> i {sidemove = sidemove - f s}
    EventKey (SpecialKey KeySpace) s _ _ -> i {shoot = s == Down}
    _ -> i

stepFun :: Float -> World -> World
stepFun dt = execState $ do
  -- update time
  wInput %= (\i -> i {dtime = dt, time = time i + dt})
  input <- use wInput
  ents <- use wEntities
  vis <- use wVisuals
  rand <- use wRandomGen
  let (r1,e,v1) = updateEntities rand input ents
      Input{..} = input
      (r2,v2) = updateVisuals r1 time dtime vis
  wEntities .= e
  wRandomGen .= r2
  wVisuals .= v1 ++ v2

renderFun w = Pictures $ ents ++ vis where
  ents = flip map (w^.wEntities) $ \case
    EPlayer p -> let (x,y) = p^.pPosition
                     gfx = Translate x y $ text (p^.pName) $ Rotate (-p^.pAngle) $ Pictures [Polygon [(-10,-6),(10,0),(-10,6)],Circle 20]
                     hud = Translate (-50) 250 $ Scale 0.2 0.2 $ Text $ printf "health:%d ammo:%d armor:%d" (p^.pHealth) (p^.pAmmo) (p^.pArmor)
                 in Pictures [hud,gfx]
    EBullet b   -> Translate x y $ Color green $ Circle 2 where (x,y) = b^.bPosition
    EWeapon a   -> Translate x y $ text "Weapon" $ Color blue $ Circle 10 where (x,y) = a^.wPosition
    EAmmo a     -> Translate x y $ text "Ammo" $ Color (light blue) $ Circle 8 where (x,y) = a^.aPosition
    EArmor a    -> Translate x y $ text "Armor" $ Color red $ Circle 10 where (x,y) = a^.rPosition
    EHealth a   -> Translate x y $ text "Health" $ Color yellow $ Circle 10 where (x,y) = a^.hPosition
    ELava a     -> Translate x y $ text "Lava" $ Color orange $ Circle 50 where (x,y) = a^.lPosition
    ETeleport a -> Translate x y $ text "Teleport" $ Color magenta $ Circle 20 where (x,y) = a^.tPosition
    ETarget a   -> Translate x y $ text "Target" Blank where (x,y) = a^.ttPosition
    _ -> Blank

  text s p = Pictures [Scale 0.1 0.1 $ Text s, p]

  vis = flip map (w^.wVisuals) $ \case
    VParticle a -> Translate x y $ Color red $ Circle 1 where (x,y) = a^.vpPosition
    _ -> Blank

emptyInput = Input 0 0 0 False 0 0
emptyWorld = World
  [ EPlayer initialPlayer
  , EBullet   (Bullet (30,30) (10,10) 100 10)
  , EWeapon   (Weapon (10,20) False)
  , EAmmo     (Ammo (100,100) 20 False)
  , EArmor    (Armor (200,100) 30 False)
  , EHealth   (Health (100, 200) 50)
  , ELava     (Lava (-200,-100) 10)
  , ETeleport (Teleport (-200,100) "t1")
  , ETarget   (Target (300,-100) "t1")
  ] [] emptyInput (pureMT 123456789)

main = play (InWindow "Lens MiniGame" (800, 600) (10, 10)) white 100 emptyWorld renderFun inputFun stepFun
