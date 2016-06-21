{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, LambdaCase, RecordWildCards, FlexibleContexts, TupleSections #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Monad.RWS
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
    full q3 inventory
    drop inventory on death
    count deaths and kills (persistent data support)
    teleport (target support)
    jump pad
    animated visual only elements (i.e. particles on collision)

  goals:
    rule based
    compositional with reusable small components
    intuitive and easy to use
    simple and efficient operational semantics and implementation (should be easy to imagine the compilation/codegen)

  events:
    collision between entities and client

-}

type Vec2 = Vector

_x = _1
_y = _2

-- entities

data Player
  = Player
  { _pPosition    :: Vec2
  , _pVelocity    :: Float
  , _pAngle       :: Float
  , _pHealth      :: Int
  , _pAmmo        :: Int
  , _pArmor       :: Int
  , _pShootTime   :: Float
  , _pDamageTimer :: Float
  } deriving Show

data Bullet
  = Bullet
  { _bPosition   :: Vec2
  , _bDirection  :: Vec2
  , _bDamage     :: Int
  , _bLifeTime   :: Float
  } deriving Show

data Weapon
  = Weapon
  { _wPosition   :: Vec2
  } deriving Show

data Ammo
  = Ammo
  { _aPosition   :: Vec2
  , _aQuantity   :: Int
  } deriving Show

data Armor
  = Armor
  { _rPosition   :: Vec2
  , _rQuantity   :: Int
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

data Entity
  = EPlayer Player
  | EBullet Bullet
  | EWeapon Weapon
  | EAmmo   Ammo
  | EArmor  Armor
  | EHealth Health
  | ELava   Lava
  | PSpawn  Spawn
  deriving Show

concat <$> mapM makeLenses [''Player, ''Bullet, ''Weapon, ''Ammo, ''Armor, ''Spawn, ''Health, ''Lava]

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
    EPlayer a -> Just (a^.pPosition, 20)
    EBullet a -> Just (a^.bPosition, 2)
    EWeapon a -> Just (a^.wPosition, 10)
    EAmmo a   -> Just (a^.aPosition, 8)
    EArmor a  -> Just (a^.rPosition, 10)
    EHealth a -> Just (a^.hPosition, 10)
    ELava a   -> Just (a^.lPosition, 50)
    _ -> Nothing

type EM s a = ReaderT s (StateT s (MaybeT (WriterT [Entity] (Rand PureMT)))) a
type CM a = WriterT [Entity] (Rand PureMT) a

die = fail "die"

respawn t f = do
  s <- get
  t' <- getRandomR (t + 5, t + 10)
  tell [PSpawn $ Spawn t' (f s)]
  die

update :: (s -> Entity) -> s -> EM s a -> CM (Maybe Entity)
update f a m = fmap f <$> runMaybeT (execStateT (runReaderT m a) a)

{-
  collect collided entities
  transform entities in interaction, collect newly generated entities
  step entities, also collect generated entities
  append generated entities
-}
updateEntities :: PureMT -> Input -> [Entity] -> (PureMT,[Entity])
updateEntities randGen input@Input{..} ents = (randGen',catMaybes (V.toList nextEnts) ++ newEnts) where
  entityVector :: V.Vector (Maybe Entity)
  entityVector = V.fromList $ map Just ents

  collisions :: [(Int,Int)]
  collisions = collide ents

  ((nextEnts,newEnts),randGen') = collect $ do
    let go entV (i1,i2) = case (entV ! i1, entV ! i2) of
          (Just e1, Just e2) -> interact True (e1,e2) >>= \(e1',e2') -> return (entV // [(i1,e1'),(i2,e2')])
          _ -> return entV
    v <- foldM go entityVector collisions
    mapM (maybe (return Nothing) step) v

  collect :: WriterT w (Rand PureMT) a -> ((a,w),PureMT)
  collect m = runIdentity $ runRandT (runWriterT m) randGen

  step :: Entity -> CM (Maybe Entity)
  step = \case
    EPlayer a -> update EPlayer a $ stepPlayer input
    EBullet a -> update EBullet a $ stepBullet time dtime
    PSpawn  a -> update PSpawn a $ stepSpawn time dtime
    e -> return $ Just e

  interact :: Bool -> (Entity,Entity) -> CM (Maybe Entity,Maybe Entity)
  interact swap = \case

    (EPlayer p,EHealth a) -> do -- collects newly create entities also handles random seed
                              let c = p^.pHealth >= 200
                              (,) <$> update EPlayer p (unless c $ pHealth += a^.hQuantity) -- can: die or create new entities
                                  <*> update EHealth a (unless c $ respawn time EHealth)

    (EPlayer p,EBullet a) -> (,) <$> update EPlayer p (pHealth -= a^.bDamage)
                                 <*> update EBullet a die

    (EPlayer p,EArmor a)  -> (,) <$> update EPlayer p (pArmor += a^.rQuantity)
                                 <*> update EArmor a (respawn time EArmor)

    (EPlayer p,EAmmo a)   -> (,) <$> update EPlayer p (pAmmo += a^.aQuantity)
                                 <*> update EAmmo a (respawn time EAmmo)

    (EPlayer p,EWeapon a) -> (,) <$> update EPlayer p (pAmmo += 10)
                                 <*> update EWeapon a (respawn time EWeapon)

    (EPlayer p,ELava a)   -> (,) <$> update EPlayer p (do {tick <- oncePerSec; when tick (pHealth -= a^.lDamage)})
                                 <*> update ELava a (return ())

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
  , _pVelocity    = 0
  , _pAngle       = 0
  , _pHealth      = 100
  , _pAmmo        = 100
  , _pArmor       = 0
  , _pShootTime   = 0
  , _pDamageTimer = 0
  }

stepSpawn :: Time -> DTime -> EM Spawn ()
stepSpawn t dt = do
  spawnTime <- view sSpawnTime
  unless (t < spawnTime) $ do
    ent <- view sEntity
    tell [ent]
    die

stepPlayer :: Input -> EM Player ()
stepPlayer input@Input{..} = do
  -- acceleration according input
  pAngle += rightmove * dtime
  angle <- use pAngle
  let direction = unitVectorAtAngle $ degToRad angle
  pVelocity += forwardmove * dtime
  -- friction
  len <- use pVelocity
  let friction = 150
  pVelocity %= (*) (max 0 $ (len - dtime * friction * signum len) / len)
  -- move
  pVelocity %= max (-200) . min 200
  velocity <- use pVelocity
  pPosition += mulSV (dtime * velocity) direction

  -- shoot
  shootTime <- view pShootTime
  when (shoot && shootTime < time) $ do
    pos <- use pPosition
    tell [EBullet $ Bullet (pos + mulSV 30 direction) (mulSV 500 direction) 1 2]
    pShootTime .= time + 0.1

  pHealth %= min 200
  -- death
  health <- use pHealth
  unless (health > 0) $ do
    tell [PSpawn $ Spawn (time + 2) $ EPlayer initialPlayer]
    die

stepBullet :: Time -> DTime -> EM Bullet ()
stepBullet t dt = do
  (u,v) <- use bDirection
  bPosition += (dt * u, dt * v)
  -- die on collision
  -- die on spent lifetime
  bLifeTime -= dt
  lifeTime <- use bLifeTime
  when (lifeTime < 0) die

-----

data Input
  = Input
  { forwardmove :: Float
  , rightmove   :: Float
  , shoot       :: Bool
  , dtime       :: Float
  , time        :: Float
  } deriving Show

data World
  = World
  { _wEntities  :: [Entity]
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
    EventKey (SpecialKey KeySpace) s _ _ -> i {shoot = s == Down}
    _ -> i

stepFun :: Float -> World -> World
stepFun dt = execState $ do
  -- update time
  wInput %= (\i -> i {dtime = dt, time = time i + dt})
  input <- use wInput
  ents <- use wEntities
  rand <- use wRandomGen
  let (r,e) = updateEntities rand input ents
  wEntities .= e
  wRandomGen .= r

renderFun w = Pictures $ flip map (w^.wEntities) $ \case
  EPlayer p -> let (x,y) = p^.pPosition
                   gfx = Translate x y $ Rotate (-p^.pAngle) $ Pictures [Polygon [(-10,-6),(10,0),(-10,6)],Circle 20]
                   hud = Translate (-50) 250 $ Scale 0.2 0.2 $ Text $ printf "health:%d ammo:%d armor:%d" (p^.pHealth) (p^.pAmmo) (p^.pArmor)
               in Pictures [hud,gfx]
  EBullet b -> Translate x y $ Color green $ Circle 2 where (x,y) = b^.bPosition
  EWeapon a -> Translate x y $ Color blue $ Circle 10 where (x,y) = a^.wPosition
  EAmmo a   -> Translate x y $ Color (light blue) $ Circle 8 where (x,y) = a^.aPosition
  EArmor a  -> Translate x y $ Color red $ Circle 10 where (x,y) = a^.rPosition
  EHealth a -> Translate x y $ Color yellow $ Circle 10 where (x,y) = a^.hPosition
  ELava a   -> Translate x y $ Color orange $ Circle 50 where (x,y) = a^.lPosition

  _ -> Blank

emptyInput = Input 0 0 False 0 0
emptyWorld = World
  [ EPlayer initialPlayer
  , EBullet (Bullet (30,30) (10,10) 100 10)
  , EWeapon (Weapon (10,20))
  , EAmmo   (Ammo (100,100) 20)
  , EArmor  (Armor (200,100) 30)
  , EHealth (Health (100, 200) 50)
  , ELava   (Lava (-200,-100) 10)
  ] emptyInput (pureMT 123456789)

main = play (InWindow "Lens MiniGame" (800, 600) (10, 10)) white 100 emptyWorld renderFun inputFun stepFun
