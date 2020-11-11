{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

-- | Strict tuples.
module Data.Tuple.Strict where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Hashable.Lifted
  ( Hashable1,
    Hashable2,
    defaultLiftHashWithSalt,
    hashWithSalt1,
    liftHashWithSalt,
    liftHashWithSalt2,
  )
import Data.Semigroup
import GHC.Generics (Generic)

newtype T1 a
  = T1 a
  deriving stock (Bounded, Eq, Generic, Ord, Read, Show)
  deriving newtype (Hashable, Monoid, NFData, Semigroup)

-- | @since 0.1.3
deriving stock instance Foldable T1

-- | @since 0.1.3
deriving stock instance Functor T1

-- | @since 0.1.3
deriving stock instance Traversable T1

-- | @since 0.1.3
instance Applicative T1 where
  pure = T1
  T1 f <*> T1 a = T1 (f a)

-- | @since 0.1.3
instance Monad T1 where
  return = pure
  T1 a >>= f = f a

data T2 a b
  = T2 a b
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T2 a)

-- | @since 0.1.3
deriving stock instance Functor (T2 a)

-- | @since 0.1.3
deriving stock instance Traversable (T2 a)

-- | @since 0.1.3
instance Monoid a => Applicative (T2 a) where
  pure b = T2 mempty b
  T2 a f <*> T2 a' b = T2 (a <> a') (f b)

-- | @since 0.1.3
instance Monoid a => Monad (T2 a) where
  return = pure
  T2 a b >>= f = case f b of
    T2 a' b' -> T2 (a <> a') b'

instance (Hashable a, Hashable b) => Hashable (T2 a b) where
  hash (T2 a b) = hash a `hashWithSalt` b
  hashWithSalt = hashWithSalt1

instance Hashable a => Hashable1 (T2 a) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable2 T2 where
  liftHashWithSalt2 h1 h2 slt (T2 a b) = slt `h1` a `h2` b

instance (Monoid a, Monoid b) => Monoid (T2 a b) where
  mempty = T2 mempty mempty

instance (NFData a, NFData b) => NFData (T2 a b) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b) => Semigroup (T2 a b) where
  T2 a1 b1 <> T2 a2 b2 = T2 (a1 <> a2) (b1 <> b2)
  stimes ii (T2 a b) = T2 (stimes ii a) (stimes ii b)

-- | @since 0.1.3
instance Bifunctor T2 where
  bimap f g (T2 a b) = T2 (f a) (g b)

-- | @since 0.1.3
instance Biapplicative T2 where
  bipure = T2
  T2 f g <<*>> T2 a b = T2 (f a) (g b)

-- | @since 0.1.3
instance Bifoldable T2 where
  bifoldMap f g (T2 a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable T2 where
  bitraverse f g (T2 a b) = T2 <$> f a <*> g b

-- | A strict, 'T2'-based analog to 'fst'
--
-- @since 0.1.3
sfst :: T2 a b -> a
sfst (T2 a _) = a

-- | A strict, 'T2'-based analog to 'snd'
--
-- @since 0.1.3
ssnd :: T2 a b -> b
ssnd (T2 _ b) = b

-- | A strict, 'T2'-based analog to 'curry'
--
-- @since 0.1.3
scurry :: (T2 a b -> c) -> a -> b -> c
scurry f a b = f (T2 a b)

-- | A strict, 'T2'-based analog to 'uncurry'
--
-- @since 0.1.3
suncurry :: (a -> b -> c) -> T2 a b -> c
suncurry f (T2 a b) = f a b

-- | A strict, 'T2'-based analog to 'swap'
--
-- @since 0.1.3
sswap :: T2 a b -> T2 b a
sswap (T2 a b) = T2 b a

data T3 a b c
  = T3 a b c
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T3 a b)

-- | @since 0.1.3
deriving stock instance Functor (T3 a b)

-- | @since 0.1.3
deriving stock instance Traversable (T3 a b)

-- | @since 0.1.3
instance (Monoid a, Monoid b) => Applicative (T3 a b) where
  pure c = T3 mempty mempty c
  T3 a b f <*> T3 a' b' c = T3 (a <> a') (b <> b') (f c)

-- | @since 0.1.3
instance (Monoid a, Monoid b) => Monad (T3 a b) where
  return = pure
  T3 a b c >>= f = case f c of
    T3 a' b' c' -> T3 (a <> a') (b <> b') c'

instance (Hashable a, Hashable b, Hashable c) => Hashable (T3 a b c) where
  hash (T3 a b c) = hash a `hashWithSalt` b `hashWithSalt` c
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b) => Hashable1 (T3 a b) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable a => Hashable2 (T3 a) where
  liftHashWithSalt2 h1 h2 slt (T3 a b c) =
    (slt `hashWithSalt` a) `h1` b `h2` c

instance (Monoid a, Monoid b, Monoid c) => Monoid (T3 a b c) where
  mempty = T3 mempty mempty mempty

instance (NFData a, NFData b, NFData c) => NFData (T3 a b c) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (T3 a b c) where
  T3 a1 b1 c1 <> T3 a2 b2 c2 = T3 (a1 <> a2) (b1 <> b2) (c1 <> c2)
  stimes ii (T3 a b c) = T3 (stimes ii a) (stimes ii b) (stimes ii c)

-- | @since 0.1.3
instance Bifunctor (T3 x) where
  bimap f g (T3 x a b) = T3 x (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T3 x) where
  bifoldMap f g (T3 _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T3 x) where
  bitraverse f g (T3 x a b) = T3 x <$> f a <*> g b

data T4 a b c d
  = T4 a b c d
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T4 a b c)

-- | @since 0.1.3
deriving stock instance Functor (T4 a b c)

-- | @since 0.1.3
deriving stock instance Traversable (T4 a b c)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c) => Applicative (T4 a b c) where
  pure d = T4 mempty mempty mempty d
  T4 a b c f <*> T4 a' b' c' d = T4 (a <> a') (b <> b') (c <> c') (f d)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c) => Monad (T4 a b c) where
  return = pure
  T4 a b c d >>= f = case f d of
    T4 a' b' c' d' -> T4 (a <> a') (b <> b') (c <> c') d'

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (T4 a b c d) where
  hash (T4 a b c d) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c) => Hashable1 (T4 a b c) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b) => Hashable2 (T4 a b) where
  liftHashWithSalt2 h1 h2 slt (T4 a b c d) =
    (slt `hashWithSalt` a `hashWithSalt` b) `h1` c `h2` d

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (T4 a b c d) where
  mempty = T4 mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d) => NFData (T4 a b c d) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (T4 a b c d) where
  T4 a1 b1 c1 d1 <> T4 a2 b2 c2 d2 = T4 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
  stimes ii (T4 a b c d) = T4 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d)

-- | @since 0.1.3
instance Bifunctor (T4 x y) where
  bimap f g (T4 x y a b) = T4 x y (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T4 x y) where
  bifoldMap f g (T4 _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T4 x y) where
  bitraverse f g (T4 x y a b) = T4 x y <$> f a <*> g b

data T5 a b c d e
  = T5 a b c d e
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T5 a b c d)

-- | @since 0.1.3
deriving stock instance Functor (T5 a b c d)

-- | @since 0.1.3
deriving stock instance Traversable (T5 a b c d)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Applicative (T5 a b c d) where
  pure e = T5 mempty mempty mempty mempty e
  T5 a b c d f <*> T5 a' b' c' d' e = T5 (a <> a') (b <> b') (c <> c') (d <> d') (f e)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monad (T5 a b c d) where
  return = pure
  T5 a b c d e >>= f = case f e of
    T5 a' b' c' d' e' -> T5 (a <> a') (b <> b') (c <> c') (d <> d') e'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (T5 a b c d e) where
  hash (T5 a b c d e) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable1 (T5 a b c d) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c) => Hashable2 (T5 a b c) where
  liftHashWithSalt2 h1 h2 slt (T5 a b c d e) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c) `h1` d `h2` e

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (T5 a b c d e) where
  mempty = T5 mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e) => NFData (T5 a b c d e) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (T5 a b c d e) where
  T5 a1 b1 c1 d1 e1 <> T5 a2 b2 c2 d2 e2 = T5 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
  stimes ii (T5 a b c d e) = T5 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e)

-- | @since 0.1.3
instance Bifunctor (T5 x y z) where
  bimap f g (T5 x y z a b) = T5 x y z (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T5 x y z) where
  bifoldMap f g (T5 _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T5 x y z) where
  bitraverse f g (T5 x y z a b) = T5 x y z <$> f a <*> g b

data T6 a b c d e f
  = T6 a b c d e f
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T6 a b c d e)

-- | @since 0.1.3
deriving stock instance Functor (T6 a b c d e)

-- | @since 0.1.3
deriving stock instance Traversable (T6 a b c d e)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Applicative (T6 a b c d e) where
  pure f = T6 mempty mempty mempty mempty mempty f
  T6 a b c d e f <*> T6 a' b' c' d' e' f' =
    T6 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f f')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monad (T6 a b c d e) where
  return = pure
  T6 a b c d e f >>= g = case g f of
    T6 a' b' c' d' e' f' -> T6 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') f'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable (T6 a b c d e f) where
  hash (T6 a b c d e f) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable1 (T6 a b c d e) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable2 (T6 a b c d) where
  liftHashWithSalt2 h1 h2 slt (T6 a b c d e f) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d) `h1` e `h2` f

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monoid (T6 a b c d e f) where
  mempty = T6 mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f) => NFData (T6 a b c d e f) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f) => Semigroup (T6 a b c d e f) where
  T6 a1 b1 c1 d1 e1 f1 <> T6 a2 b2 c2 d2 e2 f2 = T6 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)
  stimes ii (T6 a b c d e f) = T6 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f)

-- | @since 0.1.3
instance Bifunctor (T6 x y z w) where
  bimap f g (T6 x y z w a b) = T6 x y z w (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T6 x y z w) where
  bifoldMap f g (T6 _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T6 x y z w) where
  bitraverse f g (T6 x y z w a b) = T6 x y z w <$> f a <*> g b

data T7 a b c d e f g
  = T7 a b c d e f g
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T7 a b c d e f)

-- | @since 0.1.3
deriving stock instance Functor (T7 a b c d e f)

-- | @since 0.1.3
deriving stock instance Traversable (T7 a b c d e f)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Applicative (T7 a b c d e f) where
  pure g = T7 mempty mempty mempty mempty mempty mempty g
  T7 a b c d e f g <*> T7 a' b' c' d' e' f' g' =
    T7 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g g')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monad (T7 a b c d e f) where
  return = pure
  T7 a b c d e f g >>= h = case h g of
    T7 a' b' c' d' e' f' g' -> T7 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') g'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable (T7 a b c d e f g) where
  hash (T7 a b c d e f g) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable1 (T7 a b c d e f) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable2 (T7 a b c d e) where
  liftHashWithSalt2 h1 h2 slt (T7 a b c d e f g) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e) `h1` f `h2` g

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monoid (T7 a b c d e f g) where
  mempty = T7 mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g) => NFData (T7 a b c d e f g) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g) => Semigroup (T7 a b c d e f g) where
  T7 a1 b1 c1 d1 e1 f1 g1 <> T7 a2 b2 c2 d2 e2 f2 g2 = T7 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)
  stimes ii (T7 a b c d e f g) = T7 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g)

-- | @since 0.1.3
instance Bifunctor (T7 x y z w t) where
  bimap f g (T7 x y z w t a b) = T7 x y z w t (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T7 x y z w t) where
  bifoldMap f g (T7 _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T7 x y z w t) where
  bitraverse f g (T7 x y z w t a b) = T7 x y z w t <$> f a <*> g b

data T8 a b c d e f g h
  = T8 a b c d e f g h
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T8 a b c d e f g)

-- | @since 0.1.3
deriving stock instance Functor (T8 a b c d e f g)

-- | @since 0.1.3
deriving stock instance Traversable (T8 a b c d e f g)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Applicative (T8 a b c d e f g) where
  pure h = T8 mempty mempty mempty mempty mempty mempty mempty h
  T8 a b c d e f g h <*> T8 a' b' c' d' e' f' g' h' =
    T8 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h h')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monad (T8 a b c d e f g) where
  return = pure
  T8 a b c d e f g h >>= i = case i h of
    T8 a' b' c' d' e' f' g' h' ->
      T8 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') h'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable (T8 a b c d e f g h) where
  hash (T8 a b c d e f g h) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable1 (T8 a b c d e f g) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable2 (T8 a b c d e f) where
  liftHashWithSalt2 h1 h2 slt (T8 a b c d e f g h) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f) `h1` g `h2` h

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monoid (T8 a b c d e f g h) where
  mempty = T8 mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h) => NFData (T8 a b c d e f g h) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h) => Semigroup (T8 a b c d e f g h) where
  T8 a1 b1 c1 d1 e1 f1 g1 h1 <> T8 a2 b2 c2 d2 e2 f2 g2 h2 = T8 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2)
  stimes ii (T8 a b c d e f g h) = T8 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h)

-- | @since 0.1.3
instance Bifunctor (T8 x y z w t u) where
  bimap f g (T8 x y z w t u a b) = T8 x y z w t u (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T8 x y z w t u) where
  bifoldMap f g (T8 _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T8 x y z w t u) where
  bitraverse f g (T8 x y z w t u a b) = T8 x y z w t u <$> f a <*> g b

data T9 a b c d e f g h i
  = T9 a b c d e f g h i
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T9 a b c d e f g h)

-- | @since 0.1.3
deriving stock instance Functor (T9 a b c d e f g h)

-- | @since 0.1.3
deriving stock instance Traversable (T9 a b c d e f g h)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Applicative (T9 a b c d e f g h) where
  pure i = T9 mempty mempty mempty mempty mempty mempty mempty mempty i
  T9 a b c d e f g h i <*> T9 a' b' c' d' e' f' g' h' i' =
    T9 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i i')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monad (T9 a b c d e f g h) where
  return = pure
  T9 a b c d e f g h i >>= j = case j i of
    T9 a' b' c' d' e' f' g' h' i' ->
      T9 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') i'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable (T9 a b c d e f g h i) where
  hash (T9 a b c d e f g h i) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable1 (T9 a b c d e f g h) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable2 (T9 a b c d e f g) where
  liftHashWithSalt2 h1 h2 slt (T9 a b c d e f g h i) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g) `h1` h `h2` i

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Monoid (T9 a b c d e f g h i) where
  mempty = T9 mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i) => NFData (T9 a b c d e f g h i) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i) => Semigroup (T9 a b c d e f g h i) where
  T9 a1 b1 c1 d1 e1 f1 g1 h1 i1 <> T9 a2 b2 c2 d2 e2 f2 g2 h2 i2 = T9 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2)
  stimes ii (T9 a b c d e f g h i) = T9 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i)

-- | @since 0.1.3
instance Bifunctor (T9 x y z w t u v) where
  bimap f g (T9 x y z w t u v a b) = T9 x y z w t u v (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T9 x y z w t u v) where
  bifoldMap f g (T9 _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T9 x y z w t u v) where
  bitraverse f g (T9 x y z w t u v a b) = T9 x y z w t u v <$> f a <*> g b

data T10 a b c d e f g h i j
  = T10 a b c d e f g h i j
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T10 a b c d e f g h i)

-- | @since 0.1.3
deriving stock instance Functor (T10 a b c d e f g h i)

-- | @since 0.1.3
deriving stock instance Traversable (T10 a b c d e f g h i)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Applicative (T10 a b c d e f g h i) where
  pure j = T10 mempty mempty mempty mempty mempty mempty mempty mempty mempty j
  T10 a b c d e f g h i j <*> T10 a' b' c' d' e' f' g' h' i' j' =
    T10 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j j')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Monad (T10 a b c d e f g h i) where
  return = pure
  T10 a b c d e f g h i j >>= k = case k j of
    T10 a' b' c' d' e' f' g' h' i' j' ->
      T10 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') j'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable (T10 a b c d e f g h i j) where
  hash (T10 a b c d e f g h i j) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable1 (T10 a b c d e f g h i) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable2 (T10 a b c d e f g h) where
  liftHashWithSalt2 h1 h2 slt (T10 a b c d e f g h i j) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h) `h1` i `h2` j

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Monoid (T10 a b c d e f g h i j) where
  mempty = T10 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j) => NFData (T10 a b c d e f g h i j) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j) => Semigroup (T10 a b c d e f g h i j) where
  T10 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 <> T10 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 = T10 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2)
  stimes ii (T10 a b c d e f g h i j) = T10 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j)

-- | @since 0.1.3
instance Bifunctor (T10 x y z w t u v p) where
  bimap f g (T10 x y z w t u v p a b) = T10 x y z w t u v p (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T10 x y z w t u v p) where
  bifoldMap f g (T10 _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T10 x y z w t u v p) where
  bitraverse f g (T10 x y z w t u v p a b) = T10 x y z w t u v p <$> f a <*> g b

data T11 a b c d e f g h i j k
  = T11 a b c d e f g h i j k
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T11 a b c d e f g h i j)

-- | @since 0.1.3
deriving stock instance Functor (T11 a b c d e f g h i j)

-- | @since 0.1.3
deriving stock instance Traversable (T11 a b c d e f g h i j)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Applicative (T11 a b c d e f g h i j) where
  pure k = T11 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty k
  T11 a b c d e f g h i j k <*> T11 a' b' c' d' e' f' g' h' i' j' k' =
    T11 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k k')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Monad (T11 a b c d e f g h i j) where
  return = pure
  T11 a b c d e f g h i j k >>= l = case l k of
    T11 a' b' c' d' e' f' g' h' i' j' k' ->
      T11 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') k'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable (T11 a b c d e f g h i j k) where
  hash (T11 a b c d e f g h i j k) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable1 (T11 a b c d e f g h i j) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable2 (T11 a b c d e f g h i) where
  liftHashWithSalt2 h1 h2 slt (T11 a b c d e f g h i j k) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i) `h1` j `h2` k

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) => Monoid (T11 a b c d e f g h i j k) where
  mempty = T11 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k) => NFData (T11 a b c d e f g h i j k) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k) => Semigroup (T11 a b c d e f g h i j k) where
  T11 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 <> T11 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 = T11 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2)
  stimes ii (T11 a b c d e f g h i j k) = T11 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k)

-- | @since 0.1.3
instance Bifunctor (T11 x y z w t u v p q) where
  bimap f g (T11 x y z w t u v p q a b) = T11 x y z w t u v p q (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T11 x y z w t u v p q) where
  bifoldMap f g (T11 _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T11 x y z w t u v p q) where
  bitraverse f g (T11 x y z w t u v p q a b) = T11 x y z w t u v p q <$> f a <*> g b

data T12 a b c d e f g h i j k l
  = T12 a b c d e f g h i j k l
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T12 a b c d e f g h i j k)

-- | @since 0.1.3
deriving stock instance Functor (T12 a b c d e f g h i j k)

-- | @since 0.1.3
deriving stock instance Traversable (T12 a b c d e f g h i j k)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) =>
  Applicative (T12 a b c d e f g h i j k)
  where
  pure l = T12 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty l
  T12 a b c d e f g h i j k l <*> T12 a' b' c' d' e' f' g' h' i' j' k' l' =
    T12 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l l')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) =>
  Monad (T12 a b c d e f g h i j k)
  where
  return = pure
  T12 a b c d e f g h i j k l >>= m = case m l of
    T12 a' b' c' d' e' f' g' h' i' j' k' l' ->
      T12 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') l'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable (T12 a b c d e f g h i j k l) where
  hash (T12 a b c d e f g h i j k l) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable1 (T12 a b c d e f g h i j k) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable2 (T12 a b c d e f g h i j) where
  liftHashWithSalt2 h1 h2 slt (T12 a b c d e f g h i j k l) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j) `h1` k `h2` l

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) => Monoid (T12 a b c d e f g h i j k l) where
  mempty = T12 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l) => NFData (T12 a b c d e f g h i j k l) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l) => Semigroup (T12 a b c d e f g h i j k l) where
  T12 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 <> T12 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 = T12 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2)
  stimes ii (T12 a b c d e f g h i j k l) = T12 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l)

-- | @since 0.1.3
instance Bifunctor (T12 x y z w t u v p q r) where
  bimap f g (T12 x y z w t u v p q r a b) = T12 x y z w t u v p q r (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T12 x y z w t u v p q r) where
  bifoldMap f g (T12 _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T12 x y z w t u v p q r) where
  bitraverse f g (T12 x y z w t u v p q r a b) = T12 x y z w t u v p q r <$> f a <*> g b

data T13 a b c d e f g h i j k l m
  = T13 a b c d e f g h i j k l m
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T13 a b c d e f g h i j k l)

-- | @since 0.1.3
deriving stock instance Functor (T13 a b c d e f g h i j k l)

-- | @since 0.1.3
deriving stock instance Traversable (T13 a b c d e f g h i j k l)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) =>
  Applicative (T13 a b c d e f g h i j k l)
  where
  pure m = T13 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty m
  T13 a b c d e f g h i j k l m <*> T13 a' b' c' d' e' f' g' h' i' j' k' l' m' =
    T13 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m m')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) =>
  Monad (T13 a b c d e f g h i j k l)
  where
  return = pure
  T13 a b c d e f g h i j k l m >>= n = case n m of
    T13 a' b' c' d' e' f' g' h' i' j' k' l' m' ->
      T13 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') m'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m) => Hashable (T13 a b c d e f g h i j k l m) where
  hash (T13 a b c d e f g h i j k l m) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable1 (T13 a b c d e f g h i j k l) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable2 (T13 a b c d e f g h i j k) where
  liftHashWithSalt2 h1 h2 slt (T13 a b c d e f g h i j k l m) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k) `h1` l `h2` m

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) => Monoid (T13 a b c d e f g h i j k l m) where
  mempty = T13 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m) => NFData (T13 a b c d e f g h i j k l m) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m) => Semigroup (T13 a b c d e f g h i j k l m) where
  T13 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 <> T13 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 = T13 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2)
  stimes ii (T13 a b c d e f g h i j k l m) = T13 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m)

-- | @since 0.1.3
instance Bifunctor (T13 x y z w t u v p q r s) where
  bimap f g (T13 x y z w t u v p q r s a b) = T13 x y z w t u v p q r s (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T13 x y z w t u v p q r s) where
  bifoldMap f g (T13 _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T13 x y z w t u v p q r s) where
  bitraverse f g (T13 x y z w t u v p q r s a b) = T13 x y z w t u v p q r s <$> f a <*> g b

data T14 a b c d e f g h i j k l m n
  = T14 a b c d e f g h i j k l m n
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T14 a b c d e f g h i j k l m)

-- | @since 0.1.3
deriving stock instance Functor (T14 a b c d e f g h i j k l m)

-- | @since 0.1.3
deriving stock instance Traversable (T14 a b c d e f g h i j k l m)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) =>
  Applicative (T14 a b c d e f g h i j k l m)
  where
  pure n = T14 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty n
  T14 a b c d e f g h i j k l m n <*> T14 a' b' c' d' e' f' g' h' i' j' k' l' m' n' =
    T14 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n n')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) =>
  Monad (T14 a b c d e f g h i j k l m)
  where
  return = pure
  T14 a b c d e f g h i j k l m n >>= o = case o n of
    T14 a' b' c' d' e' f' g' h' i' j' k' l' m' n' ->
      T14 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') n'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n) => Hashable (T14 a b c d e f g h i j k l m n) where
  hash (T14 a b c d e f g h i j k l m n) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m) => Hashable1 (T14 a b c d e f g h i j k l m) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable2 (T14 a b c d e f g h i j k l) where
  liftHashWithSalt2 h1 h2 slt (T14 a b c d e f g h i j k l m n) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l) `h1` m `h2` n

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n) => Monoid (T14 a b c d e f g h i j k l m n) where
  mempty = T14 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n) => NFData (T14 a b c d e f g h i j k l m n) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n) => Semigroup (T14 a b c d e f g h i j k l m n) where
  T14 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 <> T14 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 = T14 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2)
  stimes ii (T14 a b c d e f g h i j k l m n) = T14 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n)

-- | @since 0.1.3
instance Bifunctor (T14 x y z w t u v p q r s i) where
  bimap f g (T14 x y z w t u v p q r s i a b) = T14 x y z w t u v p q r s i (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T14 x y z w t u v p q r s i) where
  bifoldMap f g (T14 _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T14 x y z w t u v p q r s i) where
  bitraverse f g (T14 x y z w t u v p q r s i a b) = T14 x y z w t u v p q r s i <$> f a <*> g b

data T15 a b c d e f g h i j k l m n o
  = T15 a b c d e f g h i j k l m n o
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T15 a b c d e f g h i j k l m n)

-- | @since 0.1.3
deriving stock instance Functor (T15 a b c d e f g h i j k l m n)

-- | @since 0.1.3
deriving stock instance Traversable (T15 a b c d e f g h i j k l m n)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n) =>
  Applicative (T15 a b c d e f g h i j k l m n)
  where
  pure o = T15 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty o
  T15 a b c d e f g h i j k l m n o <*> T15 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' =
    T15 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o o')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n) =>
  Monad (T15 a b c d e f g h i j k l m n)
  where
  return = pure
  T15 a b c d e f g h i j k l m n o >>= p = case p o of
    T15 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' ->
      T15 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') o'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o) => Hashable (T15 a b c d e f g h i j k l m n o) where
  hash (T15 a b c d e f g h i j k l m n o) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n) => Hashable1 (T15 a b c d e f g h i j k l m n) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m) => Hashable2 (T15 a b c d e f g h i j k l m) where
  liftHashWithSalt2 h1 h2 slt (T15 a b c d e f g h i j k l m n o) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m) `h1` n `h2` o

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o) => Monoid (T15 a b c d e f g h i j k l m n o) where
  mempty = T15 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o) => NFData (T15 a b c d e f g h i j k l m n o) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o) => Semigroup (T15 a b c d e f g h i j k l m n o) where
  T15 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 <> T15 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 = T15 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2)
  stimes ii (T15 a b c d e f g h i j k l m n o) = T15 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o)

-- | @since 0.1.3
instance Bifunctor (T15 x y z w t u v p q r s i j) where
  bimap f g (T15 x y z w t u v p q r s i j a b) = T15 x y z w t u v p q r s i j (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T15 x y z w t u v p q r s i j) where
  bifoldMap f g (T15 _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T15 x y z w t u v p q r s i j) where
  bitraverse f g (T15 x y z w t u v p q r s i j a b) = T15 x y z w t u v p q r s i j <$> f a <*> g b

data T16 a b c d e f g h i j k l m n o p
  = T16 a b c d e f g h i j k l m n o p
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T16 a b c d e f g h i j k l m n o)

-- | @since 0.1.3
deriving stock instance Functor (T16 a b c d e f g h i j k l m n o)

-- | @since 0.1.3
deriving stock instance Traversable (T16 a b c d e f g h i j k l m n o)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o) =>
  Applicative (T16 a b c d e f g h i j k l m n o)
  where
  pure p = T16 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty p
  T16 a b c d e f g h i j k l m n o p <*> T16 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' =
    T16 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p p')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o) =>
  Monad (T16 a b c d e f g h i j k l m n o)
  where
  return = pure
  T16 a b c d e f g h i j k l m n o p >>= q = case q p of
    T16 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' ->
      T16 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') p'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p) => Hashable (T16 a b c d e f g h i j k l m n o p) where
  hash (T16 a b c d e f g h i j k l m n o p) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o) => Hashable1 (T16 a b c d e f g h i j k l m n o) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n) => Hashable2 (T16 a b c d e f g h i j k l m n) where
  liftHashWithSalt2 h1 h2 slt (T16 a b c d e f g h i j k l m n o p) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n) `h1` o `h2` p

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) => Monoid (T16 a b c d e f g h i j k l m n o p) where
  mempty = T16 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o, NFData p) => NFData (T16 a b c d e f g h i j k l m n o p) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p) => Semigroup (T16 a b c d e f g h i j k l m n o p) where
  T16 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 <> T16 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 = T16 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2)
  stimes ii (T16 a b c d e f g h i j k l m n o p) = T16 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p)

-- | @since 0.1.3
instance Bifunctor (T16 x y z w t u v p q r s i j k) where
  bimap f g (T16 x y z w t u v p q r s i j k a b) = T16 x y z w t u v p q r s i j k (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T16 x y z w t u v p q r s i j k) where
  bifoldMap f g (T16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T16 x y z w t u v p q r s i j k) where
  bitraverse f g (T16 x y z w t u v p q r s i j k a b) = T16 x y z w t u v p q r s i j k <$> f a <*> g b

data T17 a b c d e f g h i j k l m n o p q
  = T17 a b c d e f g h i j k l m n o p q
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.3
deriving stock instance Functor (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.3
deriving stock instance Traversable (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) =>
  Applicative (T17 a b c d e f g h i j k l m n o p)
  where
  pure q = T17 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty q
  T17 a b c d e f g h i j k l m n o p q <*> T17 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' =
    T17 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q q')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) =>
  Monad (T17 a b c d e f g h i j k l m n o p)
  where
  return = pure
  T17 a b c d e f g h i j k l m n o p q >>= r = case r q of
    T17 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' ->
      T17 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') q'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q) => Hashable (T17 a b c d e f g h i j k l m n o p q) where
  hash (T17 a b c d e f g h i j k l m n o p q) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p `hashWithSalt` q
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p) => Hashable1 (T17 a b c d e f g h i j k l m n o p) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o) => Hashable2 (T17 a b c d e f g h i j k l m n o) where
  liftHashWithSalt2 h1 h2 slt (T17 a b c d e f g h i j k l m n o p q) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o) `h1` p `h2` q

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q) => Monoid (T17 a b c d e f g h i j k l m n o p q) where
  mempty = T17 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o, NFData p, NFData q) => NFData (T17 a b c d e f g h i j k l m n o p q) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q) => Semigroup (T17 a b c d e f g h i j k l m n o p q) where
  T17 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 <> T17 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 = T17 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2)
  stimes ii (T17 a b c d e f g h i j k l m n o p q) = T17 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q)

-- | @since 0.1.3
instance Bifunctor (T17 x y z w t u v p q r s i j k l) where
  bimap f g (T17 x y z w t u v p q r s i j k l a b) = T17 x y z w t u v p q r s i j k l (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T17 x y z w t u v p q r s i j k l) where
  bifoldMap f g (T17 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T17 x y z w t u v p q r s i j k l) where
  bitraverse f g (T17 x y z w t u v p q r s i j k l a b) = T17 x y z w t u v p q r s i j k l <$> f a <*> g b

data T18 a b c d e f g h i j k l m n o p q r
  = T18 a b c d e f g h i j k l m n o p q r
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T18 a b c d e f g h i j k l m n o p q)

-- | @since 0.1.3
deriving stock instance Functor (T18 a b c d e f g h i j k l m n o p q)

-- | @since 0.1.3
deriving stock instance Traversable (T18 a b c d e f g h i j k l m n o p q)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q) =>
  Applicative (T18 a b c d e f g h i j k l m n o p q)
  where
  pure r = T18 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty r
  T18 a b c d e f g h i j k l m n o p q r <*> T18 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' r' =
    T18 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q <> q') (r r')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q) =>
  Monad (T18 a b c d e f g h i j k l m n o p q)
  where
  return = pure
  T18 a b c d e f g h i j k l m n o p q r >>= s = case s r of
    T18 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' r' ->
      T18 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q <> q') r'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q, Hashable r) => Hashable (T18 a b c d e f g h i j k l m n o p q r) where
  hash (T18 a b c d e f g h i j k l m n o p q r) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p `hashWithSalt` q `hashWithSalt` r
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q) => Hashable1 (T18 a b c d e f g h i j k l m n o p q) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p) => Hashable2 (T18 a b c d e f g h i j k l m n o p) where
  liftHashWithSalt2 h1 h2 slt (T18 a b c d e f g h i j k l m n o p q r) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p) `h1` q `h2` r

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r) => Monoid (T18 a b c d e f g h i j k l m n o p q r) where
  mempty = T18 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o, NFData p, NFData q, NFData r) => NFData (T18 a b c d e f g h i j k l m n o p q r) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q, Semigroup r) => Semigroup (T18 a b c d e f g h i j k l m n o p q r) where
  T18 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 <> T18 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 = T18 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2) (r1 <> r2)
  stimes ii (T18 a b c d e f g h i j k l m n o p q r) = T18 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q) (stimes ii r)

-- | @since 0.1.3
instance Bifunctor (T18 x y z w t u v p q r s i j k l m) where
  bimap f g (T18 x y z w t u v p q r s i j k l m a b) = T18 x y z w t u v p q r s i j k l m (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T18 x y z w t u v p q r s i j k l m) where
  bifoldMap f g (T18 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T18 x y z w t u v p q r s i j k l m) where
  bitraverse f g (T18 x y z w t u v p q r s i j k l m a b) = T18 x y z w t u v p q r s i j k l m <$> f a <*> g b

data T19 a b c d e f g h i j k l m n o p q r s
  = T19 a b c d e f g h i j k l m n o p q r s
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T19 a b c d e f g h i j k l m n o p q r)

-- | @since 0.1.3
deriving stock instance Functor (T19 a b c d e f g h i j k l m n o p q r)

-- | @since 0.1.3
deriving stock instance Traversable (T19 a b c d e f g h i j k l m n o p q r)

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r) =>
  Applicative (T19 a b c d e f g h i j k l m n o p q r)
  where
  pure s = T19 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty s
  T19 a b c d e f g h i j k l m n o p q r s <*> T19 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' r' s' =
    T19 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q <> q') (r <> r') (s s')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r) =>
  Monad (T19 a b c d e f g h i j k l m n o p q r)
  where
  return = pure
  T19 a b c d e f g h i j k l m n o p q r s >>= t = case t s of
    T19 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' r' s' ->
      T19 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q <> q') (r <> r') s'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q, Hashable r, Hashable s) => Hashable (T19 a b c d e f g h i j k l m n o p q r s) where
  hash (T19 a b c d e f g h i j k l m n o p q r s) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p `hashWithSalt` q `hashWithSalt` r `hashWithSalt` s
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q, Hashable r) => Hashable1 (T19 a b c d e f g h i j k l m n o p q r) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q) => Hashable2 (T19 a b c d e f g h i j k l m n o p q) where
  liftHashWithSalt2 h1 h2 slt (T19 a b c d e f g h i j k l m n o p q r s) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p `hashWithSalt` q) `h1` r `h2` s

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r, Monoid s) => Monoid (T19 a b c d e f g h i j k l m n o p q r s) where
  mempty = T19 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o, NFData p, NFData q, NFData r, NFData s) => NFData (T19 a b c d e f g h i j k l m n o p q r s) where
  rnf = rwhnf

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q, Semigroup r, Semigroup s) => Semigroup (T19 a b c d e f g h i j k l m n o p q r s) where
  T19 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 <> T19 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 = T19 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2) (r1 <> r2) (s1 <> s2)
  stimes ii (T19 a b c d e f g h i j k l m n o p q r s) = T19 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q) (stimes ii r) (stimes ii s)

-- | @since 0.1.3
instance Bifunctor (T19 x y z w t u v p q r s i j k l m n) where
  bimap f g (T19 x y z w t u v p q r s i j k l m n a b) = T19 x y z w t u v p q r s i j k l m n (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T19 x y z w t u v p q r s i j k l m n) where
  bifoldMap f g (T19 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T19 x y z w t u v p q r s i j k l m n) where
  bitraverse f g (T19 x y z w t u v p q r s i j k l m n a b) = T19 x y z w t u v p q r s i j k l m n <$> f a <*> g b
