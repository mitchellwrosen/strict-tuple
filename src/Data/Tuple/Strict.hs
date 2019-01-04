{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StrictData #-}

-- | Strict tuples.

module Data.Tuple.Strict where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Hashable.Lifted (Hashable1, Hashable2, defaultLiftHashWithSalt,
                             hashWithSalt1, liftHashWithSalt, liftHashWithSalt2)
import Data.Semigroup
import GHC.Generics (Generic)

newtype T1 a
  = T1 a
  deriving (Bounded, Eq, Generic, Hashable, Monoid, NFData, Ord, Read, Semigroup, Show)

data T2 a b
  = T2 a b
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T3 a b c
  = T3 a b c
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T4 a b c d
  = T4 a b c d
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T5 a b c d e
  = T5 a b c d e
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T6 a b c d e f
  = T6 a b c d e f
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T7 a b c d e f g
  = T7 a b c d e f g
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T8 a b c d e f g h
  = T8 a b c d e f g h
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T9 a b c d e f g h i
  = T9 a b c d e f g h i
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T10 a b c d e f g h i j
  = T10 a b c d e f g h i j
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T11 a b c d e f g h i j k
  = T11 a b c d e f g h i j k
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T12 a b c d e f g h i j k l
  = T12 a b c d e f g h i j k l
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T13 a b c d e f g h i j k l m
  = T13 a b c d e f g h i j k l m
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T14 a b c d e f g h i j k l m n
  = T14 a b c d e f g h i j k l m n
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T15 a b c d e f g h i j k l m n o
  = T15 a b c d e f g h i j k l m n o
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T16 a b c d e f g h i j k l m n o p
  = T16 a b c d e f g h i j k l m n o p
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T17 a b c d e f g h i j k l m n o p q
  = T17 a b c d e f g h i j k l m n o p q
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T18 a b c d e f g h i j k l m n o p q r
  = T18 a b c d e f g h i j k l m n o p q r
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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

data T19 a b c d e f g h i j k l m n o p q r s
  = T19 a b c d e f g h i j k l m n o p q r s
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

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
