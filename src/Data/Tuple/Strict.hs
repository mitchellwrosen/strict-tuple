{-# language DeriveGeneric, GeneralizedNewtypeDeriving, StrictData #-}

-- | Strict tuples.

module Data.Tuple.Strict where

import Data.Semigroup
import GHC.Generics (Generic)

newtype T1 a
  = T1 a
  deriving (Bounded, Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

data T2 a b
  = T2 a b
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b) => Monoid (T2 a b) where
  mempty = T2 mempty mempty

instance (Semigroup a, Semigroup b) => Semigroup (T2 a b) where
  T2 a1 b1 <> T2 a2 b2 = T2 (a1 <> a2) (b1 <> b2)
  stimes ii (T2 a b) = T2 (stimes ii a) (stimes ii b)

data T3 a b c
  = T3 a b c
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (T3 a b c) where
  mempty = T3 mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (T3 a b c) where
  T3 a1 b1 c1 <> T3 a2 b2 c2 = T3 (a1 <> a2) (b1 <> b2) (c1 <> c2)
  stimes ii (T3 a b c) = T3 (stimes ii a) (stimes ii b) (stimes ii c)

data T4 a b c d
  = T4 a b c d
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (T4 a b c d) where
  mempty = T4 mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (T4 a b c d) where
  T4 a1 b1 c1 d1 <> T4 a2 b2 c2 d2 = T4 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
  stimes ii (T4 a b c d) = T4 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d)

data T5 a b c d e
  = T5 a b c d e
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (T5 a b c d e) where
  mempty = T5 mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (T5 a b c d e) where
  T5 a1 b1 c1 d1 e1 <> T5 a2 b2 c2 d2 e2 = T5 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
  stimes ii (T5 a b c d e) = T5 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e)

data T6 a b c d e f
  = T6 a b c d e f
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monoid (T6 a b c d e f) where
  mempty = T6 mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f) => Semigroup (T6 a b c d e f) where
  T6 a1 b1 c1 d1 e1 f1 <> T6 a2 b2 c2 d2 e2 f2 = T6 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)
  stimes ii (T6 a b c d e f) = T6 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f)

data T7 a b c d e f g
  = T7 a b c d e f g
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monoid (T7 a b c d e f g) where
  mempty = T7 mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g) => Semigroup (T7 a b c d e f g) where
  T7 a1 b1 c1 d1 e1 f1 g1 <> T7 a2 b2 c2 d2 e2 f2 g2 = T7 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)
  stimes ii (T7 a b c d e f g) = T7 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g)

data T8 a b c d e f g h
  = T8 a b c d e f g h
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monoid (T8 a b c d e f g h) where
  mempty = T8 mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h) => Semigroup (T8 a b c d e f g h) where
  T8 a1 b1 c1 d1 e1 f1 g1 h1 <> T8 a2 b2 c2 d2 e2 f2 g2 h2 = T8 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2)
  stimes ii (T8 a b c d e f g h) = T8 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h)

data T9 a b c d e f g h i
  = T9 a b c d e f g h i
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Monoid (T9 a b c d e f g h i) where
  mempty = T9 mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i) => Semigroup (T9 a b c d e f g h i) where
  T9 a1 b1 c1 d1 e1 f1 g1 h1 i1 <> T9 a2 b2 c2 d2 e2 f2 g2 h2 i2 = T9 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2)
  stimes ii (T9 a b c d e f g h i) = T9 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i)

data T10 a b c d e f g h i j
  = T10 a b c d e f g h i j
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Monoid (T10 a b c d e f g h i j) where
  mempty = T10 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j) => Semigroup (T10 a b c d e f g h i j) where
  T10 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 <> T10 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 = T10 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2)
  stimes ii (T10 a b c d e f g h i j) = T10 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j)

data T11 a b c d e f g h i j k
  = T11 a b c d e f g h i j k
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) => Monoid (T11 a b c d e f g h i j k) where
  mempty = T11 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k) => Semigroup (T11 a b c d e f g h i j k) where
  T11 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 <> T11 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 = T11 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2)
  stimes ii (T11 a b c d e f g h i j k) = T11 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k)

data T12 a b c d e f g h i j k l
  = T12 a b c d e f g h i j k l
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) => Monoid (T12 a b c d e f g h i j k l) where
  mempty = T12 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l) => Semigroup (T12 a b c d e f g h i j k l) where
  T12 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 <> T12 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 = T12 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2)
  stimes ii (T12 a b c d e f g h i j k l) = T12 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l)

data T13 a b c d e f g h i j k l m
  = T13 a b c d e f g h i j k l m
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) => Monoid (T13 a b c d e f g h i j k l m) where
  mempty = T13 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m) => Semigroup (T13 a b c d e f g h i j k l m) where
  T13 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 <> T13 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 = T13 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2)
  stimes ii (T13 a b c d e f g h i j k l m) = T13 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m)

data T14 a b c d e f g h i j k l m n
  = T14 a b c d e f g h i j k l m n
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n) => Monoid (T14 a b c d e f g h i j k l m n) where
  mempty = T14 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n) => Semigroup (T14 a b c d e f g h i j k l m n) where
  T14 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 <> T14 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 = T14 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2)
  stimes ii (T14 a b c d e f g h i j k l m n) = T14 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n)

data T15 a b c d e f g h i j k l m n o
  = T15 a b c d e f g h i j k l m n o
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o) => Monoid (T15 a b c d e f g h i j k l m n o) where
  mempty = T15 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o) => Semigroup (T15 a b c d e f g h i j k l m n o) where
  T15 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 <> T15 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 = T15 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2)
  stimes ii (T15 a b c d e f g h i j k l m n o) = T15 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o)

data T16 a b c d e f g h i j k l m n o p
  = T16 a b c d e f g h i j k l m n o p
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) => Monoid (T16 a b c d e f g h i j k l m n o p) where
  mempty = T16 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p) => Semigroup (T16 a b c d e f g h i j k l m n o p) where
  T16 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 <> T16 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 = T16 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2)
  stimes ii (T16 a b c d e f g h i j k l m n o p) = T16 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p)

data T17 a b c d e f g h i j k l m n o p q
  = T17 a b c d e f g h i j k l m n o p q
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q) => Monoid (T17 a b c d e f g h i j k l m n o p q) where
  mempty = T17 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q) => Semigroup (T17 a b c d e f g h i j k l m n o p q) where
  T17 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 <> T17 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 = T17 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2)
  stimes ii (T17 a b c d e f g h i j k l m n o p q) = T17 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q)

data T18 a b c d e f g h i j k l m n o p q r
  = T18 a b c d e f g h i j k l m n o p q r
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r) => Monoid (T18 a b c d e f g h i j k l m n o p q r) where
  mempty = T18 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q, Semigroup r) => Semigroup (T18 a b c d e f g h i j k l m n o p q r) where
  T18 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 <> T18 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 = T18 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2) (r1 <> r2)
  stimes ii (T18 a b c d e f g h i j k l m n o p q r) = T18 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q) (stimes ii r)

data T19 a b c d e f g h i j k l m n o p q r s
  = T19 a b c d e f g h i j k l m n o p q r s
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q, Monoid r, Monoid s) => Monoid (T19 a b c d e f g h i j k l m n o p q r s) where
  mempty = T19 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q, Semigroup r, Semigroup s) => Semigroup (T19 a b c d e f g h i j k l m n o p q r s) where
  T19 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 <> T19 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 = T19 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2) (r1 <> r2) (s1 <> s2)
  stimes ii (T19 a b c d e f g h i j k l m n o p q r s) = T19 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q) (stimes ii r) (stimes ii s)
