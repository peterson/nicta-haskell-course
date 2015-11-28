{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
  (<$>) ::
    (a -> b)
    -> Id a
    -> Id b
  -- get the 'a' out of Id a (using a pattern match)
  -- and then wrap (f a) in Id, to give an Id (b)
  (<$>) f (Id a) =
    Id (f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  -- When you're here, you're using constructors, not types, so List will never appear
  -- List is a data-type that has two constructors, 'Nil' and ':.' (cons)
  (<$>) _ Nil = Nil
  (<$>) f (h:.t) =
    f h :. ( f <$> t )
  -- or you can do the same using a fold ...
  -- (<$>) f =
  --   foldRight ( (:.) . f ) Nil -- aka map


-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  -- Similarly, Optional's constructors are 'Full' and 'Empty'
  (<$>) _ (Empty)  = Empty
  (<$>) f (Full a) =
    Full (f a)


-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  -- z is a fn (t -> a), f is a fn (a -> b), therefore f . z is fn (t -> b) ... ;)
  (<$>) f z =
    f . z
  -- or, in fully point-free style
  -- (<$>) =
  --   (.)



-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> x <$ Full q == Full x
--
-- i.e. throw away the values in functor (f b) and return a functor (f a)
-- it's like doing a map without even looking at the elements of the functor
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) a g =
  const a <$> g

-- or, using point-free
-- (<$) =
--   (<$>) . const


-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  (<$) () -- map across functor (f a) and fill it with ()'s

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
