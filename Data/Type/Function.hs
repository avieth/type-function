{-|
Module      : Data.Type.Function
Description : Type-level functions.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Function (

      Function
    , F
    , C
    , ApplyFunction
    , At
    , EvalFunction

    , Apply
    , type (:$)

    , Dot
    , type (:.)

    , Id
    , Const

    ) where

import Data.Kind
import Data.Proxy

-- | The kind of type-level functions. This includes constructors (typical 
--   Haskell 98 type constructors and promoted data constructors) and type
--   families. As they're not types, type families must be proxied by a datatype
--   of a special form: one which takes 1 or more arguments before ending in
--   kind Proxy t -> Type, where t is the kind after the rightmost arrow of
--   the function which it represents. See the definitions of Apply, Dot, Id,
--   and Const for some examples.
--
--   Do not use the constructors of this GADT directly, as they do not
--   constraint the type parameters. Instead, use the families C and F, which
--   infer the parameters from the kinds of their arguments. That's C for
--   constructor and F for family.
data Function (s :: Type) (t :: Type) where
    Constructor :: (k -> l) -> Function s t
    Family :: (k -> l) -> Function s t

infixr 0 :->
type s :-> t = Function s t

type family ConstructorCodomain (k :: Type) :: Type where
    ConstructorCodomain (k -> (l -> r)) = Function l (ConstructorCodomain (l -> r))
    ConstructorCodomain (k -> l) = l

type family FamilyCodomain (k :: Type) :: Type where
    FamilyCodomain (Proxy l -> Type) = l
    FamilyCodomain (k -> l) = Function k (FamilyCodomain l)

-- | Make a Function from a type constructor or promoted data constructor.
type family C (f :: k -> l) :: Function k (ConstructorCodomain (k -> l)) where
    C (f :: k -> l) = 'Constructor f

-- | Make a Function from a type family proxy.
type family F (f :: k -> l) :: Function k (FamilyCodomain l) where
    F (f :: k -> l) = 'Family f

type family ApplyFunction (f :: Function s t) (x :: s) :: t where
    ApplyFunction ('Constructor (f :: k -> (l -> r))) (x :: k) = 'Constructor (f x)
    ApplyFunction ('Constructor (f :: k -> l)) (x :: k) = f x
    ApplyFunction ('Family (f :: k -> Proxy l -> Type)) (x :: k) =
        EvalFunction (f x)
    ApplyFunction ('Family (f :: k -> (l -> r))) (x :: k) = 'Family (f x)

type At f x = ApplyFunction f x

type family EvalFunction (d :: Proxy l -> Type) :: l

type Apply = F ApplyProxy
infixr 0 :$
type f :$ x = Apply `At` f `At` x
data ApplyProxy (f :: Function s t) (x :: s) (p :: Proxy t)
type instance EvalFunction (ApplyProxy f x) = f `At` x

type Dot = F DotProxy
infixr 9 :.
type f :. x = Dot `At` f `At` x
data DotProxy (g :: Function t u) (f :: Function s t) (x :: s) (p :: Proxy u)
type instance EvalFunction (DotProxy g f x) = g `At` (f `At` x)

type Id = F IdProxy
data IdProxy (x :: k) (p :: Proxy k)
type instance EvalFunction (IdProxy x) = x

type Const = F ConstProxy
data ConstProxy (x :: k) (y :: l) (p :: Proxy k)
type instance EvalFunction (ConstProxy x y) = x

type Fmap = F FmapProxy
infixl 4 :<$>
type f :<$> x = Fmap `At` f `At` x
data FmapProxy (g :: Function s t) (x :: f s) (p :: Proxy (f t))
type instance EvalFunction (FmapProxy f x) = FmapInstance f x

type family FmapInstance (g :: Function s t) (x :: f s) :: f t
type instance FmapInstance g ('Just x) = 'Just (g `At` x)
type instance FmapInstance g 'Nothing = 'Nothing

type Ap = F ApProxy
infixl 4 :<*>
type mf :<*> mx = Ap `At` mf `At` mx
data ApProxy (mf :: f (Function s t)) (mx :: f s) (p :: Proxy (f t))
type instance EvalFunction (ApProxy mf mx) = ApInstance mf mx

type family ApInstance (mf :: f (Function s t)) (x :: f s) :: f t
type instance ApInstance 'Nothing 'Nothing = 'Nothing
type instance ApInstance ('Just f) 'Nothing = 'Nothing
type instance ApInstance 'Nothing ('Just x) = 'Nothing
type instance ApInstance ('Just f) ('Just x) = 'Just (f `At` x)
