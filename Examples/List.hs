{-|
Module      : Examples.List
Description : An example of type-level list functions.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples.List where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.Type.Function

type Foldr = F FoldrProxy
data FoldrProxy (f :: a :-> b :-> b) (r :: b) (xs :: [a]) (p :: Proxy b)
type instance EvalFunction (FoldrProxy f r '[]) = r
type instance EvalFunction (FoldrProxy f r (x ': xs)) = f `At` x `At` (Foldr `At` f `At` r `At` xs)

type Plus = F PlusProxy
data PlusProxy (n :: Nat) (m :: Nat) (p :: Proxy Nat)
type instance EvalFunction (PlusProxy n m) = n + m

-- Six ~ 6 :: Nat
type Six = Foldr `At` Plus `At` 0 `At` '[1,2,3]

type Append = F AppendProxy
data AppendProxy (xs :: [k]) (ys :: [k]) (p :: Proxy [k])
type instance EvalFunction (AppendProxy '[] ys) = ys
type instance EvalFunction (AppendProxy (x ': xs) ys) = x ': Append `At` xs `At` ys

type Concat = F ConcatProxy
data ConcatProxy (xs :: [[k]]) (p :: Proxy [k])
type instance EvalFunction (ConcatProxy '[]) = '[]
type instance EvalFunction (ConcatProxy (xs ': xss)) = Append `At` xs `At` (Concat `At` xss)

type instance BindInstance (xs :: [l]) k = Concat `At` (k :<$> xs)

-- BindExample ~ '[1,2,3] :: [Nat]
type BindExample = '[1,2,3] :>>= (Pure `At` ('Proxy :: Proxy []))
