{-|
Module      : Examples.Parser
Description : Type level parsing.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}


module Examples.List where

import GHC.TypeLits hiding (SplitSymbol)
import qualified GHC.TypeLits as TypeLits
import Data.Kind
import Data.Proxy
import Data.Type.Function

data Parser (t :: Type) where
    Parser :: (Symbol :-> Maybe (t, Symbol)) -> Parser t

type RunParser = F RunParserProxy
data RunParserProxy (p :: Parser t) (q :: Proxy (Symbol :-> Maybe (t, Symbol)))
type instance EvalFunction (RunParserProxy ('Parser f)) = f

-- Parser is a functor.
type instance FmapInstance g ('Parser f) =
    'Parser ((Fmap `At` (Swap :. (Fmap `At` g) :. Swap)) :. f)

-- Parser is an applicative.
type instance PureInstance Parser t = 'Parser (C 'Just :. (C '(,) `At` t))
-- Tuple the output of mf and mx, then apply the former to the latter.
--   mf >>= fmap applyTuple . flip fmap mx . (,)
type instance ApInstance (mf :: Parser (s :-> t)) (mx :: Parser s) =
    Bind `At` mf `At` ((Fmap `At` ApplyTuple) :. (Flip `At` Fmap `At` mx) :. C '(,))

-- Parser is a monad.
type instance BindInstance (p :: Parser t) k = ParserJoin `At` (Fmap `At` k `At` p)

type ParserJoin = F ParserJoinProxy
data ParserJoinProxy (p :: Parser (Parser t)) (q :: Proxy (Parser t))
-- \p -> (=<<) (uncurry runParser) . runParser p
type instance EvalFunction (ParserJoinProxy p) = 'Parser (
      ((Flip `At` Bind) `At` (Uncurry `At` RunParser)) :. (RunParser `At` p)
    )

type SplitSymbol = F SplitSymbolProxy
data SplitSymbolProxy (s :: Symbol) (p :: Proxy (Maybe (Character, Symbol)))
type instance EvalFunction (SplitSymbolProxy s) = TypeLits.SplitSymbol s

type ParserCharacter = 'Parser SplitSymbol

-- :kind! RunParser `At` ((Const `At` 'True) :<$> ParserCharacter) `At` "hello"
-- = 'Just '('True, "ello")
