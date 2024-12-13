{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE

     CPP

   , FlexibleInstances

   , DataKinds
   , InstanceSigs
   , KindSignatures
   , MultiParamTypeClasses
   , TypeFamilies, NoMonoLocalBinds
   , TypeSynonymInstances
   , UndecidableInstances
   , TypeOperators
   , NoMonomorphismRestriction
   #-}

module Vivid.SynthDef.ToSig (
     ToSig(..)
   ) where

import Vivid.SC.Server.Types (BufferId(..))
import Vivid.SynthDef.Types

import qualified Data.ByteString.UTF8 as UTF8
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
-- import Data.Proxy
import GHC.TypeLits
import Numeric.Natural (Natural)

class ToSig s (args :: [Symbol]) where
   toSig :: s -> SDBody' args Signal

instance ToSig Signal args where
   toSig :: Signal -> SDBody' args Signal
   toSig = pure

instance (KnownSymbol a, Subset '[a] args) => ToSig (Variable a) args where
   toSig a = (return . Param . UTF8.fromString . symbolVal) a

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 801

realSig :: (Real n, Applicative f) => n -> f Signal
realSig = pure . Constant . realToFrac

instance ToSig Double       args where toSig = realSig
instance ToSig Float        args where toSig = realSig

instance ToSig Rational     args where toSig = realSig

instance ToSig Integer      args where toSig = realSig
instance ToSig Natural      args where toSig = realSig

instance ToSig Int          args where toSig = realSig
instance ToSig Int8         args where toSig = realSig
instance ToSig Int16        args where toSig = realSig
instance ToSig Int32        args where toSig = realSig
instance ToSig Int64        args where toSig = realSig

instance ToSig Word         args where toSig = realSig
instance ToSig Word8        args where toSig = realSig
instance ToSig Word16       args where toSig = realSig
instance ToSig Word32       args where toSig = realSig
instance ToSig Word64       args where toSig = realSig

#else


-- Incoherent is to get numbers defaulting to Floats in a useful way in
-- SynthDefs. The type resolution algorithm should rarely (discovery: not
-- never!) give weird behavior as long as other instances aren't defined:

-- | For 'Constant' (Float) values
instance {-# INCOHERENT #-} (Num n, Real n) => ToSig n args where
   toSig :: n -> SDBody' args Signal
   toSig = return . Constant . realToFrac

#endif

-- This way instead of e.g.
-- > BufferId b <- makeBuffer 1
-- > playBuf (buf_ $ toEnum $ fromEnum b
--
-- we can say:
-- > b <- makeBuffer 1
-- > playBuf (buf_ b
-- instance ToSomeSig BufferId where
instance ToSig BufferId args where
   toSig :: BufferId -> SDBody' args Signal
   toSig (BufferId n) = (return . Constant . realToFrac) n

instance (a ~ args) => ToSig (SDBody' a Signal) args where
   toSig :: SDBody' args Signal -> SDBody' args Signal
   toSig x = x
