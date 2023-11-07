\subsection{Bytes}

Arbitrary byte array.

\begin{code}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Network.Tox.SaveData.Bytes
    ( Bytes (..)
    ) where

import           Data.Binary               (Binary (..))
import qualified Data.Binary.Get           as Get
import qualified Data.Binary.Put           as Put
import qualified Data.ByteString.Lazy      as LBS
import           Data.MessagePack          (MessagePack)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

newtype Bytes = Bytes LBS.ByteString
    deriving (Eq, Show, Read, Generic)

instance MessagePack Bytes

instance Binary Bytes where
    get = Bytes <$> Get.getRemainingLazyByteString
    put (Bytes bs) = Put.putLazyByteString bs

instance Arbitrary Bytes where
    arbitrary = Bytes . LBS.pack <$> arbitrary

\end{code}
