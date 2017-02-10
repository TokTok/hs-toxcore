\section{DHT Request packets}

A DHT Request packet is a Protocol Packet. It contains the sender's DHT Public
Key, the DHT Public Key of an addressee, an encryption Nonce, and an encrypted
payload.  The payload is encrypted with the DHT secret key of the sender, the
DHT public key of the addressee, and the nonce that is sent along with the
packet.

When constructing a DHT Request packet, the nonce used should be randomly
generated (TODO: put this along with the code which does the random generation).

\begin{tabular}{l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{1}         & \texttt{uint8_t} (0x20) \\
  \texttt{32}        & addressee DHT public key \\
  \texttt{32}        & sender's DHT public key \\
  \texttt{24}        & nonce \\
  \texttt{?}         & encrypted payload \\
\end{tabular}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.DhtRequestPacket where

import           Control.Applicative            ((<$>), (<*>))
import           Data.Binary                    (Binary, get, put)
import           Data.Binary.Get                (getRemainingLazyByteString)
import           Data.Binary.Put                (putByteString, putByteString,
                                                 runPut)
import qualified Data.ByteString.Lazy           as LazyByteString
import           Data.MessagePack               (MessagePack)
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           Network.Tox.Crypto.Box         (CipherText, PlainText (..),
                                                 unCipherText)
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.Key         (Nonce, PublicKey)
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary)



{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data DhtRequestPacket = DhtRequestPacket
  { addresseePublicKey :: PublicKey
  , senderPublicKey    :: PublicKey
  , encryptionNonce    :: Nonce
  , encryptedPayload   :: CipherText
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance MessagePack DhtRequestPacket


instance Binary DhtRequestPacket where
  put packet = do
    put $ addresseePublicKey packet
    put $ senderPublicKey packet
    put $ encryptionNonce packet
    putByteString . unCipherText . encryptedPayload $ packet

  get =
    DhtRequestPacket <$> get <*> get <*> (LazyByteString.toStrict <$> getRemainingLazyByteString >>= Box.cipherText)


encrypt :: KeyPair -> PublicKey -> Nonce -> PlainText -> DhtRequestPacket
encrypt (KeyPair senderSecretKey senderPublicKey') addresseePublicKey nonce plainText =
  DhtRequestPacket senderPublicKey' nonce $ Box.encrypt combinedKey nonce plainText
  where combinedKey = CombinedKey.precompute senderSecretKey addresseePublicKey


encode :: Binary payload => KeyPair -> PublicKey -> Nonce -> payload -> DhtRequestPacket
encode keyPair addresseePublicKey nonce =
  encrypt keyPair addresseePublicKey nonce
  . PlainText
  . LazyByteString.toStrict
  . runPut
  . put


decrypt :: KeyPair -> DhtRequestPacket -> Maybe PlainText
decrypt (KeyPair receiverSecretKey _) DhtRequestPacket { senderPublicKey, encryptionNonce, encryptedPayload } =
  Box.decrypt combinedKey encryptionNonce encryptedPayload
  where combinedKey = CombinedKey.precompute receiverSecretKey senderPublicKey


decode :: Binary payload => KeyPair -> DhtRequestPacket -> Maybe payload
decode keyPair packet = decrypt keyPair packet >>= Box.decode


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtRequestPacket where
  arbitrary =
    DhtRequestPacket <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
\end{code}
