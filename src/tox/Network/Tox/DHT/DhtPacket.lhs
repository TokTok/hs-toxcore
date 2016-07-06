\section{DHT Packet}

The DHT Packet contains the sender's DHT Public Key, an encryption Nonce, and
an encrypted payload.  The payload is encrypted with the the DHT secret key of
the sender, the DHT public key of the receiver, and the nonce that is sent
along with the packet.  DHT Packets are sent inside Protocol Packets with a
varying Packet Kind.

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{32}        & Public Key  & Sender DHT Public Key \\
  \texttt{24}        & Nonce       & Random nonce \\
  \texttt{[16,]}     & Bytes       & Encrypted payload \\
\end{tabular}

The encrypted payload is at least 16 bytes long, because the encryption
includes a \href{https://en.wikipedia.org/wiki/Message_authentication_code}{MAC}
of 16 bytes.  A 16 byte payload would thus be the empty message.  The DHT
protocol never actually sends empty messages, so in reality the minimum size is
27 bytes for the \href{#ping-service}{Ping Packet}.

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE Safe               #-}
module Network.Tox.DHT.DhtPacket where

import           Control.Applicative            ((<$>), (<*>))
import           Data.Binary                    (Binary, get, put)
import           Data.Binary.Get                (Decoder (..),
                                                 getRemainingLazyByteString,
                                                 pushChunk, runGetIncremental)
import           Data.Binary.Put                (putByteString, putByteString,
                                                 runPut)
import qualified Data.ByteString.Lazy           as LazyByteString
import           Data.MessagePack               (MessagePack)
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.Key         (Nonce, PublicKey)
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import           Network.Tox.Crypto.Text        (CipherText (..),
                                                 PlainText (..))
import qualified Network.Tox.Crypto.Text        as Text
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary)



{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data DhtPacket = DhtPacket
  { senderPublicKey  :: PublicKey
  , encryptionNonce  :: Nonce
  , encryptedPayload :: CipherText
  }
  deriving (Eq, Read, Show, Generic, Typeable)

instance MessagePack DhtPacket


instance Binary DhtPacket where
  put packet = do
    put $ senderPublicKey packet
    put $ encryptionNonce packet
    putByteString . unCipherText . encryptedPayload $ packet

  get =
    DhtPacket <$> get <*> get <*> (CipherText . LazyByteString.toStrict <$> getRemainingLazyByteString)


encrypt :: KeyPair -> PublicKey -> Nonce -> PlainText -> DhtPacket
encrypt (KeyPair senderSecretKey senderPublicKey') receiverPublicKey nonce plainText =
  DhtPacket senderPublicKey' nonce $ Box.encrypt combinedKey nonce plainText
  where combinedKey = CombinedKey.precompute senderSecretKey receiverPublicKey


encode :: Binary payload => KeyPair -> PublicKey -> Nonce -> payload -> DhtPacket
encode keyPair receiverPublicKey nonce =
  encrypt keyPair receiverPublicKey nonce
  . PlainText
  . LazyByteString.toStrict
  . runPut
  . put


decrypt :: KeyPair -> DhtPacket -> Maybe PlainText
decrypt (KeyPair receiverSecretKey _) DhtPacket { senderPublicKey, encryptionNonce, encryptedPayload } =
  Box.decrypt combinedKey encryptionNonce encryptedPayload
  where combinedKey = CombinedKey.precompute receiverSecretKey senderPublicKey


decode :: Binary payload => KeyPair -> DhtPacket -> Maybe payload
decode keyPair packet = decrypt keyPair packet >>= Text.decode


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtPacket where
  arbitrary =
    DhtPacket <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}
