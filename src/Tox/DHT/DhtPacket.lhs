\section{DHT Packet}

The DHT Packet contains the sender's DHT Public Key, an encryption Nonce, and
an encrypted payload.  The payload is encrypted with the DHT secret key of the
sender, the DHT public key of the receiver, and the nonce that is sent along
with the packet.  DHT Packets are sent inside Protocol Packets with a varying
Packet Kind.

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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
module Tox.DHT.DhtPacket where

import qualified Crypto.Saltine.Class      as Sodium
import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (getRemainingLazyByteString)
import           Data.Binary.Put           (putByteString, runPut)
import qualified Data.ByteString.Lazy      as LazyByteString
import           Data.MessagePack          (MessagePack)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Tox.Crypto.Core.Box       as Box
import           Tox.Crypto.Core.Box       (CipherText, PlainText (..),
                                            unCipherText)
import           Tox.Crypto.Core.Key       (Nonce, PublicKey, unKey)
import qualified Tox.Crypto.Core.Keyed     as Keyed
import           Tox.Crypto.Core.Keyed     (Keyed)
import           Tox.Crypto.Core.KeyPair   (KeyPair (..))



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
    putByteString . Sodium.encode . unKey . senderPublicKey $ packet
    putByteString . Sodium.encode . unKey . encryptionNonce $ packet
    putByteString . unCipherText . encryptedPayload $ packet

  get =
    DhtPacket <$> get <*> get <*> (getRemainingLazyByteString >>= Box.cipherText . LazyByteString.toStrict)


encrypt :: KeyPair -> PublicKey -> Nonce -> PlainText -> DhtPacket
encrypt = (((Keyed.runNullKeyed .) .) .) . encryptKeyed

encryptKeyed :: Keyed m => KeyPair -> PublicKey -> Nonce -> PlainText -> m DhtPacket
encryptKeyed (KeyPair senderSecretKey senderPublicKey') receiverPublicKey nonce plainText =
  (\combinedKey -> DhtPacket senderPublicKey' nonce $
    Box.encrypt combinedKey nonce plainText) <$>
  Keyed.getCombinedKey senderSecretKey receiverPublicKey


encode :: Binary payload => KeyPair -> PublicKey -> Nonce -> payload -> DhtPacket
encode = (((Keyed.runNullKeyed .) .) .) . encodeKeyed

encodeKeyed :: (Binary payload, Keyed m) => KeyPair -> PublicKey -> Nonce -> payload -> m DhtPacket
encodeKeyed keyPair receiverPublicKey nonce =
  encryptKeyed keyPair receiverPublicKey nonce
  . PlainText
  . LazyByteString.toStrict
  . runPut
  . put


decrypt :: KeyPair -> DhtPacket -> Maybe PlainText
decrypt = (Keyed.runNullKeyed .) . decryptKeyed

decryptKeyed :: Keyed m => KeyPair -> DhtPacket -> m (Maybe PlainText)
decryptKeyed (KeyPair receiverSecretKey _) DhtPacket { senderPublicKey, encryptionNonce, encryptedPayload } =
  (\combinedKey -> Box.decrypt combinedKey encryptionNonce encryptedPayload) <$>
  Keyed.getCombinedKey receiverSecretKey senderPublicKey


decode :: Binary payload => KeyPair -> DhtPacket -> Maybe payload
decode = (Keyed.runNullKeyed .) . decodeKeyed

decodeKeyed :: (Binary payload, Keyed m) => KeyPair -> DhtPacket -> m (Maybe payload)
decodeKeyed keyPair packet = (>>= Box.decode) <$> decryptKeyed keyPair packet


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary DhtPacket where
  arbitrary =
    DhtPacket <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}
