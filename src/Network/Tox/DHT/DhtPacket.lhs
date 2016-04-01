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
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Trustworthy    #-}
module Network.Tox.DHT.DhtPacket where

import           Control.Applicative            ((<$>), (<*>))
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Binary                    (Binary, get, put)
import           Data.Binary.Get                (Decoder (..),
                                                 getRemainingLazyByteString,
                                                 pushChunk, runGetIncremental)
import           Data.Binary.Put                (putByteString, putByteString,
                                                 runPut)
import qualified Data.ByteString.Lazy           as LazyByteString
import           GHC.Generics                   (Generic)
import qualified Network.Tox.Crypto.Box         as Box
import qualified Network.Tox.Crypto.CombinedKey as CombinedKey
import           Network.Tox.Crypto.Key         (Nonce, PublicKey)
import           Network.Tox.Crypto.KeyPair     (KeyPair (..))
import           Network.Tox.Crypto.Text        (CipherText (..),
                                                 PlainText (..))
import           Test.QuickCheck.Arbitrary      (Arbitrary, arbitrary)



{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


data DhtPacket payload = DhtPacket
  { senderPublicKey  :: PublicKey
  , encryptionNonce  :: Nonce
  , encryptedPayload :: CipherText payload
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON (DhtPacket payload)
instance FromJSON (DhtPacket payload)


instance Binary (DhtPacket payload) where
  put DhtPacket { senderPublicKey, encryptionNonce, encryptedPayload = CipherText bytes } = do
    put senderPublicKey
    put encryptionNonce
    putByteString bytes

  get =
    DhtPacket <$> get <*> get <*> (CipherText . LazyByteString.toStrict <$> getRemainingLazyByteString)


encrypt :: KeyPair -> PublicKey -> Nonce -> PlainText payload -> DhtPacket payload
encrypt (KeyPair senderSecretKey senderPublicKey') receiverPublicKey nonce plainText =
  DhtPacket senderPublicKey' nonce $ Box.encrypt combinedKey nonce plainText
  where combinedKey = CombinedKey.precompute senderSecretKey receiverPublicKey


encode :: Binary payload => KeyPair -> PublicKey -> Nonce -> payload -> DhtPacket payload
encode keyPair receiverPublicKey nonce payload =
  encrypt keyPair receiverPublicKey nonce
    $ PlainText $ LazyByteString.toStrict $ runPut $ put payload


decrypt :: KeyPair -> DhtPacket payload -> Maybe (PlainText payload)
decrypt (KeyPair receiverSecretKey _) DhtPacket { senderPublicKey, encryptionNonce, encryptedPayload } =
  Box.decrypt combinedKey encryptionNonce encryptedPayload
  where combinedKey = CombinedKey.precompute receiverSecretKey senderPublicKey


decode :: Binary payload => KeyPair -> DhtPacket payload -> Maybe payload
decode keyPair packet = do
  PlainText bytes <- decrypt keyPair packet
  case pushChunk (runGetIncremental get) bytes of
    Fail {}         -> Nothing
    Partial _       -> Nothing
    Done _ _ output -> Just output


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary (DhtPacket payload) where
  arbitrary =
    DhtPacket <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}
