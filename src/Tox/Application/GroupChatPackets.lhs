\chapter{DHT Group Chats Packet Protocols}

All packet fields are considered mandatory unless flagged as \textbf{\verb'[optional]'}.
The minimum size of an encrypted packet is 83 bytes for lossless and 75
bytes for lossy. The maximum size of an encrypted packet is 1400 bytes.

\section{Full Packet Structure}

\subsection{Plaintext header}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & Packet Kind \\
  \texttt{32}        & Sender's Public Encryption Key \\
  \texttt{32}        & Receiver's Public Encryption Key \textbf{\verb'[optional]'} \\
  \texttt{24}        & Nonce \\
\end{tabular}

\subsection{Encrypted header}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{0-8}       & Padding \\
  \texttt{1}         & Group Packet Identifier \\
  \texttt{8}         & Message Id \textbf{\verb'[optional]'} \\
\end{tabular}

\subsection{Encrypted payload}
\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  Variable           & Payload \\
\end{tabular}

The plaintext header contains a \textbf{\verb'Toxcore Network Packet Kind'}
which identifies the toxcore networking level packet type. These types
are:
\begin{tabular}{l|l}
  Type                                    & Net Packet ID \\
  \hline
  \textbf{\verb'NET_PACKET_GC_HANDSHAKE'} & 0x5a \\
  \textbf{\verb'NET_PACKET_GC_LOSSLESS'}  & 0x5b \\
  \textbf{\verb'NET_PACKET_GC_LOSSY'}     & 0x5c \\
\end{tabular}

The sender's public encryption key is used to identify the peer who sent
the packet, as well as to identify the group instance for which the packet
is intended for all \textbf{\verb'NET_PACKET_GC_LOSSLESS'}
and \textbf{\verb'NET_PACKET_GC_LOSSY'} packets. It is also used to establish
a secure connection with the sender during the handshake protocol.

The receiver's public encryption key is only sent in
\textbf{\verb'NET_PACKET_GC_HANDSHAKE'} packets, and is used to identify
the group instance for which the packet is intended.

The encrypted header for lossless and lossy packets contains between 0
and 8 bytes of empty padding. The \textbf{\verb'Group Packet Identifier'}
is used to identify the type of group packet, and the
\textbf{\verb'Message ID'} is a unique packet identifier which must be
included in the lossless packet header. The message ID must begin at 1,
and increment by 1 after each new lossless packet is successfully sent.

The encrypted payload contains arbitrary data specific to the respective
group packet identifier. The length may range from zero to the maximum
packet size (minus the headers). These payloads will be the focus of the
remainder of this document.

\section{Handshake packet payloads}

Handshake packet payloads are structured as follows:

\subsection{HANDSHAKE\_REQUEST (0x00) and HANDSHAKE\_RESPONSE (0x01)}
\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{32}           & Public Session Key \\
  \texttt{32}           & Public Signature Key \\
  \texttt{1}            & Request Type \\
  \texttt{1}            & Join Type \\
  variable              & 1 Packed TCP Relay \\
\end{tabular}
\hline
This packet type is used to initiate a secure connection with a peer.

The \textbf{\verb'Public Session Key'} is a temporary key unique to this peer
which, along with its secret counterpart, will be used to create a
shared session encryption key. This keypair is used for all further
communication for the current session. It must only be used for a single
peer, and must be discarded of once the connection with the peer is
severed.

The \textbf{\verb'Public Signature Key'} is our own permanent signature key
for this group chat.

The \textbf{\verb'Request Type'} is an identifier for the type of handshake
being initiated, defined as an enumerator starting at zero as follows:

\begin{tabular}{l|l}
  Type                                          & ID \\
  \hline
  \textbf{\verb'HANDSHAKE_INVITE_REQUEST'}      & 0x00 \\
  \textbf{\verb'HANDSHAKE_PEER_INFO_EXCHANGE'}  & 0x01 \\
\end{tabular}

If the request type is an invite request, the receiving peer must
respond with a \textbf{\verb'INVITE_REQUEST'} packet. If the request type is a
peer info exchange, the receiving peer must respond with a
\textbf{\verb'PEER_INFO_RESPONSE'} packet followed immediately by a
\textbf{\verb'PEER_INFO_REQUEST'} packet.

The \textbf{\verb'Join Type'} indicates whether the initiator of a handshake
is joining via the public DHT or a private friend invite. This field is ignored
in handshake response packets. It is defined as an enumerator beginning at zero
as follows:

\begin{tabular}{l|l}
  Type                        & ID \\
  \hline
  \textbf{\verb'PUBLIC'}      & 0x00 \\
  \textbf{\verb'PRIVATE'}     & 0x01 \\
\end{tabular}

The packed TCP relay contains a TCP relay that the sender may be
connected through by the receiver.

\section{Lossy Packet Payloads}

\subsection{PING (0x01)}
A ping packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{2}            & Peerlist Checksum \\
  \texttt{2}            & Confirmed Peer Count \\
  \texttt{4}            & Shared State Version \\
  \texttt{4}            & Sanctions Credentials Version \\
  \texttt{2}            & Peer Roles Checksum \\
  \texttt{4}            & Topic Version \\
  \texttt{2}            & Topic Checksum \\
  Variable              & Packed IP Address and Port \\
\end{tabular}

Ping packets are sent every 12 seconds to every confirmed peer in order to
maintain peer connections, and to ensure the group state between peers
are in sync. A peer is considered to be disconnected from the group after
a ping packet has not been receieved over a period of time.

\subsection{MESSAGE\_ACK (0x02)}

Message ack packets are structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{8}            & Message ID \\
  \texttt{1}            & Type \\
\end{tabular}

This packet ensures that all lossless packets are successfully
received and processed in sequential order as they were sent.

Message ack types are defined by an enumerator beginning at zero as
follows:

\begin{tabular}{l|l}
  Type                          & ID \\
  \hline
  \textbf{\verb'GR_ACK_RECV'}   & 0x00 \\
  \textbf{\verb'GR_ACK_REQ'}    & 0x01 \\
\end{tabular}

If the type is \textbf{\verb'GR_ACK_RECV'}, this indicates that the packet
with the given id has been received and successfully processed. If the
type is \textbf{\verb'GR_ACK_REQ'}, this indicates that the message with the
given id should be sent again.

\subsection{INVITE\_RESPONSE\_REJECT (0x03)}

An invite response reject payload is structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{1}            & Type \\
\end{tabular}

This packet alerts a peer that their invite request has been rejected. The
reason for the rejection is specified by the \textbf{\verb'type'} field.

Rejection types are defined by an enumerator beginning at zero as
follows:
\begin{tabular}{l|l}
  Type                              & ID \\
  \hline
  \textbf{\verb'GROUP_FULL'}        & 0x00 \\
  \textbf{\verb'INVALID_PASSWORD'}  & 0x01 \\
  \textbf{\verb'INVITE_FAILED'}     & 0x02 \\
\end{tabular}

\section{Lossless Packet Payloads}

\subsection{CUSTOM\_PRIVATE\_PACKET (0xee)}

A custom private packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  Variable              & Arbitrary Data \\
\end{tabular}

This packet is used to to send arbitrary data to a specific peer. It may be
used for client-side features. A custom private packet must be greater than 0 bytes,
and may not exceed \textbf{\verb'TOX_MAX_CUSTOM_PACKET'} bytes.

\subsection{FRAGMENT (0xef)}

Fragment packets are structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{1}            & Lossless Packet Type \textbf{\verb'[First chunk only]'} \\
  Variable              & Arbitrary data \\
\end{tabular}

Represents a segment in a sequence of packet fragments that comprise one
full lossless packet payload which exceeds the maximum allowed packet
chunk size (500 bytes). The first byte in the first chunk must be a lossless
packet type. Each chunk in the sequence must be sent in succession.

The end of the sequence is signaled by a fragment packet with a length of zero.

A fully assembled packet must be no greater than 50,000 bytes.

\subsection{KEY\_ROTATIONS (0xf0)}

Key rotation packets are structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{1}            & \textbf{\verb'is_response'} \\
  \texttt{32}           & Public Encryption Key \\
\end{tabular}

Key rotation packets are used to rotate session encryption keys with
a peer. If \textbf{\verb'is_response'} is zero, the packet initiates a
public key exchange. Otherwise, a non-zero value indicates that the packet
is a response to a previously initiated exchange.

The public encryption key must be a newly generated key which takes
the place of the previously used session key. The resulting shared
session key is generated using the same protocol as the initial
handshake, and must be kept secret.

Request packets should be sent at least once every 10 minutes, and only by the
peer whose permanent public encryption key for the given group is closer to
the group \textbf{\verb'Chat ID'} according to the
\href{#distance}{\texttt{Distance}} metric.

If the receiving peer does not receive a key rotation request packet after more
than 10 minutes plus 12 seconds since the last successful rotation, the connection
with the peer should be severed.

\subsection{TCP\_RELAYS (0xf1)}

A TCP relay packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  Variable              & Packed TCP Relays \\
\end{tabular}

The purpose of this packet is to share a list of TCP relays with a
confirmed peer. Used to maintain a list of mutual TCP relays with
other peers, which are used to maintain TCP connections when direct
connections cannot be established.

This packet is sent to every confirmed peer whenever a new TCP relay is
added to our list, or periodically when we presently have no shared TCP
relays with a given peer.

\subsection{CUSTOM\_PACKETS (0xf2)}

A custom packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  Variable              & Arbitrary Data \\
\end{tabular}

This packet is used to send arbitrary data to the group. It may be
used for client-side features. A custom packet must be greater than 0 bytes,
and may not exceed \textbf{\verb'TOX_MAX_CUSTOM_PACKET'} bytes.

\subsection{BROADCAST (0xf3)}

A broadcast packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{1}             & Type \\
  Variable               & Payload \\
\end{tabular}

This packet broadcasts a message to all confirmed peers in a group (with the
exception of \textbf{\verb'PRIVATE_MESSAGE'}). The type of broadcast is
specificed by the \textbf{\verb'type'} field.

Broadcast types are defined and structured as follows:

\paragraph{STATUS (0x00)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{1}             & User status \\
\end{tabular}

Indicates that a peer has changed their status. Statuses must be of type
\textbf{\verb'USERSTATUS'}.

\paragraph{NICK (0x01)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  Variable               & Name \\
\end{tabular}

Indicates that a peer has changed their nickname. A nick must be greater
than 0 bytes, and may not exceed \textbf{\verb'TOX_MAX_NAME_LENGTH'} bytes
in length.

\paragraph{PLAIN\_MESSAGE (0x02)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  Variable               & Arbitrary data \\
\end{tabular}

Contains an arbitrary message. A plain message must be greater than 0
bytes, and may not exceed \textbf{\verb'TOX_MAX_MESSAGE_LENGTH'} bytes.

\paragraph{ACTION\_MESSAGE (0x03)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  Variable               & Arbitrary data \\
\end{tabular}

Contains an arbitrary message. An action message must be greater than 0
bytes, and may not exceed \textbf{\verb'TOX_MAX_MESSAGE_LENGTH'} bytes.

\paragraph{PRIVATE\_MESSAGE (0x04)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  Variable               & Arbitrary data \\
\end{tabular}

Contains an arbitrary message which is only sent to the intended peer. A
private message must be greater than 0 bytes, and may not exceed
\textbf{\verb'TOX_MAX_MESSAGE_LENGTH'} bytes.

\paragraph{PEER\_EXIT (0x05)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  Variable               & Arbitrary data  \textbf{\verb'[optional]'} \\
\end{tabular}

Indicates that a peer is leaving the group. Contains an optional parting
message which may not exceed \textbf{\verb'TOX_GROUP_MAX_PART_LENGTH'}.

\paragraph{PEER\_KICK (0x06)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{32}            & Public Encryption Key \\
\end{tabular}

Indicates that the peer associated with the public encryption key has
been kicked from the group by a moderator or the founder. This peer must
be removed from the peer list.

\paragraph{SET\_MOD (0x07)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{1}             & is_promoted \\
  \texttt{32}            & Public Signature Key \\
\end{tabular}

Indicates that the peer associated with the public signature key has
either been promoted to or demoted from the \textbf{\verb'Moderator'} role
by the group founder. If \textbf{\verb'is_promoted'} is non-zero, the peer should
be promoted and added to the moderator list. Otherwise they should be
demoted to the \textbf{\verb'User'} role and removed from the moderator list.

\paragraph{SET\_OBSERVER (0x08)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{1}             & is_demoted \\
  \texttt{32}            & Public Encryption Key \\
  \texttt{32}            & Public Signature Key \\
  \texttt{137}           & Sanctions List Entry \textbf{\verb'[optional]'} \\
  \texttt{132}           & Packed Sanctions List Credentials \\
\end{tabular}

Indicates that the peer associated with the given public keys has either
been demoted to or promoted from the \textbf{\verb'Observer'} role by the group
founder or a moderator. If \textbf{\verb'is_demoted'} is non-zero, the peer should be
demoted and added to the sanctions list. Otherwise they should be
promoted to the \textbf{\verb'User'} role and removed from the sanctions list.

\subsection{PEER\_INFO\_REQUEST (0xf4)}

A peer info request packet contains an empty payload. Its purpose is to
request a peer to send us information about themselves.

\subsection{PEER\_INFO\_RESPONSE (0xf5)}

A peer info response packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{2}             & Group Password Length \textbf{\verb'[optional]'} \\
  \texttt{32}            & Group Password \textbf{\verb'[optional]'} \\
  \texttt{2}             & Name Length \\
  \texttt{128}           & Name \\
  \texttt{1}             & Status \\
\end{tabular}

This packet supplies information about ourselves to a peer. It is sent
as a response to a \textbf{\verb'PEER_INFO_REQUEST'} or
\textbf{\verb'HS_PEER_INFO_EXCHANGE'} packet as part of the handshake
protocol. A password and length of password must be included in the
packet if the group is password protected.

\subsection{INVITE\_REQUEST (0xf6)}

An invite request packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{2}             & Group Password Length \textbf{\verb'[optional]'} \\
  \texttt{32}            & Group Password \textbf{\verb'[optional]'} \\
\end{tabular}

This packet requests an invite to the group. A password and length of
password must be included in the packet if the group is password protected.

\subsection{INVITE\_RESPONSE (0xf7)}

An invite response packet has an empty payload.

This packet alerts a peer who sent us an \textbf{\verb'INVITE_REQUEST'} packet
that their request has been validated, which informs them that they may
continue to the next step in the handshake protocol.

Before sending this packet we first attempt to validate the invite
request. If validation fails, we instead send a packet of type
\textbf{\verb'INVITE_RESPONSE_REJECT'} in response, and remove the peer from
our peer list.

\subsection{SYNC\_REQUEST (0xf8)}

A sync request packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{2}             & \textbf{\verb'Sync_Flags'} \\
  \texttt{2}             & Group Password Length \textbf{\verb'[optional]'} \\
  \texttt{32}            & Group Password \textbf{\verb'[optional]'} \\
\end{tabular}

This packet asks a peer to send us state information about the group chat.
The specific information being requested is specified via the
\textbf{\verb'Sync_Flags'} field. A password and length of password must be
included in the packet if the group is password protected.

\textbf{\verb'Sync_Flags'} is a bitfield defined as a 16-bit unsigned integer
which may have the bits set for the respective values depending on what
information is being requested:
\begin{tabular}{l|l}
  Type                             & Set Bits \\
  \hline
  \textbf{\verb'PEER_LIST'}        & 0x01 \\
  \textbf{\verb'TOPIC'}            & 0x02 \\
  \textbf{\verb'STATE'}            & 0x04 \\
\end{tabular}

When the \textbf{\verb'STATE'} flag is set, the group's state data must be
sent first, and in the following order: \textbf{\verb'Shared State'},
\textbf{\verb'Moderation List'}, \textbf{\verb'Sanctions List'}.

\subsection{SYNC\_RESPONSE (0xf9)}

A sync response packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{32}            & Public Encryption Key \\
  \texttt{1}             & \textbf{\verb'IP_Port_Is_Set'} \\
  \texttt{1}             & TCP Relays Count \\
  Variable               & Packed IP\_Port \textbf{\verb'[optional]'} \\
  Variable               & Packed TCP Relays \textbf{\verb'[optional]'} \\
\end{tabular}

This packet is sent as a response to a peer who made a sync request via the
\textbf{\verb'SYNC_REQUEST'} packet. It contains a single packed peer
announce, which is a data structure that contains all of the information
about a peer needed to initiate the handshake protocol via TCP relays, a
direct connection, or both.

If the \textbf{\verb'IP_Port_Is_Set'} flag is non-zero, the packet will
contain a packed \textbf{\verb'IP_Port'} of the peer associated with
the given public key. If \textbf{\verb'TCP Relays Count'} is greater than
0, the packet will contain a list of tcp relays that the peer associated
with the given public key is connected to.

When responding to a sync request, one separate sync response will be
sent for each peer in the peer list. All other requested group
information is sent via its respective packet.

\subsection{TOPIC (0xfa)}

A topic packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{64}              & Topic Signature \\
  \texttt{4}               & Topic Version \\
  \texttt{2}               & Topic Checksum \\
  \texttt{2}               & Topic Length \\
  Topic Length             & Topic \\
  \texttt{32}              & Public Signature Key \\
\end{tabular}

This packet contains a topic as well as information used to validate
the topic. Sent when the topic changes, or in response to
a \textbf{\verb'SYNC_REQUEST'} in which the \textbf{\verb'TOPIC'} flag
is set. A topic may not exceed \textbf{\verb'TOX_GROUP_MAX_TOPIC_LENGTH'}
bytes in length.

\subsection{SHARED\_STATE (0xfb)}

A shared state packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{64}              & Shared State Signature \\
  \texttt{4}               & Shared State Version \\
  \texttt{64}              & Founder Extended Public Key \\
  \texttt{2}               & Peer Limit \\
  \texttt{2}               & Group Name Length \\
  \texttt{48}              & Group Name \\
  \texttt{1}               & Privacy State \\
  \texttt{2}               & Group Password Length \\
  \texttt{32}              & Group Password \\
  \texttt{32}              & Moderator List Hash (Sha256) \\
  \texttt{4}               & Topic Lock State \\
  \texttt{1}               & Voice State \\
\end{tabular}

This packet contains information about the group shared state. Sent to
all peers by the group founder whenever the shared state has changed.
Also sent in response to a \textbf{\verb'SYNC_REQUEST'} in which the
\textbf{\verb'STATE'} flag is set.

\subsection{MOD\_LIST (0xfc)}

A moderation list packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{2}               & Moderator Count \\
  Variable                 & Moderator List \\
\end{tabular}

This packet contains information about the moderator list, including
the number of moderators, and a list of public signature keys of all
current moderators. Sent to all peers by the group founder after the
moderator list has been modified. Also sent in response to a
\textbf{\verb'SYNC_REQUEST'} in which the \textbf{\verb'STATE'} flag is set.

The moderator list is comprised of one or more 32 byte public signature
keys.

This packet must always be sent after a \textbf{\verb'SHARED_STATE'} packet,
as the moderator list is validated using data contained within the
shared state.

\subsection{SANCTIONS\_LIST (0xfd)}

A sanctions list packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{2}               & Sanctions List Count \\
  Variable                 & Sanctions List \\
  \texttt{132}             & Packed Sanctions List Credentials \\
\end{tabular}

\paragraph{Sanctions List Entry}
\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{1}               & Type \\
  \texttt{32}              & Public Signature Key \\
  \texttt{8}               & Unix Timestamp \\
  \texttt{32}              & Public Encryption Key \\
  \texttt{64}              & Signature \\
\end{tabular}

\paragraph{Sanctions Credentials}
\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{4}               & Version \\
  \texttt{32}              & Hash (Sha256) \\
  \texttt{2}               & Checksum \\
  \texttt{32}              & Public Signature Key \\
  \texttt{64}              & Signature \\
\end{tabular}

This packet contains information about the sanctions list, including the number
of entries, the sanctions list, and the credentials needed to validate the
sanctions list.

Sanctions types are defined as an enumerator beginning at zero as
follows:

\begin{tabular}{l|l}
  Type                          & ID \\
  \hline
  \textbf{\verb'OBSERVER'}      & 0x00 \\
\end{tabular}

During a sync response, this packet must be sent after a
\textbf{\verb'MOD_LIST'} packet, as the sanctions list is validated using the
moderator list.

\subsection{FRIEND\_INVITE (0xfe)}

A friend invite packet payload is structured as follows:

\begin{tabular}{l|l}
  Length                   & Contents \\
  \hline
  \texttt{1}               & Type \\
\end{tabular}

Used to initiate or respond to a group invite to or from an existing
friend. The invite action is specified by the \textbf{\verb'type'} field.

Invite types are defined as an enumerator beginning at zero as
follows:

\begin{tabular}{l|l}
  Type                                  & ID \\
  \hline
  \textbf{\verb'GROUP_INVITE'}          & 0x00 \\
  \textbf{\verb'GROUP_INVITE_ACCEPTED'} & 0x01 \\
  \textbf{\verb'GROUP_INVITE_CONFIRM'}  & 0x02 \\
\end{tabular}

\subsection{HS\_RESPONSE\_ACK (0xff)}

A handshake response ack packet has an empty payload. This
packet is used to send acknowledgement that a lower level toxcore
\textbf{\verb'NET_PACKET_GC_HANDSHAKE'} packet has been received, which is
the first step in the group handshake protocol. This packet will
initiate an invite request via the \textbf{\verb'INVITE_REQUEST'} packet.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tox.Application.GroupChatPackets where

import           Control.Monad                      (guard, replicateM)
import           Control.Monad.Validate             (runValidate)
import qualified Crypto.Saltine.Class               as Sodium
import qualified Data.Binary                        as Binary
import           Data.Binary.Get                    (Get, getByteString,
                                                     getWord16be, getWord32be,
                                                     getWord64be, getWord8,
                                                     runGetOrFail)
import           Data.Binary.Put                    (Put, putByteString,
                                                     putWord16be, putWord32be,
                                                     putWord32le, putWord64be,
                                                     putWord8, runPut)
import           Data.Bits                          (shiftL, shiftR, xor, (.&.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.List                          (foldl', sort)
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)

import qualified Tox.Crypto.Core.Box                as Box
import           Tox.Crypto.Core.Box                (PlainText (..))
import qualified Tox.Crypto.Core.CombinedKey        as CombinedKey
import           Tox.Crypto.Core.Key                (CombinedKey, Key (..),
                                                     Nonce, PublicKey,
                                                     SecretKey, unKey)
import qualified Tox.Crypto.Core.Sign               as Sign
import qualified Tox.Network.Core.NodeInfo          as NodeInfo
import           Tox.Network.Core.NodeInfo          (NodeInfo)
import qualified Tox.Network.Core.PacketKind        as PacketKind
import qualified Tox.Network.Core.SocketAddress     as SocketAddress
import           Tox.Network.Core.SocketAddress     (SocketAddress)
import           Tox.Network.Core.TransportProtocol (TransportProtocol (..))


{-------------------------------------------------------------------------------
 -
 - :: Constants.
 -
 ------------------------------------------------------------------------------}

-- | Messenger packet id of friend invites, and its sub-header.
packetIdGroupInvite, friendInviteMarker :: Word8
packetIdGroupInvite = 0x5f
friendInviteMarker  = 0xfe

inviteGroup, inviteAccepted, inviteConfirmation :: Word8
inviteGroup        = 0x00
inviteAccepted     = 0x01
inviteConfirmation = 0x02

handshakeRequest, handshakeResponse :: Word8
handshakeRequest  = 0x00
handshakeResponse = 0x01

requestInvite, requestPeerInfoExchange :: Word8
requestInvite           = 0x00
requestPeerInfoExchange = 0x01

joinPublic, joinPrivate :: Word8
joinPublic  = 0x00
joinPrivate = 0x01

-- Lossy packet types.
gpPing, gpMessageAck, gpInviteResponseReject, gpCustomPacket, gpCustomPrivatePacket :: Word8
gpPing                 = 0x01
gpMessageAck           = 0x02
gpInviteResponseReject = 0x03
gpCustomPacket         = 0xf2
gpCustomPrivatePacket  = 0xee

-- Lossless packet types.
gpFragment, gpKeyRotation, gpTcpRelays, gpBroadcast, gpPeerInfoRequest, gpPeerInfoResponse,
  gpInviteRequest, gpInviteResponse, gpSyncRequest, gpSyncResponse, gpTopic, gpSharedState,
  gpModList, gpSanctionsList, gpHsResponseAck :: Word8
gpFragment         = 0xef
gpKeyRotation      = 0xf0
gpTcpRelays        = 0xf1
gpBroadcast        = 0xf3
gpPeerInfoRequest  = 0xf4
gpPeerInfoResponse = 0xf5
gpInviteRequest    = 0xf6
gpInviteResponse   = 0xf7
gpSyncRequest      = 0xf8
gpSyncResponse     = 0xf9
gpTopic            = 0xfa
gpSharedState      = 0xfb
gpModList          = 0xfc
gpSanctionsList    = 0xfd
gpHsResponseAck    = 0xff

ackRecv, ackReq :: Word8
ackRecv = 0x00
ackReq  = 0x01

-- Broadcast types.
bcStatus, bcNick, bcPlainMessage, bcActionMessage, bcPrivateMessage, bcPeerExit, bcKickPeer, bcSetMod,
  bcSetObserver :: Word8
bcStatus         = 0x00
bcNick           = 0x01
bcPlainMessage   = 0x02
bcActionMessage  = 0x03
bcPrivateMessage = 0x04
bcPeerExit       = 0x05
bcKickPeer       = 0x06
bcSetMod         = 0x07
bcSetObserver    = 0x08

syncPeers, syncTopic, syncState :: Word16
syncPeers = 0x01
syncTopic = 0x02
syncState = 0x04

maxNickSize, maxTopicSize, maxGroupNameSize, maxPartSize, maxPasswordSize, maxMessageSize :: Int
maxNickSize      = 128
maxTopicSize     = 512
maxGroupNameSize = 48
maxPartSize      = 128
maxPasswordSize  = 32
maxMessageSize   = 1372

-- | Lossless payloads longer than this go in fragments.
maxChunkSize, maxReassembledSize :: Int
maxChunkSize       = 500
maxReassembledSize = 50000

-- | Padding brings packets towards these sizes, in steps of 8.
losslessPadTarget, lossyPadTarget :: Int
losslessPadTarget = 500
lossyPadTarget    = 1373

defaultMaxPeers, maxModerators :: Word16
defaultMaxPeers = 100
maxModerators   = 100

-- | Field lengths in group packets.
keyLength, chatIdLength, nonceLength, macLength, timeLength :: Int
keyLength       = 32
chatIdLength    = Sign.signPublicKeyLength
nonceLength     = 24
macLength       = 16
timeLength      = 8

-- | A group packet after its kind byte: the sender's key, a nonce and a MAC
-- around the contents, at most toxcore's MAX_GC_PACKET_INCOMING_CHUNK_SIZE
-- and a key.
packetOverhead, maxIncomingChunkSize :: Int
packetOverhead       = keyLength + nonceLength + macLength
maxIncomingChunkSize = 1372

maxCustomPacketSize :: Int
maxCustomPacketSize = 1373

-- | Away and busy come after online.
maxPeerStatus :: Word8
maxPeerStatus = 2

-- | A chat id and a peer's key, as invites carry them.
inviteLength :: Int
inviteLength = chatIdLength + keyLength


{-------------------------------------------------------------------------------
 -
 - :: Encoding helpers.
 -
 ------------------------------------------------------------------------------}

runGetStrict :: Get a -> ByteString -> Maybe a
runGetStrict g bs = case runGetOrFail g (LBS.fromStrict bs) of
  Right (_, _, a) -> Just a
  Left _          -> Nothing

-- | A decoder and what it left.
runGetRest :: Get a -> ByteString -> Maybe (a, ByteString)
runGetRest g bs = case runGetOrFail g (LBS.fromStrict bs) of
  Right (rest, _, a) -> Just (a, LBS.toStrict rest)
  Left _             -> Nothing

putStrict :: Put -> ByteString
putStrict = LBS.toStrict . runPut

cipherMaybe :: ByteString -> Maybe Box.CipherText
cipherMaybe = either (const Nothing) Just . runValidate . Box.cipherText

keyBytes :: Sodium.IsEncoding a => Key a -> ByteString
keyBytes = Sodium.encode . unKey

getKey :: Sodium.IsEncoding a => Get (Key a)
getKey = getByteString keyLength >>= maybe (fail "bad key") (return . Key) . Sodium.decode

-- | The key at an offset.
keyAt :: Sodium.IsEncoding a => Int -> ByteString -> Maybe (Key a)
keyAt offset = fmap fst . runGetRest getKey . BS.drop offset

zeroPadded :: Int -> ByteString -> ByteString
zeroPadded n bs = BS.take n bs <> BS.replicate (n - BS.length bs) 0

-- | toxcore's pair-XOR checksum, taken as a little-endian number.
dataChecksum :: ByteString -> Word16
dataChecksum bs = fromIntegral evens + fromIntegral odds `shiftL` 8
  where
    evens = foldl' xor 0 [ BS.index bs i | i <- [0, 2 .. BS.length bs - 1] ]
    odds = foldl' xor 0 [ BS.index bs i | i <- [1, 3 .. BS.length bs - 1] ]

jenkinsHash :: ByteString -> Word32
jenkinsHash bs = final $ BS.foldl' step 0 bs
  where
    step h b = let h1 = h + fromIntegral b
                   h2 = h1 + h1 `shiftL` 10
               in h2 `xor` (h2 `shiftR` 6)
    final h = let h1 = h + h `shiftL` 3
                  h2 = h1 `xor` (h1 `shiftR` 11)
              in h2 + h2 `shiftL` 15


{-------------------------------------------------------------------------------
 -
 - :: Keys.
 -
 ------------------------------------------------------------------------------}

-- | A group key pair: Ed25519 for signing and its X25519 conversion for
-- encryption.  Peers check that the two belong together.
data ExtKeys = ExtKeys
  { extSign  :: Sign.SignKeyPair
  , extEncPk :: PublicKey
  , extEncSk :: SecretKey
  } deriving (Eq, Show)

extKeysFromSign :: Sign.SignKeyPair -> Maybe ExtKeys
extKeysFromSign keys = do
  pk <- Sign.encPublicKey (Sign.signPublicKey keys) >>= fmap Key . Sodium.decode
  sk <- Sign.encSecretKey (Sign.signSecretKey keys) >>= fmap Key . Sodium.decode
  return $ ExtKeys keys pk sk

extSigPk :: ExtKeys -> ByteString
extSigPk = Sign.signPublicKey . extSign

-- | Whether a signature key converts to the encryption key.
validSigPk :: PublicKey -> ByteString -> Bool
validSigPk encPk sigPk = Sign.encPublicKey sigPk == Just (keyBytes encPk)

-- | The encryption key of a chat, from its chat id.
chatEncPk :: ByteString -> Maybe PublicKey
chatEncPk chatId = Sign.encPublicKey chatId >>= fmap Key . Sodium.decode


{-------------------------------------------------------------------------------
 -
 - :: Outer packets.
 -
 ------------------------------------------------------------------------------}

data Handshake = Handshake
  { hsType        :: Word8
  , hsSessionPk   :: PublicKey
  , hsSigPk       :: ByteString
  , hsRequestType :: Word8
  , hsJoinType    :: Word8
  , hsRelay       :: Maybe NodeInfo
  } deriving (Eq, Show)

packHandshake :: Handshake -> ByteString
packHandshake h = putStrict $ do
  putWord8 (hsType h)
  putByteString (keyBytes $ hsSessionPk h)
  putByteString (hsSigPk h)
  putWord8 (hsRequestType h)
  putWord8 (hsJoinType h)
  mapM_ Binary.put (hsRelay h)

unpackHandshake :: ByteString -> Maybe Handshake
unpackHandshake bs = do
  (h, rest) <- flip runGetRest bs $
    Handshake <$> getWord8 <*> getKey <*> getByteString Sign.signPublicKeyLength <*> getWord8 <*> getWord8 <*> pure Nothing
  return h { hsRelay = if BS.null rest then Nothing else runGetStrict Binary.get rest }

-- | A handshake packet: boxed with the permanent keys of both sides.
wrapHandshake :: PublicKey -> SecretKey -> PublicKey -> Nonce -> Handshake -> ByteString
wrapHandshake ourPk ourSk theirPk nonce h =
  BS.singleton (PacketKind.kindToByte PacketKind.GroupHandshake) <> keyBytes ourPk <> keyBytes theirPk <> keyBytes nonce
    <> Box.unCipherText (Box.encrypt (CombinedKey.precompute ourSk theirPk) nonce (PlainText $ packHandshake h))

-- | The sender, receiver and contents of a handshake packet, after its id
-- byte.  The receiver key says which group it is for.
handshakeKeys :: ByteString -> Maybe (PublicKey, PublicKey)
handshakeKeys = runGetStrict ((,) <$> getKey <*> getKey)

openHandshake :: SecretKey -> ByteString -> Maybe Handshake
openHandshake ourSk payload = do
  (senderPk, _) <- handshakeKeys payload
  nonce <- Key <$> Sodium.decode (BS.take nonceLength $ BS.drop (2 * keyLength) payload)
  ct <- cipherMaybe $ BS.drop (2 * keyLength + nonceLength) payload
  PlainText plain <- Box.decrypt (CombinedKey.precompute ourSk senderPk) nonce ct
  unpackHandshake plain

-- | A lossless or lossy packet: padded, typed, numbered if lossless, and
-- sealed with the session key.
wrapPacket :: Bool -> PublicKey -> CombinedKey -> Nonce -> Word8 -> Maybe Word64 -> ByteString -> ByteString
wrapPacket lossless ourPk shared nonce ty msgId payload =
  BS.singleton (PacketKind.kindToByte $ if lossless then PacketKind.GroupLossless else PacketKind.GroupLossy)
    <> keyBytes ourPk <> keyBytes nonce
    <> Box.unCipherText (Box.encrypt shared nonce (PlainText $ BS.replicate pad 0 <> inner))
  where
    inner = BS.singleton ty <> maybe BS.empty (putStrict . putWord64be) msgId <> payload
    target = if lossless then losslessPadTarget else lossyPadTarget
    pad = if BS.length inner < target then (target - BS.length inner) `mod` 8 else 0

-- | The sender of a lossless or lossy packet, after its id byte.
packetSender :: ByteString -> Maybe PublicKey
packetSender = runGetStrict getKey

-- | Open a lossless or lossy packet, after its id byte: the type, the message
-- id of a lossless one, and the payload.
unwrapPacket :: Bool -> CombinedKey -> ByteString -> Maybe (Word8, Maybe Word64, ByteString)
unwrapPacket lossless shared payload = do
  nonce <- Key <$> Sodium.decode (BS.take nonceLength $ BS.drop keyLength payload)
  ct <- cipherMaybe $ BS.drop (keyLength + nonceLength) payload
  PlainText plain <- Box.decrypt shared nonce ct
  -- The type and, in a lossless packet, its number.
  let headerSize = if lossless then 1 + 8 else 1
      stripped = dropPadding headerSize plain
  (ty, rest) <- BS.uncons stripped
  if lossless
    then do
      (msgId, body) <- runGetRest getWord64be rest
      return (ty, Just msgId, body)
    else return (ty, Nothing, rest)
  where
    dropPadding keep bs
      | BS.length bs > keep, BS.head bs == 0 = dropPadding keep (BS.tail bs)
      | otherwise = bs


{-------------------------------------------------------------------------------
 -
 - :: Group state.
 -
 ------------------------------------------------------------------------------}

data SharedState = SharedState
  { ssVersion     :: Word32
  , ssFounderEnc  :: PublicKey
  , ssFounderSig  :: ByteString
  , ssMaxPeers    :: Word16
  , ssGroupName   :: ByteString
  , ssPrivacy     :: Word8
  , ssPassword    :: ByteString
  , ssModListHash :: ByteString
  , ssTopicLock   :: Word32 -- ^ 0 when the topic lock is on, else the topic version.
  , ssVoice       :: Word8
  } deriving (Eq, Show)

packSharedState :: SharedState -> ByteString
packSharedState s = putStrict $ do
  putWord32be (ssVersion s)
  putByteString (keyBytes $ ssFounderEnc s)
  putByteString (ssFounderSig s)
  putWord16be (ssMaxPeers s)
  putWord16be (fromIntegral . BS.length $ ssGroupName s)
  putByteString (zeroPadded maxGroupNameSize $ ssGroupName s)
  putWord8 (ssPrivacy s)
  putWord16be (fromIntegral . BS.length $ ssPassword s)
  putByteString (zeroPadded maxPasswordSize $ ssPassword s)
  putByteString (ssModListHash s)
  putWord32be (ssTopicLock s)
  putWord8 (ssVoice s)

unpackSharedState :: ByteString -> Maybe SharedState
unpackSharedState = runGetStrict $ do
  version <- getWord32be
  enc <- getKey
  sig <- getByteString Sign.signPublicKeyLength
  maxPeers <- getWord16be
  nameLength <- min (fromIntegral maxGroupNameSize) <$> getWord16be
  name <- BS.take (fromIntegral nameLength) <$> getByteString maxGroupNameSize
  privacy <- getWord8
  passwordLength <- getWord16be
  password <- BS.take (fromIntegral passwordLength) <$> getByteString maxPasswordSize
  hash <- getByteString Sign.sha256Length
  lock <- getWord32be
  voice <- getWord8
  guard $ maxPeers > 0 && passwordLength <= fromIntegral maxPasswordSize && nameLength >= 1 && privacy <= 1 && voice <= maxVoice
  return $ SharedState version enc sig maxPeers name privacy password hash lock voice

-- | Founder, moderators and everyone, in the shared state.
maxVoice :: Word8
maxVoice = 2

sharedStateLength :: Int
sharedStateLength = 4 + keyLength + Sign.signPublicKeyLength + 2 + 2 + maxGroupNameSize + 1 + 2 + maxPasswordSize
  + Sign.sha256Length + 4 + 1

-- | A shared state signed with the chat's signing key.
signSharedState :: Sign.SignKeyPair -> SharedState -> ByteString
signSharedState chatKeys s = let packed = packSharedState s in Sign.sign chatKeys packed <> packed

-- | The shared state in a SHARED_STATE packet, if the chat id signed it.
openSharedState :: ByteString -> ByteString -> Maybe (SharedState, ByteString)
openSharedState chatId payload = do
  let signed = BS.take (Sign.signatureLength + sharedStateLength) payload
      (sig, packed) = BS.splitAt Sign.signatureLength signed
  guard $ BS.length packed == sharedStateLength && Sign.verify chatId sig packed
  s <- unpackSharedState packed
  return (s, signed)

data TopicInfo = TopicInfo
  { tiVersion :: Word32
  , tiTopic   :: ByteString
  , tiSetter  :: ByteString -- ^ Signing key of who set it.
  } deriving (Eq, Show)

packTopicInfo :: TopicInfo -> ByteString
packTopicInfo t = putStrict $ do
  putWord32be (tiVersion t)
  putWord16be (dataChecksum $ tiTopic t)
  putWord16be (fromIntegral . BS.length $ tiTopic t)
  putByteString (tiTopic t)
  putByteString (tiSetter t)

-- | A topic signed by its setter.
signTopic :: Sign.SignKeyPair -> TopicInfo -> ByteString
signTopic keys t = let packed = packTopicInfo t in Sign.sign keys packed <> packed

-- | The topic in a TOPIC packet, if its setter signed it and its checksum is
-- right.
openTopic :: ByteString -> Maybe (TopicInfo, Word16, ByteString)
openTopic payload = do
  let (sig, packed) = BS.splitAt Sign.signatureLength payload
  ((version, checksum, len), rest) <- runGetRest ((,,) <$> getWord32be <*> getWord16be <*> getWord16be) packed
  let topicLength = min maxTopicSize (fromIntegral len)
      headerLength = BS.length packed - BS.length rest
  guard $ BS.length sig == Sign.signatureLength && BS.length rest >= topicLength + Sign.signPublicKeyLength
  let topic = BS.take topicLength rest
      setter = BS.take Sign.signPublicKeyLength (BS.drop topicLength rest)
      signed = BS.take (headerLength + topicLength + Sign.signPublicKeyLength) packed
  guard $ Sign.verify setter sig signed && checksum == dataChecksum topic
  return (TopicInfo version topic setter, checksum, sig <> signed)

-- | The mod list hash of signing keys.
modListHash :: [ByteString] -> ByteString
modListHash []   = BS.replicate Sign.sha256Length 0
modListHash mods = Sign.sha256 (BS.concat $ sort mods)

packModList :: [ByteString] -> ByteString
packModList mods = putStrict (putWord16be . fromIntegral $ length mods) <> BS.concat (sort mods)

unpackModList :: ByteString -> Maybe [ByteString]
unpackModList = runGetStrict $ do
  n <- getWord16be
  guard $ n <= maxModerators
  replicateM (fromIntegral n) (getByteString Sign.signPublicKeyLength)

-- | Who may sanction peers, and a hash over the sanctions, signed.
data Creds = Creds
  { crVersion  :: Word32
  , crHash     :: ByteString
  , crChecksum :: Word16
  , crSigPk    :: ByteString
  , crSig      :: ByteString
  } deriving (Eq, Show)

packCreds :: Creds -> ByteString
packCreds c = putStrict $ do
  putWord32be (crVersion c)
  putByteString (crHash c)
  putWord16be (crChecksum c)
  putByteString (crSigPk c)
  putByteString (crSig c)

getCreds :: Get Creds
getCreds = Creds <$> getWord32be <*> getByteString Sign.sha256Length <*> getWord16be
  <*> getByteString Sign.signPublicKeyLength <*> getByteString Sign.signatureLength

-- | Credentials for no sanctions, signed by a moderator or the founder.
emptyCreds :: Sign.SignKeyPair -> Word32 -> Creds
emptyCreds keys = makeCreds keys []

-- | Credentials for a sanctions list, signed by a moderator or the founder.
makeCreds :: Sign.SignKeyPair -> [Sanction] -> Word32 -> Creds
makeCreds keys sanctions version = Creds version hash (dataChecksum hash) (Sign.signPublicKey keys) (Sign.sign keys hash)
  where
    hash = sanctionsHash sanctions version

-- | The sanctions hash: of the sorted signatures and the version, which is
-- little-endian. Zeros for no sanctions.
sanctionsHash :: [Sanction] -> Word32 -> ByteString
sanctionsHash [] _ = BS.replicate Sign.sha256Length 0
sanctionsHash sanctions version = sanctionsHashOf (sort $ map sanctionSig sanctions) version

sanctionsHashOf :: [ByteString] -> Word32 -> ByteString
sanctionsHashOf sigs version = Sign.sha256 (BS.concat sigs <> putStrict (putWord32le version))

sanctionSig :: Sanction -> ByteString
sanctionSig = BS.drop sanctionBodyLength . snPacked

-- | Whether credentials are signed by one of the allowed keys and match the
-- sanctions, which must be set by allowed keys too. The hash may also be of
-- the signatures in list order.
validCreds :: [ByteString] -> [Sanction] -> Creds -> Bool
validCreds allowed sanctions creds =
  all ((`elem` allowed) . snSetter) sanctions && crSigPk creds `elem` allowed
    && Sign.verify (crSigPk creds) (crSig creds) (crHash creds)
    && (if null sanctions then crHash creds == BS.replicate Sign.sha256Length 0
        else crHash creds `elem` [sanctionsHash sanctions (crVersion creds), sanctionsHashOf (map sanctionSig sanctions) (crVersion creds)])
    && crChecksum creds == dataChecksum (crHash creds)

-- | An observer sanction on the target, set by us at the given time.
makeSanction :: Sign.SignKeyPair -> ByteString -> PublicKey -> Sanction
makeSanction keys time target = Sanction (Sign.signPublicKey keys) target (body <> Sign.sign keys body)
  where
    body = BS.singleton 0 <> Sign.signPublicKey keys <> time <> keyBytes target

-- | The same sanction, set by us instead.
resignSanction :: Sign.SignKeyPair -> Sanction -> Sanction
resignSanction keys sn = makeSanction keys (sanctionTime sn) (snTarget sn)

sanctionTime :: Sanction -> ByteString
sanctionTime = BS.take timeLength . BS.drop (1 + Sign.signPublicKeyLength) . snPacked

-- | The time a sanction we set says it was set: toxcore uses 1, so its
-- signatures do not depend on the clock.
sanctionTimeSet :: ByteString
sanctionTimeSet = putStrict (putWord64be 1)

-- | A sanction: the setter's signature over the rest.
data Sanction = Sanction
  { snSetter :: ByteString
  , snTarget :: PublicKey
  , snPacked :: ByteString -- ^ All 137 bytes.
  } deriving (Eq, Show)

-- | A sanction's type, setter, time and target, before the signature.
sanctionBodyLength :: Int
sanctionBodyLength = 1 + Sign.signPublicKeyLength + timeLength + keyLength

getSanction :: Get Sanction
getSanction = do
  packed <- getByteString (sanctionBodyLength + Sign.signatureLength)
  maybe (fail "bad sanction") return $ do
    let (body, sig) = BS.splitAt sanctionBodyLength packed
    (0, setter, _, target) <- runGetStrict ((,,,) <$> getWord8 <*> getByteString Sign.signPublicKeyLength
                                              <*> getByteString timeLength <*> getKey) body
    guard $ Sign.verify setter sig body
    return $ Sanction setter target packed

-- | The sanctions and their credentials, if the credentials are signed by
-- one of the allowed keys and match the sanctions.
openSanctionsList :: [ByteString] -> ByteString -> Maybe ([Sanction], Creds)
openSanctionsList allowed payload = do
  (sanctions, creds) <- runGetStrict (do n <- getWord16be; guard (n <= maxSanctions); (,) <$> replicateM (fromIntegral n) getSanction <*> getCreds) payload
  guard $ validCreds allowed sanctions creds
  return (sanctions, creds)

maxSanctions :: Word16
maxSanctions = 30

packSanctionsList :: [Sanction] -> Creds -> ByteString
packSanctionsList sanctions creds =
  putStrict (putWord16be . fromIntegral $ length sanctions) <> BS.concat (map snPacked sanctions) <> packCreds creds


{-------------------------------------------------------------------------------
 -
 - :: Peers.
 -
 ------------------------------------------------------------------------------}

-- | A password field: its length and 32 bytes.
packPassword :: ByteString -> ByteString
packPassword pw = putStrict (putWord16be . fromIntegral $ BS.length pw) <> zeroPadded maxPasswordSize pw

-- | A password field at the start, when the sender has a password.
takePassword :: Bool -> ByteString -> Maybe (Maybe ByteString, ByteString)
takePassword False bs = Just (Nothing, bs)
takePassword True bs = do
  guard $ BS.length bs >= 2 + maxPasswordSize
  len <- runGetStrict getWord16be bs
  guard $ fromIntegral len <= maxPasswordSize
  return (Just . BS.take (fromIntegral len) $ BS.drop 2 bs, BS.drop (2 + maxPasswordSize) bs)

packPeerInfo :: ByteString -> Word8 -> ByteString
packPeerInfo nick status = putStrict (putWord16be . fromIntegral $ BS.length nick) <> zeroPadded maxNickSize nick <> BS.singleton status

unpackPeerInfo :: ByteString -> Maybe (ByteString, Word8)
unpackPeerInfo bs = do
  guard $ BS.length bs >= 2 + maxNickSize + 1
  len <- min maxNickSize . fromIntegral <$> runGetStrict getWord16be bs
  let status = BS.index bs (2 + maxNickSize)
  guard $ len > 0 && status <= maxPeerStatus
  return (BS.take len (BS.drop 2 bs), status)

-- | How to reach a peer: its address and a relay it uses, as SYNC_RESPONSE
-- carries them.
data Announce = Announce
  { anPk    :: PublicKey
  , anAddr  :: Maybe SocketAddress
  , anRelay :: Maybe NodeInfo
  } deriving (Eq, Show)

packAnnounce :: Announce -> ByteString
packAnnounce a = putStrict $ do
  putByteString (keyBytes $ anPk a)
  putWord8 (maybe 0 (const 1) $ anAddr a)
  putWord8 (maybe 0 (const 1) $ anRelay a)
  mapM_ (SocketAddress.putSocketAddress UDP) (anAddr a)
  mapM_ Binary.put (anRelay a)

unpackAnnounce :: ByteString -> Maybe Announce
unpackAnnounce = runGetStrict getAnnounce

-- | Announcements one after another, as announce responses carry them.
unpackAnnounces :: ByteString -> Maybe [Announce]
unpackAnnounces bs
  | BS.null bs = Just []
  | otherwise = do
      (a, rest) <- runGetRest getAnnounce bs
      (a :) <$> unpackAnnounces rest

getAnnounce :: Get Announce
getAnnounce = do
  pk <- getKey
  hasAddr <- getWord8
  relays <- getWord8
  addr <- if hasAddr /= 0 then Just . snd <$> SocketAddress.getSocketAddress else return Nothing
  relay <- if relays /= 0 then Just <$> Binary.get else return Nothing
  return $ Announce pk addr (relay >>= \n -> if NodeInfo.protocol n == TCP then Just n else Nothing)

-- | What pings carry, for peers to see what they are missing.
data PingInfo = PingInfo
  { piPeersChecksum :: Word16
  , piConfirmed     :: Word16
  , piStateVersion  :: Word32
  , piCredsVersion  :: Word32
  , piRolesChecksum :: Word16
  , piTopicVersion  :: Word32
  , piTopicChecksum :: Word16
  } deriving (Eq, Show)

packPing :: PingInfo -> ByteString
packPing p = putStrict $ do
  putWord16be (piPeersChecksum p)
  putWord16be (piConfirmed p)
  putWord32be (piStateVersion p)
  putWord32be (piCredsVersion p)
  putWord16be (piRolesChecksum p)
  putWord32be (piTopicVersion p)
  putWord16be (piTopicChecksum p)

unpackPing :: ByteString -> Maybe PingInfo
unpackPing = runGetStrict $
  PingInfo <$> getWord16be <*> getWord16be <*> getWord32be <*> getWord32be <*> getWord16be <*> getWord32be <*> getWord16be

-- | A role's contribution to the roles checksum: its number plus the first
-- byte of the peer's encryption key.
roleChecksum :: Word8 -> PublicKey -> Word16
roleChecksum role pk = fromIntegral role + fromIntegral (BS.head $ keyBytes pk) .&. 0xffff
\end{code}
