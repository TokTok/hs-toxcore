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
\textbf{\verb'Message ID'} is a unique packet identifier which is used
for the lossless UDP implementation.

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

Ping packets are periodically sent to every confirmed peer in order to
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

\subsection{KEY\_ROTATIONS (0xf0)}

Key rotation packets are structured as follows:

\begin{tabular}{l|l}
  Length                & Contents \\
  \hline
  \texttt{1}            & \textbf{\verb'is_response'} \\
  \texttt{32}           & Public Encryption Key \\
\end{tabular}

Key rotation packets are used to rotate session encryption keys with
a peer. If \textbf{\verb'is_response'} is false, the packet initiates a
public key exchange. Otherwise the packet is a response to a previously
initiated exchange.

The public encryption key must be a newly generated key which takes
the place of the previously used session key. The resulting shared
session key is generated using the same protocol as the initial
handshake, and must be kept secret.

Request packets should only be sent by the peer whose permanent public
encryption key for the given group is closer to the group
\textbf{\verb'Chat ID'} according to the \href{#distance}{\texttt{Distance}}
metric.

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

This packet is used to to send arbitrary data to another peer. It may be
used for client-side features.

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
  \texttt{1}             & Flag \\
  \texttt{32}            & Public Signature Key \\
\end{tabular}

Indicates that the peer associated with the public signature key has
either been promoted to or demoted from the \textbf{\verb'Moderator'} role
by the group founder. If \textbf{\verb'flag'} is non-zero, the peer should
be promoted and added to the moderator list. Otherwise they should be
demoted to the \textbf{\verb'User'} role and removed from the moderator list.

\paragraph{SET\_OBSERVER (0x08)}

\begin{tabular}{l|l}
  Length                 & Contents \\
  \hline
  \texttt{1}             & Flag \\
  \texttt{32}            & Public Encryption Key \\
  \texttt{32}            & Public Signature Key \\
  \texttt{137}           & Sanctions List Entry \textbf{\verb'[optional]'} \\
  \texttt{132}           & Packed Sanctions List Credentials \\
\end{tabular}

Indicates that the peer associated with the given public keys has either
been demoted to or promoted from the \textbf{\verb'Observer'} role by the group
founder or a moderator. If \textbf{\verb'flag'} is non-zero, the peer should be
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
  \texttt{4}               & Peer Limit \\
  \texttt{2}               & Group Name Length \\
  \texttt{48}              & Group Name \\
  \texttt{1}               & Privacy State \\
  \texttt{2}               & Group Password Length \\
  \texttt{32}              & Group Password \\
  \texttt{32}              & Moderator List Hash (Sha256) \\
  \texttt{4}               & Topic Lock State \\
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
module Network.Tox.Application.GroupChatsPackets where
\end{code}
