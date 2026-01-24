\chapter{Introduction}

\begin{code}
{-# LANGUAGE StrictData #-}
module Tox.Toxcore where
\end{code}

This document is a textual specification of the Tox protocol and all the
supporting modules required to implement it.  The goal of this document is to
give enough guidance to permit a complete and correct implementation of the
protocol.

\section{Objectives}

This section provides an overview of goals and non-goals of Tox.  It provides
the reader with:

\begin{itemize}
  \item a basic understanding of what problems Tox intends to solve;
  \item a means to validate whether those problems are indeed solved by the
    protocol as specified;
  \item the ability to make better tradeoffs and decisions in their own
    reimplementation of the protocol.
\end{itemize}

\subsection{Goals}

\begin{itemize}

  \item \textbf{Authentication:} Tox aims to provide authenticated
    communication. This means that during a communication session, both parties
    can be sure of the other party's identity. Users are identified by their
    public key. The initial key exchange is currently not in scope for the Tox
    protocol. In the future, Tox may provide a means for initial authentication
    using a challenge/response or shared secret based exchange.

    If the secret key is compromised, the user's identity is compromised, and an
    attacker can impersonate that user. When this happens, the user must create
    a new identity with a new public key.

  \item \textbf{End-to-end encryption:} The Tox protocol establishes end-to-end
    encrypted communication links. Shared keys are deterministically derived
    using a Diffie-Hellman-like method, so keys are never transferred over the
    network.

  \item \textbf{Forward secrecy}: Session keys are re-negotiated when the peer
    connection is established.

  \item \textbf{Privacy}: When Tox establishes a communication link, it aims to
    avoid leaking to any third party the identities of the parties involved
    (i.e. their public keys).

    Furthermore, it aims to avoid allowing third parties to determine the IP
    address of a given user.

  \item \textbf{Resilience:}
    \begin{itemize}
      \item Independence of infrastructure: Tox avoids relying on servers as
        much as possible. Communications are not transmitted via or stored on
        central servers. Joining a Tox network requires connecting to a
        well-known node called a bootstrap node. Anyone can run a bootstrap
        node, and users need not put any trust in them.
      \item Tox tries to establish communication paths in difficult network
        situations. This includes connecting to peers behind a NAT or firewall.
        Various techniques help achieve this, such as UDP hole-punching, UPnP,
        NAT-PMP, other untrusted nodes acting as relays, and DNS tunnels.
      \item Resistance to basic denial of service attacks: short timeouts make
        the network dynamic and resilient against poisoning attempts.
    \end{itemize}

  \item \textbf{Minimum configuration:} Tox aims to be nearly zero-conf.
    User-friendliness is an important aspect to security. Tox aims to make
    security easy to achieve for average users.
\end{itemize}

\subsection{Non-goals}

\begin{itemize}
  \item \textbf{Anonymity} is not in scope for the Tox protocol itself, but it
    provides an easy way to integrate with software providing anonymity, such as
    Tor.

    By default, Tox tries to establish direct connections between peers; as a
    consequence, each is aware of the other's IP address, and third parties
    may be able to determine that a connection has been established between
    those IP addresses. One of the reasons for making direct connections is that
    relaying real-time multimedia conversations over anonymity networks is not
    feasible with the current network infrastructure.
\end{itemize}

\section{Threat model}

TODO(iphydf): Define one.

\section{Data types}

All data types are defined before their first use, and their binary protocol
representation is given.  The protocol representations are normative and must
be implemented exactly as specified.  For some types, human-readable
representations are suggested.  An implementation may choose to provide no such
representation or a different one.  The implementation is free to choose any
in-memory representation of the specified types.

Binary formats are specified in tables with length, type, and content
descriptions.  If applicable, specific enumeration types are used, so types may
be self-explanatory in some cases.  The length can be either a fixed number in
bytes (e.g. \texttt{32}), a number in bits (e.g. \texttt{7} bit), a choice of
lengths (e.g. \texttt{4 / 16}), or an inclusive range (e.g. \texttt{[0,
100]}). Open ranges are denoted \texttt{[n,]} to mean a minimum length of
\texttt{n} with no specified maximum length.

\section{Integers}

The protocol uses four bounded unsigned integer types.  Bounded means they have
an upper bound beyond which incrementing is not defined.  The integer types
support modular arithmetic, so overflow wraps around to zero.  Unsigned means
their lower bound is 0.  Signed integer types are not used.  The binary
encoding of all integer types is a fixed-width byte sequence with the integer
encoded in \href{https://en.wikipedia.org/wiki/Endianness}{Big Endian} unless
stated otherwise.

\begin{tabular}{l|l|l|l}
  Type name  & C type            & Length & Upper bound \\
  \hline
  Word8      & \texttt{uint8\_t}  & 1      & 255 (0xff) \\
  Word16     & \texttt{uint16\_t} & 2      & 65535 (0xffff) \\
  Word32     & \texttt{uint32\_t} & 4      & 4294967295 (0xffffffff) \\
  Word64     & \texttt{uint64\_t} & 8      & 18446744073709551615 (0xffffffffffffffff) \\
\end{tabular}

\section{Strings}

A String is a data structure used for human readable text.  Strings are
sequences of glyphs.  A glyph consists of one non-zero-width unicode code point
and zero or more zero-width unicode code points.  The human-readable
representation of a String starts and ends with a quotation mark (\texttt{"})
and contains all human-readable glyphs verbatim.  Control characters are
represented in an isomorphic human-readable way.  I.e. every control character
has exactly one human-readable representation, and a mapping exists from the
human-readable representation to the control character.  Therefore, the use of
Unicode Control Characters (U+240x) is not permitted without additional marker.

\input{src/Tox/Crypto/Core.lhs}
\input{src/Tox/Core/PingArray.lhs}
\input{src/Tox/Network/Core.lhs}
\input{src/Tox/Network/Core/Backend.lhs}
\input{src/Tox/Network/Core/Protocol.lhs}
\input{src/Tox/Network/Discovery/LAN.lhs}
\input{src/Tox/Network/TCP/Client.lhs}
\input{src/Tox/Network/TCP/Connections.lhs}
\input{src/Tox/Network/TCP/Server.lhs}
\input{src/Tox/DHT.lhs}
\input{src/Tox/Session/Connection.lhs}
\input{src/Tox/Session/Friend.lhs}
\input{src/Tox/Application/LegacyGroup.lhs}
\input{src/Tox/Onion/Tunnel.lhs}
\input{src/Tox/Onion/RPC.lhs}
\input{src/Tox/Onion/Path.lhs}
\input{src/Tox/Onion/Operation.lhs}
\input{src/Tox/Transport/SecureSession.lhs}
\input{src/Tox/Transport/Reliability.lhs}
\input{src/Tox/Transport/Stream.lhs}
\input{src/Tox/Application/GroupChat.lhs}
\input{src/Tox/Application/GroupChatPackets.lhs}
\input{src/Tox/Persistence.lhs}
