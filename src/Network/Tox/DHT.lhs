\chapter{DHT}

\begin{code}
{-# LANGUAGE Safe       #-}
{-# LANGUAGE StrictData #-}
module Network.Tox.DHT where
\end{code}

The DHT is a self-organizing swarm of all nodes in the Tox network.  A node in
the Tox network is also called a "Tox node".  When we talk about "peers", we mean
any node that is not the local node (the subject).  This module takes care of
finding the IP and port of nodes and establishing a route to them directly via
UDP using \href{#hole-punching}{hole punching} if necessary.  The DHT only runs
on UDP and so is only used if UDP works.

Every node in the Tox DHT has an ephemeral Key Pair called the DHT Key Pair,
consisting of the DHT Secret Key and the DHT Public Key.  The DHT Public Key
acts as the node address.  The DHT Key Pair is renewed every time the Tox
instance is closed or restarted.  An implementation may choose to renew the key
more often, but doing so will disconnect all peers.

The DHT public key of a friend is found using the \href{#onion}{onion} module.
Once the DHT public key of a friend is known, the DHT is used to find them and
connect directly to them via UDP.

\input{src/Network/Tox/DHT/Distance.lhs}
\input{src/Network/Tox/DHT/ClientList.lhs}
\input{src/Network/Tox/DHT/KBuckets.lhs}
\input{src/Network/Tox/DHT/DhtState.lhs}

\section{Self-organisation}

Self-organising in the DHT occurs through each DHT peer connecting to an
arbitrary number of peers closest to their own DHT public key and some that are
further away.

If each peer in the network knows the peers with the DHT public key closest to
its DHT public key, then to find a specific peer with public key X a peer just
needs to recursively ask peers in the DHT for known peers that have the DHT
public keys closest to X.  Eventually the peer will find the peers in the DHT
that are the closest to that peer and, if that peer is online, they will find
them.

\input{src/Network/Tox/DHT/DhtPacket.lhs}

\section{RPC Services}

\input{src/Network/Tox/DHT/RpcPacket.lhs}
\input{src/Network/Tox/DHT/PingPacket.lhs}

\subsection{Nodes Service}

The Nodes Service is used to query another DHT node for up to 4 nodes they know
that are the closest to a requested node.

The DHT Nodes RPC service uses the Packed Node Format.

Only the UDP Protocol (IP Type \texttt{2} and \texttt{10}) is used in the DHT
module when sending nodes with the packed node format.  This is because the TCP
Protocol is used to send TCP relay information and the DHT is UDP only.

\input{src/Network/Tox/DHT/NodesRequest.lhs}
\input{src/Network/Tox/DHT/NodesResponse.lhs}

\input{src/Network/Tox/DHT/Operation.lhs}

\section{NATs}

We assume that peers are either directly accessible or are behind one of 3
types of NAT:

Cone NATs: Assign one whole port to each UDP socket behind the NAT; any packet
from any IP/port sent to that assigned port from the internet will be forwarded
to the socket behind it.

Restricted Cone NATs: Assign one whole port to each UDP socket behind the NAT.
However, it will only forward packets from IPs that the UDP socket has sent a
packet to.

Symmetric NATs: The worst kind of NAT, they assign a new port for each IP/port
a packet is sent to.  They treat each new peer you send a UDP packet to as a
\texttt{'connection'} and will only forward packets from the IP/port of that
\texttt{'connection'}.


\section{Hole punching}

Holepunching on normal cone NATs is achieved simply through the way in which
the DHT functions.

If more than half of the 8 peers closest to the friend in the DHT return an
IP/port for the friend and we send a ping request to each of the returned
IP/ports but get no response.  If we have sent 4 ping requests to 4 IP/ports
that supposedly belong to the friend and get no response, then this is enough
for toxcore to start the hole punching.  The numbers 8 and 4 are used in
toxcore and were chosen based on feel alone and so may not be the best numbers.

Before starting the hole punching, the peer will send a NAT ping packet to the
friend via the peers that say they know the friend.  If a NAT ping response
with the same random number is received the hole punching will start.

If a NAT ping request is received, we will first check if it is from a friend.
If it is not from a friend it will be dropped.  If it is from a friend, a
response with the same 8 byte number as in the request will be sent back via
the nodes that know the friend sending the request.  If no nodes from the
friend are known, the packet will be dropped.

Receiving a NAT ping response therefore means that the friend is both online
and actively searching for us, as that is the only way they would know nodes
that know us.  This is important because hole punching will work only if the
friend is actively trying to connect to us.

NAT ping requests are sent every 3 seconds in toxcore, if no response is
received for 6 seconds, the hole punching will stop.  Sending them in longer
intervals might increase the possibility of the other node going offline and
ping packets sent in the hole punching being sent to a dead peer but decrease
bandwidth usage.  Decreasing the intervals will have the opposite effect.

There are 2 cases that toxcore handles for the hole punching.  The first case
is if each 4+ peers returned the same IP and port.  The second is if the 4+
peers returned same IPs but different ports.

A third case that may occur is the peers returning different IPs and ports.
This can only happen if the friend is behind a very restrictive NAT that cannot
be hole punched or if the peer recently connected to another internet
connection and some peers still have the old one stored.  Since there is
nothing we can do for the first option it is recommended to just use the most
common IP returned by the peers and to ignore the other IP/ports.

In the case where the peers return the same IP and port it means that the other
friend is on a restricted cone NAT.  These kinds of NATs can be hole punched by
getting the friend to send a packet to our public IP/port.  This means that
hole punching can be achieved easily and that we should just continue sending
DHT ping packets regularly to that IP/port until we get a ping response.  This
will work because the friend is searching for us in the DHT and will find us
and will send us a packet to our public IP/port (or try to with the hole
punching), thereby establishing a connection.

For the case where peers do not return the same ports, this means that the
other peer is on a symmetric NAT.  Some symmetric NATs open ports in sequences
so the ports returned by the other peers might be something like: 1345, 1347,
1389, 1395.  The method to hole punch these NATs is to try to guess which ports
are more likely to be used by the other peer when they try sending us ping
requests and send some ping requests to these ports.  Toxcore just tries all
the ports beside each returned port (ex: for the 4 ports previously it would
try: 1345, 1347, 1389, 1395, 1346, 1348, 1390, 1396, 1344, 1346...) getting
gradually further and further away and, although this works, the method could
be improved.  When using this method toxcore will try up to 48 ports every 3
seconds until both connect.  After 5 tries toxcore doubles this and starts
trying ports from 1024 (48 each time) along with the previous port guessing.
This is because I have noticed that this seemed to fix it for some symmetric
NATs, most likely because a lot of them restart their count at 1024.

Increasing the amount of ports tried per second would make the hole punching go
faster but might DoS NATs due to the large number of packets being sent to
different IPs in a short amount of time.  Decreasing it would make the hole
punching slower.

This works in cases where both peers have different NATs.  For example, if A
and B are trying to connect to each other: A has a symmetric NAT and B a
restricted cone NAT.  A will detect that B has a restricted cone NAT and keep
sending ping packets to his one IP/port.  B will detect that A has a symmetric
NAT and will send packets to it to try guessing his ports.  If B manages to
guess the port A is sending packets from they will connect together.

\section{DHT Bootstrap Info (0xf0)}

Bootstrap nodes are regular Tox nodes with a stable DHT public key. This means
the DHT public key does not change across restarts. DHT bootstrap nodes have one
additional request kind: Bootstrap Info. The request is simply a packet of
length 78 bytes where the first byte is 0xf0. The other bytes are ignored.

The response format is as follows:

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{4}         & Word32      & Bootstrap node version \\
  \texttt{256}       & Bytes       & Message of the day \\
\end{tabular}
