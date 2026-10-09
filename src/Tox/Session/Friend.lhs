\begin{code}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Tox.Session.Friend where

import           Control.Monad.State            (gets, modify)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Word                      (Word8, Word32)

import           Tox.Crypto.Core.Key            (PublicKey)
import           Tox.Session.Connection         (ConnectionManager, FriendStatus(..), FriendConnection(..))
import qualified Tox.Session.Connection         as Connection
import           Tox.Core.Timed                 (Timed)
import           Tox.Crypto.Core.MonadRandomBytes (MonadRandomBytes)
import           Tox.Crypto.Core.Keyed          (Keyed)
import           Tox.Network.Core.Networked     (Networked)
import           Tox.Crypto.Core.Box            (PlainText(..))
import qualified Tox.Transport.SecureSession    as SecureSession

{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

data UserStatus = Online | Away | Busy
  deriving (Eq, Show, Enum)

data Friend = Friend
  { friendRealPk :: PublicKey
  , friendName   :: ByteString
  , friendStatus :: UserStatus
  , friendMsg    :: ByteString
  } deriving (Show)

data Messenger = Messenger
  { messengerFriends :: Map PublicKey Friend
  , selfName         :: ByteString
  , selfStatus       :: UserStatus
  , selfMsg          :: ByteString
  }

class (Monad m, Connection.ConnectionMonad m) => MessengerMonad m where
  getMessenger :: m Messenger
  putMessenger :: Messenger -> m ()

getsMessenger :: MessengerMonad m => (Messenger -> a) -> m a
getsMessenger f = f <$> getMessenger

modifyMessenger :: MessengerMonad m => (Messenger -> Messenger) -> m ()
modifyMessenger f = getMessenger >>= putMessenger . f


-- | Initialize a new Messenger.
initMessenger :: Messenger
initMessenger = Messenger
  { messengerFriends = Map.empty
  , selfName         = ""
  , selfStatus       = Online
  , selfMsg          = ""
  }

-- | Add a friend to Messenger and initiate connection.
addFriend :: MessengerMonad m => PublicKey -> m ()
addFriend pk = do
  modifyMessenger $ \s -> s { messengerFriends = Map.insert pk (Friend pk "" Online "") (messengerFriends s) }
  Connection.addFriend pk

-- | Send a text message to a friend.
sendMessage :: MessengerMonad m => PublicKey -> ByteString -> m ()
sendMessage pk msg = do
  mFc <- Connection.getsConn (Map.lookup pk . Connection.friends)
  case mFc of
    Just FriendConnection{ fcStatus = FriendConnected _ss } -> do
      -- Packet ID 0x40 for MESSAGE
      let _payload = BS.singleton 0x40 <> msg
      -- TODO: this needs to be integrated with SecureSession sending logic
      return ()
    _ -> return () -- Not connected

\end{code}

\chapter{Messenger}

Messenger is the module at the top of all the other modules.  It sits on top of
\texttt{friend\_connection} in the hierarchy of toxcore.

Messenger takes care of sending and receiving messages using the connection
provided by \texttt{friend\_connection}.  The module provides a way for friends
to connect and makes it usable as an instant messenger.  For example, Messenger
lets users set a nickname and status message which it then transmits to friends
when they are online.  It also allows users to send messages to friends and
builds an instant messenging system on top of the lower level
\texttt{friend\_connection} module.

Messenger offers two methods to add a friend.  The first way is to add a friend
with only their long term public key, this is used when a friend needs to be
added but for some reason a friend request should not be sent.  The friend
should only be added.  This method is most commonly used to accept friend
requests but could also be used in other ways.  If two friends add each other
using this function they will connect to each other.  Adding a friend using
this method just adds the friend to \texttt{friend\_connection} and creates a
new friend entry in Messenger for the friend.

The Tox ID is used to identify peers so that they can be added as friends in
Tox.  In order to add a friend, a Tox user must have the friend's Tox ID. The
Tox ID contains the long term public key of the peer (32 bytes) followed by the
4 byte nospam (see: \texttt{friend\_requests}) value and a 2 byte XOR checksum.
The method of sending the Tox ID to others is up to the user and the client but
the recommended way is to encode it in hexadecimal format and have the user
manually send it to the friend using another program.

Tox ID:

\begin{figure}
\includegraphics{res/images/tox-id.png}
\caption{Tox ID}
\end{figure}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{32}        & long term public key \\
  \texttt{4}         & nospam \\
  \texttt{2}         & checksum \\
\end{tabular}

The checksum is calculated by XORing the first two bytes of the ID with the
next two bytes, then the next two bytes until all the 36 bytes have been XORed
together.  The result is then appended to the end to form the Tox ID.

The user must make sure the Tox ID is not intercepted and replaced in transit
by a different Tox ID, which would mean the friend would connect to a malicious
person instead of the user, though taking reasonable precautions as this is
outside the scope of Tox.  Tox assumes that the user has ensured that they are
using the correct Tox ID, belonging to the intended person, to add a friend.

The second method to add a friend is by using their Tox ID and a message to be
sent in a friend request.  This way of adding friends will try to send a friend
request, with the set message, to the peer whose Tox ID was added.  The method
is similar to the first one, except that a friend request is crafted and sent
to the other peer.

When a friend connection associated to a Messenger friend goes online, a ONLINE
packet will be sent to them.  Friends are only set as online if an ONLINE
packet is received.

As soon as a friend goes online, Messenger will stop sending friend requests to
that friend, if it was sending them, as they are redundant for this friend.

Friends will be set as offline if either the friend connection associated to
them goes offline or if an OFFLINE packet is received from the friend.

Messenger packets are sent to the friend using the online friend connection to
the friend.

Should Messenger need to check whether any of the non lossy packets in the
following list were received by the friend, for example to implement receipts
for text messages, \texttt{net\_crypto} can be used.  The \texttt{net\_crypto}
packet number, used to send the packets, should be noted and then
\texttt{net\_crypto} checked later to see if the bottom of the send array is
after this packet number.  If it is, then the friend has received them.  Note
that \texttt{net\_crypto} packet numbers could overflow after a long time, so
checks should happen within 2**32 \texttt{net\_crypto} packets sent with the
same friend connection.

Message receipts for action messages and normal text messages are implemented
by adding the \texttt{net\_crypto} packet number of each message, along with the
receipt number, to the top of a linked list that each friend has as they are
sent.  Every Messenger loop, the entries are read from the bottom and entries
are removed and passed to the client until an entry that refers to a packet not
yet received by the other is reached, when this happens it stops.

List of Messenger packets:

\section{\texttt{ONLINE}}

length: 1 byte

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x18) \\
\end{tabular}

Sent to a friend when a connection is established to tell them to mark us as
online in their friends list.  This packet and the OFFLINE packet are necessary
as \texttt{friend\_connections} can be established with non-friends who are part
of a groupchat.  The two packets are used to differentiate between these peers,
connected to the user through groupchats, and actual friends who ought to be
marked as online in the friendlist.

On receiving this packet, Messenger will show the peer as being online.

\section{\texttt{OFFLINE}}

length: 1 byte

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x19) \\
\end{tabular}

Sent to a friend when deleting the friend.  Prevents a deleted friend from
seeing us as online if we are connected to them because of a group chat.

On receiving this packet, Messenger will show this peer as offline.

\section{\texttt{NICKNAME}}

length: 1 byte to 129 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x30) \\
  \texttt{[0, 128]}  & Nickname as a UTF8 byte string \\
\end{tabular}

Used to send the nickname of the peer to others.  This packet should be sent
every time to each friend every time they come online and each time the
nickname is changed.

\section{\texttt{STATUSMESSAGE}}

length: 1 byte to 1008 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x31) \\
  \texttt{[0, 1007]} & Status message as a UTF8 byte string \\
\end{tabular}

Used to send the status message of the peer to others.  This packet should be
sent every time to each friend every time they come online and each time the
status message is changed.

\section{\texttt{USERSTATUS}}

length: 2 bytes

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x32) \\
  \texttt{1}         & \texttt{uint8\_t} status (0 = online, 1 = away, 2 = busy) \\
\end{tabular}

Used to send the user status of the peer to others.  This packet should be sent
every time to each friend every time they come online and each time the user
status is changed.

\section{\texttt{TYPING}}

length: 2 bytes

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x33) \\
  \texttt{1}         & \texttt{uint8\_t} typing status (0 = not typing, 1 = typing) \\
\end{tabular}

Used to tell a friend whether the user is currently typing or not.

\section{\texttt{MESSAGE}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x40) \\
  \texttt{[0, 1372]} & Message as a UTF8 byte string \\
\end{tabular}

Used to send a normal text message to the friend.

\section{\texttt{ACTION}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x41) \\
  \texttt{[0, 1372]} & Action message as a UTF8 byte string \\
\end{tabular}

Used to send an action message (like an IRC action) to the friend.

\section{\texttt{MSI}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x45) \\
  \texttt{?}         & data \\
\end{tabular}

Reserved for Tox AV usage.

\section{File Transfer Related Packets}

\subsection{\texttt{FILE\_SENDREQUEST}}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x50) \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{4}         & \texttt{uint32\_t} file type \\
  \texttt{8}         & \texttt{uint64\_t} file size \\
  \texttt{32}        & file id (32 bytes) \\
  \texttt{[0, 255]}  & filename as a UTF8 byte string \\
\end{tabular}

Note that file type and file size are sent in big endian/network byte format.

\subsection{\texttt{FILE\_CONTROL}}

length: 4 bytes if \texttt{control\_type} isn't seek.  12 bytes if
\texttt{control\_type} is seek.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x51) \\
  \texttt{1}         & \texttt{uint8\_t} \texttt{send\_receive} \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{1}         & \texttt{uint8\_t} \texttt{control\_type} \\
  \texttt{8}         & \texttt{uint64\_t} seek parameter \\
\end{tabular}

\texttt{send\_receive} is 0 if the control targets a file being sent (by the
peer sending the file control), and 1 if it targets a file being received.

\texttt{control\_type} can be one of: 0 = accept, 1 = pause, 2 = kill, 3 = seek.

The seek parameter is only included when \texttt{control\_type} is seek (3).

Note that if it is included the seek parameter will be sent in big
endian/network byte format.

\subsection{\texttt{FILE\_DATA}}

length: 2 to 1373 bytes.

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8\_t} (0x52) \\
  \texttt{1}         & \texttt{uint8\_t} file number \\
  \texttt{[0, 1371]} & file data piece \\
\end{tabular}

Files are transferred in Tox using File transfers.

To initiate a file transfer, the friend creates and sends a
\texttt{FILE\_SENDREQUEST} packet to the friend it wants to initiate a file
transfer to.

The first part of the \texttt{FILE\_SENDREQUEST} packet is the file number.  The
file number is the number used to identify this file transfer.  As the file
number is represented by a 1 byte number, the maximum amount of concurrent
files Tox can send to a friend is 256.  256 file transfers per friend is enough
that clients can use tricks like queueing files if there are more files needing
to be sent.

256 outgoing files per friend means that there is a maximum of 512 concurrent
file transfers, between two users, if both incoming and outgoing file transfers
are counted together.

As file numbers are used to identify the file transfer, the Tox instance must
make sure to use a file number that isn't used for another outgoing file
transfer to that same friend when creating a new outgoing file transfer.  File
numbers are chosen by the file sender and stay unchanged for the entire
duration of the file transfer.  The file number is used by both
\texttt{FILE\_CONTROL} and \texttt{FILE\_DATA} packets to identify which file
transfer these packets are for.

The second part of the file transfer request is the file type.  This is simply
a number that identifies the type of file.  for example, tox.h defines the file
type 0 as being a normal file and type 1 as being an avatar meaning the Tox
client should use that file as an avatar.  The file type does not effect in any
way how the file is transfered or the behavior of the file transfer.  It is set
by the Tox client that creates the file transfers and send to the friend
untouched.

The file size indicates the total size of the file that will be transfered.  A
file size of \texttt{UINT64\_MAX} (maximum value in a \texttt{uint64\_t}) means
that the size of the file is undetermined or unknown.  For example if someone
wanted to use Tox file transfers to stream data they would set the file size to
\texttt{UINT64\_MAX}.  A file size of 0 is valid and behaves exactly like a
normal file transfer.

The file id is 32 bytes that can be used to uniquely identify the file
transfer.  For example, avatar transfers use it as the hash of the avatar so
that the receiver can check if they already have the avatar for a friend which
saves bandwidth.  It is also used to identify broken file transfers across
toxcore restarts (for more info see the file transfer section of tox.h).  The
file transfer implementation does not care about what the file id is, as it is
only used by things above it.

The last part of the file transfer is the optional file name which is used to
tell the receiver the name of the file.

When a \texttt{FILE\_SENDREQUEST} packet is received, the implementation
validates and sends the info to the Tox client which decides whether they
should accept the file transfer or not.

To refuse or cancel a file transfer, they will send a \texttt{FILE\_CONTROL}
packet with \texttt{control\_type} 2 (kill).

\texttt{FILE\_CONTROL} packets are used to control the file transfer.
\texttt{FILE\_CONTROL} packets are used to accept/unpause, pause, kill/cancel
and seek file transfers.  The \texttt{control\_type} parameter denotes what the
file control packet does.

The \texttt{send\_receive} and file number are used to identify a specific file
transfer.  Since file numbers for outgoing and incoming files are not related
to each other, the \texttt{send\_receive} parameter is used to identify if the
file number belongs to files being sent or files being received.  If
\texttt{send\_receive} is 0, the file number corresponds to a file being sent by
the user sending the file control packet.  If \texttt{send\_receive} is 1, it
corresponds to a file being received by the user sending the file control
packet.

\texttt{control\_type} indicates the purpose of the \texttt{FILE\_CONTROL}
packet.  \texttt{control\_type} of 0 means that the \texttt{FILE\_CONTROL} packet
is used to tell the friend that the file transfer is accepted or that we are
unpausing a previously paused (by us) file transfer.  \texttt{control\_type} of
1 is used to tell the other to pause the file transfer.

If one party pauses a file transfer, that party must be the one to unpause it.
Should both sides pause a file transfer, both sides must unpause it before the
file can be resumed.  For example, if the sender pauses the file transfer, the
receiver must not be able to unpause it.  To unpause a file transfer,
\texttt{control\_type} 0 is used.  Files can only be paused when they are in
progress and have been accepted.

\texttt{control\_type} 2 is used to kill, cancel or refuse a file transfer.
When a \texttt{FILE\_CONTROL} is received, the targeted file transfer is
considered dead, will immediately be wiped and its file number can be reused.
The peer sending the \texttt{FILE\_CONTROL} must also wipe the targeted file
transfer from their side.  This control type can be used by both sides of the
transfer at any time.

\texttt{control\_type} 3, the seek control type is used to tell the sender of
the file to start sending from a different index in the file than 0.  It can
only be used right after receiving a \texttt{FILE\_SENDREQUEST} packet and
before accepting the file by sending a \texttt{FILE\_CONTROL} with
\texttt{control\_type} 0.  When this \texttt{control\_type} is used, an extra 8
byte number in big endian format is appended to the \texttt{FILE\_CONTROL} that
is not present with other control types.  This number indicates the index in
bytes from the beginning of the file at which the file sender should start
sending the file.  The goal of this control type is to ensure that files can be
resumed across core restarts.  Tox clients can know if they have received a
part of a file by using the file id and then using this packet to tell the
other side to start sending from the last received byte.  If the seek position
is bigger or equal to the size of the file, the seek packet is invalid and the
one receiving it will discard it.

To accept a file Tox will therefore send a seek packet, if it is needed, and
then send a \texttt{FILE\_CONTROL} packet with \texttt{control\_type} 0 (accept)
to tell the file sender that the file was accepted.

Once the file transfer is accepted, the file sender will start sending file
data in sequential chunks from the beginning of the file (or the position from
the \texttt{FILE\_CONTROL} seek packet if one was received).

File data is sent using \texttt{FILE\_DATA} packets.  The file number
corresponds to the file transfer that the file chunks belong to.  The receiver
assumes that the file transfer is over as soon as a chunk with the file data
size not equal to the maximum size (1371 bytes) is received.  This is how the
sender tells the receiver that the file transfer is complete in file transfers
where the size of the file is unknown (set to \texttt{UINT64\_MAX}).  The
receiver also assumes that if the amount of received data equals to the file
size received in the \texttt{FILE\_SENDREQUEST}, the file sending is finished
and has been successfully received.  Immediately after this occurs, the
receiver frees up the file number so that a new incoming file transfer can use
that file number.  The implementation should discard any extra data received
which is larger than the file size received at the beginning.

In 0 filesize file transfers, the sender will send one \texttt{FILE\_DATA}
packet with a file data size of 0.

The sender will know if the receiver has received the file successfully by
checking if the friend has received the last \texttt{FILE\_DATA} packet sent
(containing the last chunk of the file).  \texttt{net\_crypto} can be used to
check whether packets sent through it have been received by storing the packet
number of the sent packet and verifying later in \texttt{net\_crypto} to see
whether it was received or not.  As soon as \texttt{net\_crypto} says the other
received the packet, the file transfer is considered successful, wiped and the
file number can be reused to send new files.

\texttt{FILE\_DATA} packets should be sent as fast as the \texttt{net\_crypto}
connection can handle it respecting its congestion control.

If the friend goes offline, all file transfers are cleared in toxcore.  This
makes it simpler for toxcore as it does not have to deal with resuming file
transfers.  It also makes it simpler for clients as the method for resuming
file transfers remains the same, even if the client is restarted or toxcore
loses the connection to the friend because of a bad internet connection.

\section{Group Chat Related Packets}

\begin{tabular}{l|l}
  Packet ID & Packet Name \\
  \hline
  0x60      & \texttt{INVITE\_GROUPCHAT} \\
  0x61      & \texttt{ONLINE\_PACKET} \\
  0x62      & \texttt{DIRECT\_GROUPCHAT} \\
  0x63      & \texttt{MESSAGE\_GROUPCHAT} \\
  0xC7      & \texttt{LOSSY\_GROUPCHAT} \\
\end{tabular}

Messenger also takes care of saving the friends list and other friend
information so that it's possible to close and start toxcore while keeping all
your friends, your long term key and the information necessary to reconnect to
the network.

Important information messenger stores includes: the long term private key, our
current nospam value, our friends' public keys and any friend requests the user
is currently sending.  The network DHT nodes, TCP relays and some onion nodes
are stored to aid reconnection.

In addition to this, a lot of optional data can be stored such as the usernames
of friends, our current username, status messages of friends, our status
message, etc... can be stored.  The exact format of the toxcore save is
explained later.

The TCP server is run from the toxcore messenger module if the client has
enabled it.  TCP server is usually run independently as part of the bootstrap
node package but it can be enabled in clients.  If it is enabled in toxcore,
Messenger will add the running TCP server to the TCP relay.

Messenger is the module that transforms code that can connect to friends based
on public key into a real instant messenger.

\chapter{Friend requests}

When a Tox user adds someone with Tox, toxcore will try sending a friend
request to that person.  A friend request contains the long term public key of
the sender, a nospam number and a message.

Transmitting the long term public key is the primary goal of the friend request
as it is what the peer needs to find and establish a connection to the sender.
The long term public key is what the receiver adds to his friends list if he
accepts the friend request.

The nospam is a number used to prevent someone from spamming the network with
valid friend requests.  It makes sure that the only people who have seen the
Tox ID of a peer are capable of sending them a friend request.  The nospam is
one of the components of the Tox ID.

The nospam is a number or a list of numbers set by the peer, only received
friend requests that contain a nospam that was set by the peer are sent to the
client to be accepted or refused by the user.  The nospam prevents random peers
in the network from sending friend requests to non friends.  The nospam is not
long enough to be secure meaning an extremely resilient attacker could manage
to send a spam friend request to someone.  4 bytes is large enough to prevent
spam from random peers in the network.  The nospam could also allow Tox users
to issue different Tox IDs and even change Tox IDs if someone finds a Tox ID
and decides to send it hundreds of spam friend requests.  Changing the nospam
would stop the incoming wave of spam friend requests without any negative
effects to the users friends list.  For example if users would have to change
their public key to prevent them from receiving friend requests it would mean
they would have to essentially abandon all their current friends as friends are
tied to the public key.  The nospam is not used at all once the friends have
each other added which means changing it won't have any negative effects.

Friend request:

\begin{verbatim}
[uint32_t nospam][Message (UTF8) 1 to ONION_CLIENT_MAX_DATA_SIZE bytes]
\end{verbatim}

Friend request packet when sent as an onion data packet:

\begin{verbatim}
[uint8_t (32)][Friend request]
\end{verbatim}

Friend request packet when sent as a \texttt{net\_crypto} data packet (If we are
directly connected to the peer because of a group chat but are not friends with
them):

\begin{verbatim}
[uint8_t (18)][Friend request]
\end{verbatim}

When a friend is added to toxcore with their Tox ID and a message, the friend
is added in \texttt{friend\_connection} and then toxcore tries to send friend
requests.

When sending a friend request, toxcore will check if the peer which a friend
request is being sent to is already connected to using a \texttt{net\_crypto}
connection which can happen if both are in the same group chat.  If this is the
case the friend request will be sent as a \texttt{net\_crypto} packet using that
connection.  If not, it will be sent as an onion data packet.

Onion data packets contain the real public key of the sender and if a
\texttt{net\_crypto} connection is established it means the peer knows our real
public key.  This is why the friend request does not need to contain the real
public key of the peer.

Friend requests are sent with exponentially increasing interval of 2 seconds, 4
seconds, 8 seconds, etc... in toxcore.  This is so friend requests get resent
but eventually get resent in intervals that are so big that they essentially
expire.  The sender has no way of knowing if a peer refuses a friend requests
which is why friend requests need to expire in some way.  Note that the
interval is the minimum timeout, if toxcore cannot send that friend request it
will try again until it manages to send it.  One reason for not being able to
send the friend request would be that the onion has not found the friend in the
onion and so cannot send an onion data packet to them.

Received friend requests are passed to the client, the client is expected to
show the message from the friend request to the user and ask the user if they
want to accept the friend request or not.  Friend requests are accepted by
adding the peer sending the friend request as a friend and refused by simply
ignoring it.

Friend requests are sent multiple times meaning that in order to prevent the
same friend request from being sent to the client multiple times toxcore keeps
a list of the last real public keys it received friend requests from and
discards any received friend requests that are from a real public key that is
in that list.  In toxcore this list is a simple circular list.  There are many
ways this could be improved and made more efficient as a circular list isn't
very efficient however it has worked well in toxcore so far.

Friend requests from public keys that are already added to the friends list
should also be discarded.
