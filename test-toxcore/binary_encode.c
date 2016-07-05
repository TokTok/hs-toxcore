#include "methods.h"

METHOD (Binary_encode, CipherText) { return pending; }
METHOD (Binary_encode, DhtPacket) { return pending; }
METHOD (Binary_encode, HostAddress) { return pending; }
METHOD (Binary_encode, Int) { return pending; }
METHOD (Binary_encode, Key) { return pending; }
METHOD (Binary_encode, KeyPair) { return pending; }
METHOD (Binary_encode, NodeInfo) { return pending; }
METHOD (Binary_encode, NodesRequest) { return pending; }
METHOD (Binary_encode, NodesResponse) { return pending; }
METHOD (Binary_encode, Packet) { return pending; }
METHOD (Binary_encode, PacketKind) { return pending; }
METHOD (Binary_encode, PingPacket) { return pending; }
METHOD (Binary_encode, PlainText) { return pending; }
METHOD (Binary_encode, PortNumber) { return pending; }
METHOD (Binary_encode, RpcPacket) { return pending; }
METHOD (Binary_encode, SocketAddress) { return pending; }
METHOD (Binary_encode, TransportProtocol) { return pending; }


METHOD (Binary, encode)
{
  CHECK (args.size == 2);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_STR);

  msgpack_object_str type = args.ptr[0].via.str;
#define DISPATCH(TYPE) \
  if (type.size == sizeof #TYPE - 1 && \
      memcmp (type.ptr, #TYPE, type.size) == 0) \
    return Binary_encode_##TYPE (args, res)
  DISPATCH (CipherText);
  DISPATCH (DhtPacket);
  DISPATCH (HostAddress);
  DISPATCH (Int);
  DISPATCH (Key);
  DISPATCH (KeyPair);
  DISPATCH (NodeInfo);
  DISPATCH (NodesRequest);
  DISPATCH (NodesResponse);
  DISPATCH (Packet);
  DISPATCH (PacketKind);
  DISPATCH (PingPacket);
  DISPATCH (PlainText);
  DISPATCH (PortNumber);
  DISPATCH (RpcPacket);
  DISPATCH (SocketAddress);
  DISPATCH (TransportProtocol);
#undef DISPATCH

  return unimplemented;
}
