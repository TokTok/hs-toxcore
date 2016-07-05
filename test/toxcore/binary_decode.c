#include "methods.h"

METHOD (Binary_decode, CipherText) { return pending; }
METHOD (Binary_decode, DhtPacket) { return pending; }
METHOD (Binary_decode, HostAddress) { return pending; }
METHOD (Binary_decode, Int) { return pending; }
METHOD (Binary_decode, Key) { return pending; }
METHOD (Binary_decode, KeyPair) { return pending; }
METHOD (Binary_decode, NodeInfo) { return pending; }
METHOD (Binary_decode, NodesRequest) { return pending; }
METHOD (Binary_decode, NodesResponse) { return pending; }
METHOD (Binary_decode, Packet) { return pending; }
METHOD (Binary_decode, PacketKind) { return pending; }
METHOD (Binary_decode, PingPacket) { return pending; }
METHOD (Binary_decode, PlainText) { return pending; }
METHOD (Binary_decode, PortNumber) { return pending; }
METHOD (Binary_decode, RpcPacket) { return pending; }
METHOD (Binary_decode, SocketAddress) { return pending; }
METHOD (Binary_decode, TransportProtocol) { return pending; }


METHOD (Binary, decode)
{
  CHECK (args.size == 2);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_STR);
  CHECK (args.ptr[1].type == MSGPACK_OBJECT_BIN);

  msgpack_object_str type = args.ptr[0].via.str;
#define DISPATCH(TYPE) \
  if (type.size == sizeof #TYPE - 1 && \
      memcmp (type.ptr, #TYPE, type.size) == 0) \
    return Binary_decode_##TYPE (args, res)
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
