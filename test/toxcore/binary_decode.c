#include "methods.h"

METHOD (bin, Binary_decode, CipherText) { return pending; }
METHOD (bin, Binary_decode, DhtPacket) { return pending; }
METHOD (bin, Binary_decode, HostAddress) { return pending; }
METHOD (bin, Binary_decode, Int) { return pending; }
METHOD (bin, Binary_decode, Key) { return pending; }

METHOD (bin, Binary_decode, KeyPair)
{
  SUCCESS {
    // TODO(iphydf): Haskell implementation accepts >= 64, but shouldn't.
    if (args.size < 64)
      msgpack_pack_nil (res);
    else
      {
        msgpack_pack_array (res, 2);
        msgpack_pack_bin (res, 32);
        msgpack_pack_bin_body (res, args.ptr, 32);
        msgpack_pack_bin (res, 32);
        msgpack_pack_bin_body (res, args.ptr + 32, 32);
      }
  }

  return 0;
}

METHOD (bin, Binary_decode, NodeInfo) { return pending; }
METHOD (bin, Binary_decode, NodesRequest) { return pending; }
METHOD (bin, Binary_decode, NodesResponse) { return pending; }
METHOD (bin, Binary_decode, Packet) { return pending; }
METHOD (bin, Binary_decode, PacketKind) { return pending; }
METHOD (bin, Binary_decode, PingPacket) { return pending; }
METHOD (bin, Binary_decode, PlainText) { return pending; }
METHOD (bin, Binary_decode, PortNumber) { return pending; }
METHOD (bin, Binary_decode, RpcPacket) { return pending; }
METHOD (bin, Binary_decode, SocketAddress) { return pending; }
METHOD (bin, Binary_decode, TransportProtocol) { return pending; }


METHOD (array, Binary, decode)
{
  CHECK_SIZE (args, 2);
  CHECK_TYPE (args.ptr[0], MSGPACK_OBJECT_STR);
  CHECK_TYPE (args.ptr[1], MSGPACK_OBJECT_BIN);

  msgpack_object_str type = args.ptr[0].via.str;
#define DISPATCH(TYPE) \
  if (type.size == sizeof #TYPE - 1 && \
      memcmp (type.ptr, #TYPE, type.size) == 0) \
    return Binary_decode_##TYPE (args.ptr[1].via.bin, res)
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
