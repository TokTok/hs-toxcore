#include "methods.h"

METHOD (array, Binary_encode, CipherText) { return pending; }
METHOD (array, Binary_encode, DhtPacket) { return pending; }
METHOD (array, Binary_encode, HostAddress) { return pending; }
METHOD (array, Binary_encode, Int) { return pending; }
METHOD (array, Binary_encode, Key) { return pending; }

METHOD (array, Binary_encode, KeyPair)
{
  CHECK (args.ptr[1].type == MSGPACK_OBJECT_ARRAY);

  msgpack_object_array key_pair = args.ptr[1].via.array;

  CHECK (key_pair.size == 2);
  CHECK (key_pair.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (key_pair.ptr[1].type == MSGPACK_OBJECT_BIN);

  msgpack_object_bin secret_key = key_pair.ptr[0].via.bin;
  msgpack_object_bin public_key = key_pair.ptr[1].via.bin;

  CHECK (secret_key.size == 32);
  CHECK (public_key.size == 32);

  SUCCESS {
    uint8_t data[64];
    memcpy (data, secret_key.ptr, 32);
    memcpy (data + 32, public_key.ptr, 32);
    msgpack_pack_bin (res, 64);
    msgpack_pack_bin_body (res, data, 64);
  }

  return 0;
}

METHOD (array, Binary_encode, NodeInfo) { return pending; }
METHOD (array, Binary_encode, NodesRequest) { return pending; }
METHOD (array, Binary_encode, NodesResponse) { return pending; }
METHOD (array, Binary_encode, Packet) { return pending; }
METHOD (array, Binary_encode, PacketKind) { return pending; }
METHOD (array, Binary_encode, PingPacket) { return pending; }
METHOD (array, Binary_encode, PlainText) { return pending; }
METHOD (array, Binary_encode, PortNumber) { return pending; }
METHOD (array, Binary_encode, RpcPacket) { return pending; }
METHOD (array, Binary_encode, SocketAddress) { return pending; }
METHOD (array, Binary_encode, TransportProtocol) { return pending; }


METHOD (array, Binary, encode)
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
