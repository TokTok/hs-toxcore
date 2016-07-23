#include "methods.h"

#include <DHT.h>

METHOD (bin, Binary_decode, CipherText) { return pending; }
METHOD (bin, Binary_decode, DhtPacket) { return pending; }
METHOD (bin, Binary_decode, HostAddress) { return pending; }
METHOD (bin, Binary_decode, Word64) { return pending; }
METHOD (bin, Binary_decode, Key) { return pending; }

METHOD (bin, Binary_decode, KeyPair)
{
  SUCCESS {
    if (args.size != 64)
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

METHOD (bin, Binary_decode, NodeInfo)
{
  uint16_t data_processed = 0;
  Node_format node;
  int len = unpack_nodes(&node, 1, &data_processed, args.ptr, args.size, 1);

  bool ip6_node = (args.size == 51);
  bool tcp = ((node.ip_port.ip.family == TCP_INET) || (node.ip_port.ip.family == TCP_INET6));

  CHECK (len > 0);
  CHECK (data_processed > 0);

  SUCCESS {
    msgpack_pack_array(res, 3);
      msgpack_pack_uint8(res, tcp);
      msgpack_pack_array(res, 2);
        msgpack_pack_array(res, 2);
          msgpack_pack_uint32(res, ip6_node);
          if (ip6_node) {
            msgpack_pack_array(res, 4);
              msgpack_pack_uint32(res, node.ip_port.ip.ip6.uint32[0]);
              msgpack_pack_uint32(res, node.ip_port.ip.ip6.uint32[1]);
              msgpack_pack_uint32(res, node.ip_port.ip.ip6.uint32[2]);
              msgpack_pack_uint32(res, node.ip_port.ip.ip6.uint32[3]);
          } else {
            msgpack_pack_uint32(res, node.ip_port.ip.ip4.uint32);
          }
        msgpack_pack_uint32(res, node.ip_port.port);
      msgpack_pack_bin(res, crypto_box_PUBLICKEYBYTES);
      msgpack_pack_bin_body(res, &node.public_key, sizeof(crypto_box_PUBLICKEYBYTES));
  }
}



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
  DISPATCH (Word64);
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
