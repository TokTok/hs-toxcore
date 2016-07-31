#include "methods.h"

#include <DHT.h>
#include <network.h>

METHOD(bin, Binary_decode, CipherText) { return pending; }

METHOD(bin, Binary_decode, DhtPacket) { return pending; }

METHOD(bin, Binary_decode, HostAddress) { return pending; }

METHOD(bin, Binary_decode, Word64) { return pending; }

METHOD(bin, Binary_decode, Key) { return pending; }

METHOD(bin, Binary_decode, KeyPair) {
  SUCCESS {
    if (args.size != 64)
      msgpack_pack_nil(res);
    else {
      msgpack_pack_array(res, 2);
      msgpack_pack_bin(res, 32);
      msgpack_pack_bin_body(res, args.ptr, 32);
      msgpack_pack_bin(res, 32);
      msgpack_pack_bin_body(res, args.ptr + 32, 32);
    }
  }

  return 0;
}

METHOD(bin, Binary_decode, NodeInfo) {
  uint16_t data_processed;
  Node_format node;
  int len = unpack_nodes(&node, 1, &data_processed, (uint8_t const *)args.ptr,
                         args.size, 1);

  bool ip6_node =
      node.ip_port.ip.family == AF_INET6 || node.ip_port.ip.family == TCP_INET6;
  bool tcp =
      node.ip_port.ip.family == TCP_INET || node.ip_port.ip.family == TCP_INET6;

  uint16_t port = ntohs(node.ip_port.port);
  uint32_t ip4 = ntohl(node.ip_port.ip.ip4.uint32);
  uint32_t ip6_0 = ntohl(node.ip_port.ip.ip6.uint32[0]);
  uint32_t ip6_1 = ntohl(node.ip_port.ip.ip6.uint32[1]);
  uint32_t ip6_2 = ntohl(node.ip_port.ip.ip6.uint32[2]);
  uint32_t ip6_3 = ntohl(node.ip_port.ip.ip6.uint32[3]);

  SUCCESS {
    if (len > 0 && data_processed > 0 && data_processed == args.size) {
      msgpack_pack_array(res, 3);
      msgpack_pack_uint8(res, tcp);
      msgpack_pack_array(res, 2);
      msgpack_pack_array(res, 2);
      msgpack_pack_uint8(res, ip6_node);
      if (ip6_node) {
        msgpack_pack_array(res, 4);
        msgpack_pack_uint32(res, ip6_0);
        msgpack_pack_uint32(res, ip6_1);
        msgpack_pack_uint32(res, ip6_2);
        msgpack_pack_uint32(res, ip6_3);
      } else {
        msgpack_pack_uint32(res, ip4);
      }
      msgpack_pack_uint16(res, port);
      msgpack_pack_bin(res, crypto_box_PUBLICKEYBYTES);
      msgpack_pack_bin_body(res, &node.public_key, crypto_box_PUBLICKEYBYTES);
    } else {
      msgpack_pack_nil(res);
    }
  }

  return 0;
}

METHOD(bin, Binary_decode, NodesRequest) { return pending; }

METHOD(bin, Binary_decode, NodesResponse) { return pending; }

METHOD(bin, Binary_decode, Packet) { return pending; }

/* Only used for bootstrap nodes */
#define BOOTSTRAP_INFO_PACKET_ID 240

METHOD(bin, Binary_decode, PacketKind) {

  SUCCESS {
    if (args.size == 1) {
      uint8_t type = *args.ptr;
      if (type == NET_PACKET_PING_REQUEST || type == NET_PACKET_PING_RESPONSE ||
          type == NET_PACKET_GET_NODES || type == NET_PACKET_SEND_NODES_IPV6 ||
          type == NET_PACKET_COOKIE_REQUEST ||
          type == NET_PACKET_COOKIE_RESPONSE || type == NET_PACKET_CRYPTO_HS ||
          type == NET_PACKET_CRYPTO_DATA || type == NET_PACKET_CRYPTO ||
          type == NET_PACKET_LAN_DISCOVERY ||
          type == NET_PACKET_ONION_SEND_INITIAL ||
          type == NET_PACKET_ONION_SEND_1 || type == NET_PACKET_ONION_SEND_2 ||
          type == NET_PACKET_ANNOUNCE_REQUEST ||
          type == NET_PACKET_ANNOUNCE_RESPONSE ||
          type == NET_PACKET_ONION_DATA_REQUEST ||
          type == NET_PACKET_ONION_DATA_RESPONSE ||
          type == NET_PACKET_ONION_RECV_3 || type == NET_PACKET_ONION_RECV_2 ||
          type == NET_PACKET_ONION_RECV_1) {
        msgpack_pack_uint8(res, type);
      } else {
        msgpack_pack_nil(res);
      }
    } else {
      msgpack_pack_nil(res);
    }
  }
  return 0;
}

METHOD(bin, Binary_decode, PingPacket) { return pending; }

METHOD(bin, Binary_decode, PlainText) { return pending; }

METHOD(bin, Binary_decode, PortNumber) { return pending; }

METHOD(bin, Binary_decode, RpcPacket) { return pending; }

METHOD(bin, Binary_decode, SocketAddress) { return pending; }

METHOD(bin, Binary_decode, TransportProtocol) { return pending; }

METHOD(array, Binary, decode) {
  CHECK_SIZE(args, 2);
  CHECK_TYPE(args.ptr[0], MSGPACK_OBJECT_STR);
  CHECK_TYPE(args.ptr[1], MSGPACK_OBJECT_BIN);

  msgpack_object_str type = args.ptr[0].via.str;
#define DISPATCH(TYPE)                                                         \
  if (type.size == sizeof #TYPE - 1 &&                                         \
      memcmp(type.ptr, #TYPE, type.size) == 0)                                 \
  return Binary_decode_##TYPE(args.ptr[1].via.bin, res)
  DISPATCH(CipherText);
  DISPATCH(DhtPacket);
  DISPATCH(HostAddress);
  DISPATCH(Word64);
  DISPATCH(Key);
  DISPATCH(KeyPair);
  DISPATCH(NodeInfo);
  DISPATCH(NodesRequest);
  DISPATCH(NodesResponse);
  DISPATCH(Packet);
  DISPATCH(PacketKind);
  DISPATCH(PingPacket);
  DISPATCH(PlainText);
  DISPATCH(PortNumber);
  DISPATCH(RpcPacket);
  DISPATCH(SocketAddress);
  DISPATCH(TransportProtocol);
#undef DISPATCH

  return unimplemented;
}
