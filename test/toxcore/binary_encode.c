#include "methods.h"

#include <DHT.h>
#include <network.h>

METHOD(bin, Binary_encode, CipherText) { return pending; }

METHOD(array, Binary_encode, DhtPacket) { return pending; }

METHOD(array, Binary_encode, HostAddress) { return pending; }

METHOD(u64, Binary_encode, Word64) { return pending; }

METHOD(bin, Binary_encode, Key) { return pending; }

METHOD(array, Binary_encode, KeyPair) {
  CHECK_SIZE(args, 2);
  CHECK_TYPE(args.ptr[0], MSGPACK_OBJECT_BIN);
  CHECK_TYPE(args.ptr[1], MSGPACK_OBJECT_BIN);

  msgpack_object_bin secret_key = args.ptr[0].via.bin;
  msgpack_object_bin public_key = args.ptr[1].via.bin;

  CHECK_SIZE(secret_key, 32);
  CHECK_SIZE(public_key, 32);

  SUCCESS {
    uint8_t data[64];
    memcpy(data, secret_key.ptr, 32);
    memcpy(data + 32, public_key.ptr, 32);
    msgpack_pack_bin(res, 64);
    msgpack_pack_bin_body(res, data, 64);
  }

  return 0;
}

#define PACKED_NODE_SIZE_IP6                                                   \
  (1 + SIZE_IP6 + sizeof(uint16_t) + crypto_box_PUBLICKEYBYTES)
METHOD(array, Binary_encode, NodeInfo) {
  CHECK_SIZE(args, 3);

  CHECK_TYPE(args.ptr[0], MSGPACK_OBJECT_POSITIVE_INTEGER);
  uint64_t protocol = args.ptr[0].via.u64;

  CHECK_TYPE(args.ptr[1], MSGPACK_OBJECT_ARRAY);
  msgpack_object_array address = args.ptr[1].via.array;

  CHECK_SIZE(address, 2);
  CHECK_TYPE(address.ptr[0], MSGPACK_OBJECT_ARRAY);
  msgpack_object_array host_address = address.ptr[0].via.array;

  CHECK_TYPE(address.ptr[1], MSGPACK_OBJECT_POSITIVE_INTEGER);
  uint64_t port_number = address.ptr[1].via.u64;

  CHECK_SIZE(host_address, 2);
  CHECK_TYPE(host_address.ptr[0], MSGPACK_OBJECT_POSITIVE_INTEGER);
  uint64_t address_family = host_address.ptr[0].via.u64;

  CHECK_TYPE(args.ptr[2], MSGPACK_OBJECT_BIN);
  msgpack_object_bin public_key = args.ptr[2].via.bin;

  CHECK_SIZE(public_key, crypto_box_PUBLICKEYBYTES);

  IP_Port ipp;
  ipp.port = htons(port_number);

  switch (address_family) {
  case 0: {
    /* IPv4*/
    if (protocol == 1) {
      ipp.ip.family = TCP_INET;
    } else {
      ipp.ip.family = AF_INET;
    }

    CHECK_TYPE(host_address.ptr[1], MSGPACK_OBJECT_POSITIVE_INTEGER);
    uint64_t addr = host_address.ptr[1].via.u64;

    ipp.ip.ip4.uint32 = htonl(addr);
    break;
  }
  case 1: {
    /* IPv6 */
    if (protocol == 1) {
      ipp.ip.family = TCP_INET6;
    } else {
      ipp.ip.family = AF_INET6;
    }

    CHECK_TYPE(host_address.ptr[1], MSGPACK_OBJECT_ARRAY);
    msgpack_object_array addr = host_address.ptr[1].via.array;

    int i;
    for (i = 0; i < 4; ++i) {
      CHECK_TYPE(addr.ptr[i], MSGPACK_OBJECT_POSITIVE_INTEGER);
      uint64_t component = addr.ptr[i].via.u64;
      ipp.ip.ip6.uint32[i] = htonl(component);
    }
    break;
  }
  }

  Node_format node;
  node.ip_port = ipp;
  memcpy(&node.public_key, public_key.ptr, crypto_box_PUBLICKEYBYTES);

  uint8_t
      packed_node[PACKED_NODE_SIZE_IP6]; /* We assume IP6 because it's bigger */

  int len = pack_nodes(packed_node, sizeof packed_node, &node, 1);

  SUCCESS {
    msgpack_pack_bin(res, len);
    msgpack_pack_bin_body(res, packed_node, len);
  }
  return 0;
}

METHOD(bin, Binary_encode, NodesRequest) { return pending; }

METHOD(array, Binary_encode, NodesResponse) { return pending; }

METHOD(array, Binary_encode, Packet) { return pending; }

METHOD(u64, Binary_encode, PacketKind) {
  CHECK(args <= UINT8_MAX);

  SUCCESS {
    uint8_t type = args;
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
      msgpack_pack_bin(res, 1);
      msgpack_pack_bin_body(res, &type, 1);
    } else {
      msgpack_pack_nil(res);
    }
  }
  return 0;
}

METHOD(u64, Binary_encode, PingPacket) { return pending; }

METHOD(bin, Binary_encode, PlainText) { return pending; }

METHOD(u64, Binary_encode, PortNumber) { return pending; }

METHOD(array, Binary_encode, RpcPacket) { return pending; }

METHOD(array, Binary_encode, SocketAddress) { return pending; }

METHOD(u64, Binary_encode, TransportProtocol) { return pending; }

METHOD(array, Binary, encode) {
  CHECK_SIZE(args, 2);
  CHECK_TYPE(args.ptr[0], MSGPACK_OBJECT_STR);

  msgpack_object_str type = args.ptr[0].via.str;
#define DISPATCH(TYPE, UTYPE, LTYPE)                                           \
  do {                                                                         \
    if (type.size == sizeof #TYPE - 1 &&                                       \
        memcmp(type.ptr, #TYPE, type.size) == 0) {                             \
      CHECK_TYPE(args.ptr[1], MSGPACK_OBJECT_##UTYPE);                         \
      return Binary_encode_##TYPE(args.ptr[1].via.LTYPE, res);                 \
    }                                                                          \
  } while (0)
  DISPATCH(CipherText, BIN, bin);
  DISPATCH(DhtPacket, ARRAY, array);
  DISPATCH(HostAddress, ARRAY, array);
  DISPATCH(Word64, POSITIVE_INTEGER, u64);
  DISPATCH(Key, BIN, bin);
  DISPATCH(KeyPair, ARRAY, array);
  DISPATCH(NodeInfo, ARRAY, array);
  DISPATCH(NodesRequest, BIN, bin);
  DISPATCH(NodesResponse, ARRAY, array);
  DISPATCH(Packet, ARRAY, array);
  DISPATCH(PacketKind, POSITIVE_INTEGER, u64);
  DISPATCH(PingPacket, POSITIVE_INTEGER, u64);
  DISPATCH(PlainText, BIN, bin);
  DISPATCH(PortNumber, POSITIVE_INTEGER, u64);
  DISPATCH(RpcPacket, ARRAY, array);
  DISPATCH(SocketAddress, ARRAY, array);
  DISPATCH(TransportProtocol, POSITIVE_INTEGER, u64);
#undef DISPATCH

  return unimplemented;
}
