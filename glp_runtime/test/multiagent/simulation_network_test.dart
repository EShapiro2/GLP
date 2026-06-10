/// Tests for the simulation networking realization.
///
/// Covers docs/ma/networking-seam-spec.md v0.2 Section 3 and the router-level
/// parts of Section 7: adjacency cut/restore (§7.2), reverse-order delivery
/// (§7.3), and trust level (§7.4). The full-stack baseline (§7.1) and plays
/// (§7.5) are exercised by the isolate test suites.

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// A delivery recorded by the router's onDeliver sink.
class _Delivered {
  final String toId;
  final PubKey fromPk;
  final Uint8List payload;
  final String messageId;
  _Delivered(this.toId, this.fromPk, this.payload, this.messageId);
}

/// A connectivity event recorded by the router's onConnectivity sink.
class _Conn {
  final String toId;
  final PubKey peerPk;
  final ConnectivityEvent event;
  _Conn(this.toId, this.peerPk, this.event);
}

/// Build a router with three registered agents (alice, bob, carol), all Open,
/// recording deliveries and connectivity events into the given lists.
SimulationRouter _router(List<_Delivered> delivered, List<_Conn> conn) {
  final r = SimulationRouter();
  for (final id in ['alice', 'bob', 'carol']) {
    r.register(id, generateKeyPair().pub);
    r.setTrustLevel(id, TrustLevel.open);
  }
  r.onDeliver = (toId, fromPk, payload, messageId, t) =>
      delivered.add(_Delivered(toId, fromPk, payload, messageId));
  r.onConnectivity = (toId, peerPk, t, event) =>
      conn.add(_Conn(toId, peerPk, event));
  return r;
}

Uint8List _bytes(List<int> b) => Uint8List.fromList(b);

void main() {
  group('NetworkDirectory', () {
    test('resolves id ⇄ pk both ways', () {
      final d = NetworkDirectory();
      final pk = generateKeyPair().pub;
      d.register('alice', pk);
      expect(d.pkOf('alice'), pk);
      expect(d.idOf(pk), 'alice');
      expect(d.pkOf('nobody'), isNull);
    });
  });

  group('PubKey', () {
    test('hex round-trips and value-equals', () {
      final pk = generateKeyPair().pub;
      expect(PubKey.fromHex(pk.hex), pk);
      expect(pk.hex.length, 64);
    });
  });

  group('SimulationRouter delivery', () {
    test('delivers to an adjacent, open peer with a messageId', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      r.routeSend('alice', 'bob', _bytes([1, 2, 3]));
      expect(delivered.length, 1);
      expect(delivered[0].toId, 'bob');
      expect(delivered[0].fromPk, r.directory.pkOf('alice'));
      expect(delivered[0].payload, [1, 2, 3]);
      expect(delivered[0].messageId, isNotEmpty);
    });

    test('drops sends to unknown peers and non-adjacent peers', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      r.routeSend('alice', 'stranger', _bytes([9])); // unknown
      expect(delivered, isEmpty);

      r.setPartialAdjacency(); // now nothing is adjacent unless declared
      r.routeSend('alice', 'bob', _bytes([9]));
      expect(delivered, isEmpty);
      r.setAdjacent('alice', 'bob');
      r.routeSend('alice', 'bob', _bytes([9]));
      expect(delivered.length, 1);
    });

    test('bootComplete connects and discovers every ordered adjacent pair', () {
      final conn = <_Conn>[];
      final r = _router([], conn);
      r.bootComplete();
      // 3 agents, total adjacency: 6 ordered pairs × {discovered, connected}.
      final connected =
          conn.where((c) => c.event == ConnectivityEvent.connected).length;
      final discovered =
          conn.where((c) => c.event == ConnectivityEvent.discovered).length;
      expect(connected, 6);
      expect(discovered, 6);
    });
  });

  group('Trust (§7.4)', () {
    test('Closed receiver drops a cold-call from an unknown agent', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      r.setTrustLevel('bob', TrustLevel.closed);
      r.routeSend('alice', 'bob', _bytes([1])); // alice never contacted by bob
      expect(delivered, isEmpty);
    });

    test('Open receiver accepts a cold-call', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      r.setTrustLevel('bob', TrustLevel.open);
      r.routeSend('alice', 'bob', _bytes([1]));
      expect(delivered.length, 1);
    });

    test('Closed receiver accepts once it has contacted the sender', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      r.setTrustLevel('bob', TrustLevel.closed);
      r.routeSend('bob', 'alice', _bytes([0])); // bob contacts alice first
      delivered.clear();
      r.routeSend('alice', 'bob', _bytes([1])); // no longer first contact
      expect(delivered.length, 1);
    });
  });

  group('Adjacency cut/restore (§7.2)', () {
    test('cut queues messages and fires disconnect on both sides', () {
      final delivered = <_Delivered>[];
      final conn = <_Conn>[];
      final r = _router(delivered, conn);
      r.cut('alice', 'bob');
      expect(
          conn.where((c) => c.event == ConnectivityEvent.disconnected).length,
          2);
      r.routeSend('alice', 'bob', _bytes([1]));
      r.routeSend('alice', 'bob', _bytes([2]));
      expect(delivered, isEmpty); // queued while cut
    });

    test('restore flushes queued messages in order and reconnects both sides',
        () {
      final delivered = <_Delivered>[];
      final conn = <_Conn>[];
      final r = _router(delivered, conn);
      r.cut('alice', 'bob');
      r.routeSend('alice', 'bob', _bytes([1]));
      r.routeSend('alice', 'bob', _bytes([2]));
      conn.clear();
      r.restore('alice', 'bob');
      expect(
          conn.where((c) => c.event == ConnectivityEvent.connected).length, 2);
      expect(delivered.map((d) => d.payload[0]).toList(), [1, 2]); // FIFO
    });
  });

  group('Hold / reverse-order release (§7.3)', () {
    test('hold defers delivery with no callbacks; release flushes in reverse',
        () {
      final delivered = <_Delivered>[];
      final conn = <_Conn>[];
      final r = _router(delivered, conn);
      r.holdDelivery('alice', 'bob');
      expect(conn, isEmpty); // invisible: no connectivity callbacks
      r.routeSend('alice', 'bob', _bytes([1])); // carrier
      r.routeSend('alice', 'bob', _bytes([2])); // assignment
      expect(delivered, isEmpty);
      r.releaseDelivery('alice', 'bob');
      // Reverse-order hook: later message delivered first.
      expect(delivered.map((d) => d.payload[0]).toList(), [2, 1]);
    });
  });

  group('SimulationNetworkClient', () {
    test('send forwards to the router and round-trips a delivery', () {
      final delivered = <_Delivered>[];
      final r = _router(delivered, []);
      final client = SimulationNetworkClient(
        selfId: 'alice',
        directory: r.directory,
        sendToRouter: (toId, payload) => r.routeSend('alice', toId, payload),
      );
      client.send(r.directory.pkOf('bob')!, _bytes([7, 7]));
      expect(delivered.length, 1);
      expect(delivered[0].toId, 'bob');
      expect(delivered[0].payload, [7, 7]);
    });

    test('real Ed25519 sign/verify round-trips; tamper and wrong key fail', () {
      final kp = generateKeyPair();
      final client = SimulationNetworkClient(
        selfId: 'alice',
        directory: NetworkDirectory(),
        sendToRouter: (_, __) {},
      );
      client.putIdentity(kp.pub, kp.priv);

      final msg = _bytes([10, 20, 30, 40]);
      final sig = client.sign(msg);
      expect(client.verify(kp.pub, msg, sig), isTrue);

      final tampered = _bytes([10, 20, 30, 41]);
      expect(client.verify(kp.pub, tampered, sig), isFalse);

      final other = generateKeyPair().pub;
      expect(client.verify(other, msg, sig), isFalse);
    });

    test('getPeers excludes self; identity round-trips', () {
      final r = _router([], []);
      final client = SimulationNetworkClient(
        selfId: 'alice',
        directory: r.directory,
        sendToRouter: (_, __) {},
      );
      final kp = generateKeyPair();
      client.putIdentity(kp.pub, kp.priv);
      expect(client.getIdentity(), kp.pub);
      final peerIds =
          client.getPeers().map((p) => r.directory.idOf(p.pk)).toSet();
      expect(peerIds, {'bob', 'carol'});
    });

    test('IP methods throw UnsupportedError', () {
      final client = SimulationNetworkClient(
        selfId: 'alice',
        directory: NetworkDirectory(),
        sendToRouter: (_, __) {},
      );
      expect(() => client.getPublicAddress(), throwsUnsupportedError);
      expect(() => client.generatePeerLink(), throwsUnsupportedError);
      expect(() => client.consumePeerLink('x'), throwsUnsupportedError);
    });
  });
}
