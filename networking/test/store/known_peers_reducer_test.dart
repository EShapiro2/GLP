import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/store/known_peers_actions.dart';
import 'package:grassroots_networking_core/src/store/known_peers_reducer.dart';
import 'package:grassroots_networking_core/src/store/known_peers_state.dart';

void main() {
  const peerA =
      'aabbccdd11223344aabbccdd11223344aabbccdd11223344aabbccdd11223344';
  const peerB =
      'eeff00112233445566778899aabbccddeeff00112233445566778899aabbccdd';
  const address = '[2001:db8::1]:4001';

  group('KnownPeerPutAction', () {
    test('adds a key with no address', () {
      const state = KnownPeersState.initial;

      final result = knownPeersReducer(state, KnownPeerPutAction(peerA));

      expect(result.isKnown(peerA), isTrue);
      expect(result.addressOf(peerA), isNull);
      expect(result.dialBook, isEmpty);
    });

    test('is a no-op for an already-known key (address preserved)', () {
      const state = KnownPeersState(known: {peerA: address});

      final result = knownPeersReducer(state, KnownPeerPutAction(peerA));

      expect(result, same(state));
      expect(result.addressOf(peerA), equals(address));
    });

    test('normalizes the key to lowercase', () {
      const state = KnownPeersState.initial;

      final result = knownPeersReducer(
        state,
        KnownPeerPutAction(peerA.toUpperCase()),
      );

      expect(result.isKnown(peerA), isTrue);
    });
  });

  group('KnownPeerRemovedAction', () {
    test('removes the key and its address', () {
      const state = KnownPeersState(known: {peerA: address, peerB: null});

      final result = knownPeersReducer(state, KnownPeerRemovedAction(peerA));

      expect(result.isKnown(peerA), isFalse);
      expect(result.isKnown(peerB), isTrue);
    });

    test('is a no-op for an unknown key', () {
      const state = KnownPeersState(known: {peerB: null});

      final result = knownPeersReducer(state, KnownPeerRemovedAction(peerA));

      expect(result, same(state));
    });
  });

  group('KnownPeerAddressUpdatedAction', () {
    test('creates the entry when the key is not yet known', () {
      // putPeerAddress creates a dial-book entry even for a peer never seen.
      const state = KnownPeersState.initial;

      final result = knownPeersReducer(
        state,
        KnownPeerAddressUpdatedAction(pubkeyHex: peerA, udpAddress: address),
      );

      expect(result.isKnown(peerA), isTrue);
      expect(result.addressOf(peerA), equals(address));
      expect(result.dialBook, equals({peerA: address}));
    });

    test('updates the address of a known key', () {
      const state = KnownPeersState(known: {peerA: '10.0.0.1:4001'});

      final result = knownPeersReducer(
        state,
        KnownPeerAddressUpdatedAction(pubkeyHex: peerA, udpAddress: address),
      );

      expect(result.addressOf(peerA), equals(address));
    });

    test('never clears an address (supply-only)', () {
      const state = KnownPeersState(known: {peerA: address});

      final result = knownPeersReducer(
        state,
        KnownPeerAddressUpdatedAction(pubkeyHex: peerA, udpAddress: ''),
      );

      expect(result, same(state));
      expect(result.addressOf(peerA), equals(address));
    });

    test('is a no-op when the address is unchanged', () {
      const state = KnownPeersState(known: {peerA: address});

      final result = knownPeersReducer(
        state,
        KnownPeerAddressUpdatedAction(pubkeyHex: peerA, udpAddress: address),
      );

      expect(result, same(state));
    });
  });

  group('KnownPeersLoadedAction', () {
    test('replaces the slice with the persisted snapshot', () {
      const state = KnownPeersState(known: {peerA: address});

      final result = knownPeersReducer(
        state,
        KnownPeersLoadedAction(const {peerB: null}),
      );

      expect(result.isKnown(peerA), isFalse);
      expect(result.isKnown(peerB), isTrue);
    });
  });

  group('serialization', () {
    test('round-trips through JSON', () {
      const state = KnownPeersState(known: {peerA: address, peerB: null});

      final restored = KnownPeersState.fromJson(state.toJson());

      expect(restored, equals(state));
    });
  });

  group('unknown action', () {
    test('returns state unchanged', () {
      const state = KnownPeersState(known: {peerA: null});

      final result = knownPeersReducer(state, 'unknown');

      expect(result, same(state));
    });
  });
}
