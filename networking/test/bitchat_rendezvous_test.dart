import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking/src/store/settings_state.dart';

void main() {
  group('shouldAcceptRendezvousReply', () {
    const configuredPubkey =
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
    const pendingPubkey =
        'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';

    test('accepts already-configured rendezvous pubkey', () {
      final accepted = shouldAcceptRendezvousReply(
        configuredPubkey,
        settings: const SettingsState(
          rendezvousServers: [
            RendezvousServerSettings(
              address: '[2001:db8::10]:9516',
              pubkeyHex: configuredPubkey,
            ),
          ],
        ),
        pendingResponsePubkeys: const <String>[],
      );

      expect(accepted, isTrue);
    });

    test('accepts pending save probe reply before server is stored', () {
      final accepted = shouldAcceptRendezvousReply(
        pendingPubkey,
        settings: const SettingsState(),
        pendingResponsePubkeys: const <String>[pendingPubkey],
      );

      expect(accepted, isTrue);
    });

    test('rejects unrelated pubkey', () {
      final accepted = shouldAcceptRendezvousReply(
        'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc',
        settings: const SettingsState(),
        pendingResponsePubkeys: const <String>[pendingPubkey],
      );

      expect(accepted, isFalse);
    });
  });
}
