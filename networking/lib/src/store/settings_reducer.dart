import 'settings_state.dart';
import 'settings_actions.dart';

List<RendezvousServerSettings> _dedupeServers(
  Iterable<RendezvousServerSettings> servers,
) {
  final deduped = <RendezvousServerSettings>[];
  final seen = <String>{};

  for (final server in servers) {
    final key = server.configKey;
    if (server.address.trim().isEmpty || server.pubkeyHex.trim().isEmpty) {
      continue;
    }
    if (seen.add(key)) {
      deduped.add(server);
    }
  }

  return deduped;
}

SettingsState _copyWithServers(
  SettingsState state,
  List<RendezvousServerSettings> servers,
) {
  final primary = servers.isNotEmpty ? servers.first : null;
  return state.copyWith(
    rendezvousServers: servers,
    anchorAddress: primary?.address,
    anchorPubkeyHex: primary?.pubkeyHex,
  );
}

/// Reducer for settings state
SettingsState settingsReducer(SettingsState state, SettingsAction action) {
  if (action is SetBluetoothEnabledAction) {
    return state.copyWith(bluetoothEnabled: action.enabled);
  }

  if (action is SetUdpEnabledAction) {
    return state.copyWith(udpEnabled: action.enabled);
  }

  if (action is UpdateTransportSettingsAction) {
    return state.copyWith(
      bluetoothEnabled: action.bluetoothEnabled,
      udpEnabled: action.udpEnabled,
      transportPriority: action.transportPriority,
    );
  }

  if (action is SetAnchorServerAction) {
    final hasValue = (action.anchorAddress?.isNotEmpty ?? false) &&
        (action.anchorPubkeyHex?.isNotEmpty ?? false);
    final servers = hasValue
        ? [
            RendezvousServerSettings(
              address: action.anchorAddress!,
              pubkeyHex: action.anchorPubkeyHex!,
            ),
          ]
        : const <RendezvousServerSettings>[];
    return _copyWithServers(state, servers);
  }

  if (action is SetRendezvousServersAction) {
    return _copyWithServers(state, _dedupeServers(action.servers));
  }

  if (action is AddRendezvousServerAction) {
    return _copyWithServers(
      state,
      _dedupeServers([
        ...state.configuredRendezvousServers,
        action.server,
      ]),
    );
  }

  if (action is RemoveRendezvousServerAction) {
    return _copyWithServers(
      state,
      state.configuredRendezvousServers
          .where((server) => server.configKey != action.server.configKey)
          .toList(growable: false),
    );
  }

  if (action is HydrateSettingsAction) {
    return action.settings;
  }

  if (action is SetBleRoleModeAction) {
    return state.copyWith(bleRoleMode: action.mode);
  }

  if (action is SetColdCallTrustLevelAction) {
    return state.copyWith(coldCallTrustLevel: action.level);
  }

  return state;
}
