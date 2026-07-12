import 'settings_state.dart';

/// Base class for settings-related actions
abstract class SettingsAction {}

/// Set Bluetooth enabled state
class SetBluetoothEnabledAction extends SettingsAction {
  final bool enabled;

  SetBluetoothEnabledAction(this.enabled);
}

/// Set UDP enabled state
class SetUdpEnabledAction extends SettingsAction {
  final bool enabled;

  SetUdpEnabledAction(this.enabled);
}

/// Update both transport settings at once
class UpdateTransportSettingsAction extends SettingsAction {
  final bool? bluetoothEnabled;
  final bool? udpEnabled;
  final List<TransportProtocol>? transportPriority;

  UpdateTransportSettingsAction({
    this.bluetoothEnabled,
    this.udpEnabled,
    this.transportPriority,
  });
}

/// Configure the rendezvous server address and public key.
///
/// The server has its own independent keypair — both the address and
/// the public key must be provided.
class SetAnchorServerAction extends SettingsAction {
  final String? anchorAddress;
  final String? anchorPubkeyHex;

  SetAnchorServerAction({this.anchorAddress, this.anchorPubkeyHex});
}

class SetRendezvousServersAction extends SettingsAction {
  final List<RendezvousServerSettings> servers;

  SetRendezvousServersAction(this.servers);
}

class AddRendezvousServerAction extends SettingsAction {
  final RendezvousServerSettings server;

  AddRendezvousServerAction(this.server);
}

class RemoveRendezvousServerAction extends SettingsAction {
  final RendezvousServerSettings server;

  RemoveRendezvousServerAction(this.server);
}

/// Hydrate settings from persistence
class HydrateSettingsAction extends SettingsAction {
  final SettingsState settings;

  HydrateSettingsAction(this.settings);
}

/// Set the BLE role mode (debug knob — see [BleRoleMode]).
class SetBleRoleModeAction extends SettingsAction {
  final BleRoleMode mode;

  SetBleRoleModeAction(this.mode);
}

/// Set whether nearby unknown peers may complete BLE first contact.
class SetColdCallTrustLevelAction extends SettingsAction {
  final ColdCallTrustLevel level;

  SetColdCallTrustLevelAction(this.level);
}
