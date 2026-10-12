import '../models/underlay.dart';
import 'settings_state.dart';

/// Base class for settings-related actions.
///
/// There are no actions for the underlays or the rendezvous servers: they are
/// constructor arguments of the layer, fixed for its lifetime (GNA
/// §Instantiation).
abstract class SettingsAction {}

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

/// Set the cold-call trust level of one proximity underlay, leaving every
/// other underlay's untouched (spec §Trust levels — the two levels are
/// independent).
class SetColdCallTrustLevelAction extends SettingsAction {
  final ProximityUnderlay underlay;
  final ColdCallTrustLevel level;

  SetColdCallTrustLevelAction(this.underlay, this.level);
}
