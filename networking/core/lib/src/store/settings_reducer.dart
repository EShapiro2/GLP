import 'settings_state.dart';
import 'settings_actions.dart';

/// Reducer for settings state
SettingsState settingsReducer(SettingsState state, SettingsAction action) {
  if (action is HydrateSettingsAction) {
    return action.settings;
  }

  if (action is SetBleRoleModeAction) {
    return state.copyWith(bleRoleMode: action.mode);
  }

  if (action is SetColdCallTrustLevelAction) {
    return state.withTrustLevel(action.underlay, action.level);
  }

  return state;
}
