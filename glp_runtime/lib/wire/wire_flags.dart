/// Build switch for the canonical wire format (D3 sandbox).
///
/// Until the S7 cutover, the legacy serializer (`payload_serializer.dart`) is
/// the live default and its bytes are unchanged. The canonical codec path
/// (appendix `app:wire-format`) is reached only when this flag is on, so it can
/// be exercised by tests without disturbing the live path.
///
/// The default is read once from the environment variable `GLP_WIRE_CANONICAL`
/// (so spawned agent isolates inherit it from the process environment). A
/// single-isolate test may also flip [canonical] programmatically.
library;

import 'dart:io';

class WireFlags {
  /// When true, the multiagent payload path uses the canonical wire codec.
  static bool canonical = _envDefault();

  static bool _envDefault() {
    final v = Platform.environment['GLP_WIRE_CANONICAL'];
    return v == '1' || v == 'true';
  }
}
