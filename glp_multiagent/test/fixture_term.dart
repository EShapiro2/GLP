// A test's fixture term, read from its source: a notify written as the
// agent's program would emit it, for the bridge, which is handed terms and
// parses no text (ui_runtime/runtime.dart, UiRuntime.handle; GLP #3 Cowork,
// 2026-10-11 12:59 UTC, "21:40 AND 03:16, (iii)").  Until 2026-10-11 the
// tests handed the bridge the text, which it parsed.
import 'package:glp_multiagent/ui_runtime/term.dart';

/// The ground term [source] writes; throws on source that writes none.
GTerm fixtureTerm(String source) {
  final t = tryParseTerm(source);
  if (t == null) throw ArgumentError('no ground term: $source');
  return t;
}
