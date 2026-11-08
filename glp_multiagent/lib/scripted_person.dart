/// The scripted person of a scripted build (GLP/phones/docs/debugging.md,
/// "The scripted build"): a scripted person of
/// `programs/social/graph/core/superapp_plays.glp`, run by the runtime at the
/// person's channel as under `:boot` (IGLP, Implementation Notes, "Boot": a
/// scripted person is a program the runtime runs at the person's channel), in
/// place of the person on the screen.
///
/// The person's first act names the entry, `sg_init(Me, Mode, Ks)`, which
/// core's agent/2 takes as a boot's person sends it (programs/social/
/// nearby_boot.glp, `say/4`; core's self.glp, CoreAct and core_start/2): Me
/// the agent's own key, Mode the scripted person, Ks the keys of the persons
/// the person knows at the start.  It is sent in place of `superapp(Me, [])`
/// at the agent's first start ([SuperAppHost.firstAct]).
///
/// The build chooses the person by the Dart define `GLP_PERSON=<mode>`, and
/// the persons known at the start by `GLP_KNOWN`, their keys in hex,
/// separated by commas, none where it is not given (main_scripted.dart).
library;

import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'isolate_protocol.dart' show runtimeTermOf;
import 'ui_runtime/term.dart';

/// The define that chooses the scripted person.
const String scriptedMode = String.fromEnvironment('GLP_PERSON');

/// The define that gives the keys the person knows at the start.
const String scriptedKnown = String.fromEnvironment('GLP_KNOWN');

/// A scripted person: its mode and the keys it knows at the start.
class ScriptedPerson {
  /// The scripted persons a scripted build runs, the plays on phones:
  /// sg_init/4's nearby modes in superapp_plays.glp (Code #26's task,
  /// Cloud-Reporter_inbox.md 2026-11-08 16:38 UTC).
  static const List<String> modes = [
    'encounter_befriend',
    'encounter',
    'befriend_known',
    'encounter_introduce',
  ];

  final String mode;
  final List<String> known;

  ScriptedPerson._(this.mode, this.known);

  /// The scripted person of [mode] knowing [known], each key 64 hex
  /// characters; throws [FormatException] for a mode not in [modes] or a key
  /// that is not one.
  factory ScriptedPerson(String mode, [List<String> known = const []]) {
    if (!modes.contains(mode)) {
      throw FormatException(
          'GLP_PERSON is "$mode", which is none of ${modes.join(', ')}');
    }
    final keys = [for (final k in known) k.trim().toLowerCase()];
    for (final k in keys) {
      if (!RegExp(r'^[0-9a-f]{64}$').hasMatch(k)) {
        throw FormatException('GLP_KNOWN holds "$k", which is not a key');
      }
    }
    return ScriptedPerson._(mode, List.unmodifiable(keys));
  }

  /// The scripted person [mode] knowing [known], the keys comma-separated as
  /// the define `GLP_KNOWN` gives them.
  factory ScriptedPerson.parse(String mode, String known) => ScriptedPerson(
      mode, [for (final k in known.split(',')) if (k.trim().isNotEmpty) k]);

  /// The person's first act, `sg_init(Me, Mode, Ks)`, [me] the agent's key.
  rt.Term firstAct(String me) => runtimeTermOf(GStruct('sg_init', [
        GAtom(me),
        GAtom(mode),
        GList([for (final k in known) GAtom(k)]),
      ]));
}
