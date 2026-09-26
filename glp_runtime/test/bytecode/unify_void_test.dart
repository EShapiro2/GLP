/// `unify_void` leaves a fresh unbound writer, not a bound slot.
///
/// TGLP typed-glp.tex, "Anonymous variables": an anonymous variable "denotes a
/// fresh writer with no paired reader", and in a clause head, at a produced
/// position, it is written `_?`, where complementation (def:moded-head) "makes
/// it the fresh writer of the previous sentence".  The runner's WRITE arm used
/// to put `null` in the structure slot, which `_convertTentativeToStruct`
/// turned into `ConstTerm(null)`: that closes a stream the clause left open
/// --- `unknown/1` on it answered as a closed list does --- and the payload
/// serializer refused it across a link ("Cannot serialize constant type:
/// Null"), which is what stopped linkprobe12, linkprobe13 and linkprobe14
/// from booting.  The body arm, where the structure under construction is a
/// plain StructTerm, did nothing at all and did not advance S, so a body
/// structure holding a `_` never completed.
///
/// The program is ../programs/tests/anon_void.glp, whose four drivers are the
/// two controls (a genuinely unbound tail, and a closed list) and the two
/// shapes under test (`_?` at a produced head position, and `_` inside a term
/// the body constructs).
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final source =
      File('../programs/tests/anon_void.glp').absolute.readAsStringSync();

  GlpEngine fresh() {
    final e =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    e.loadSource(source);
    return e;
  }

  test('a genuinely unbound tail is unknown (control)', () async {
    final r = await fresh().runGoal('unbound_tail(G?, A)');
    expect(r.failed, isFalse, reason: 'Error: ${r.error}');
    expect(r.bindings['A'].toString(), contains('open'));
  });

  test('a closed list is not unknown (control)', () async {
    final r = await fresh().runGoal('closed_tail(A)');
    expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
    expect(r.bindings['A'].toString(), contains('other'));
  });

  test('`_?` at a produced head position leaves the slot unknown', () async {
    final r = await fresh().runGoal('anon_head(A)');
    expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
    expect(r.bindings['A'].toString(), contains('open'));
  });

  test('`_` inside a term the body constructs leaves the slot unknown',
      () async {
    final r = await fresh().runGoal('anon_body(A)');
    expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
    expect(r.bindings['A'].toString(), contains('open'));
  });
}
