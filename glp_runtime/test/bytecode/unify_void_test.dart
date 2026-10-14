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
/// The program is ../programs/tests/anon_void.glp, whose three drivers are the
/// two controls (a genuinely unbound tail, and a closed list) and the shape
/// under test, `_` inside a term the body constructs.  The fourth driver this
/// file ran until 2026-10-14, anon_head, had `_?` at a produced head position
/// leave a stream open; GLP-Spec glp.tex, Remark "Anonymous Variables"
/// (297db75), and TGLP "Anonymous variables" (2424f4c) now say "Anonymous
/// readers (`_?`, `_Name?`) are not permitted in a clause, in the head or the
/// body", so that clause is held refused here instead.
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

  test('`_?` at a produced head position is refused, not left open', () {
    // The clause anon_head's say/2 had: a clause writes every writer it
    // holds, closing a stream it does not extend (it now writes [hello(Me?)]).
    final e =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    expect(
      () => e.loadSource('$source\n'
          'procedure say(Constant?, Stream).\n'
          'say(Me, [hello(Me?) | _?]).\n'),
      throwsA(predicate((Object x) => x.toString().contains(
          '_? (anonymous reader) is not permitted in program clauses'))),
    );
  });

  test('`_` inside a term the body constructs completes the term', () async {
    // anon_body passes ch(S?, _) to look/2, which closes the `_` with []; the
    // body arm once left a structure holding a `_` incomplete.
    final r = await fresh().runGoal('anon_body(A)');
    expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
    expect(r.bindings['A'].toString(), contains('other'));
  });
}
