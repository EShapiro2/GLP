/// A lifted declaration keeps the root's types and the exposing module's.
///
/// TGLP modules.tex, Definition (Root, Scope): the scope of a module is
/// Pi + d_1.self + ... + d_k.self + M, the root among them; "The -expose
/// directive" lifts a module's exports "as if defined in its self.glp", whose
/// scope is that.  So the known names of a lifted declaration are the root's
/// definitions and the exposing module's, both, and a parameter of the
/// declaration is neither and stays bare (parameterized-types.tex, "Declaration
/// parameters").
///
/// Until this test, the lift entered the exposing module's parameterised types
/// as known MONOMORPHIC names, so a wildcard instance of one collapsed to its
/// bare name, which no scope defines: every program under the root failed with
/// "Unresolved type: Stream" at social/graph/routing/intro.glp:13, the root
/// self.glp both defining Stream and exposing intro_await_peer over
/// Stream(C).  The fixture has the same shape one level down: its self.glp
/// defines Pair(X) and exposes swap/2 over Pair(C), C a parameter.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;

  test('a declaration exposed over the exposer\'s own template loads and runs',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    const dir = '../programs/tests/expose/template_lift';
    expect(engine.loadProgram(dir), isTrue);

    // run/1 swaps pair(a, b) through the exposed swap/2 and reads the first
    // element of the result.
    final result = await engine.runGoal('run(T)');
    expect(result.succeeded, isTrue, reason: 'Error: ${result.error}');
    expect(result.bindings['T'].toString(), 'Const(b)');
  });

  test('a single-module load under the root keeps the root\'s Stream', () {
    // channel_consumer_closed.glp names Channel and Stream, and the root
    // self.glp exposes the routing modules over Stream(C): the lift into its
    // scope must not lose Stream.
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    final path = File(
            '../programs/tests/moded_types/valid/channel_consumer_closed.glp')
        .absolute
        .path;
    expect(engine.loadFile(path), isTrue);
  });
}
