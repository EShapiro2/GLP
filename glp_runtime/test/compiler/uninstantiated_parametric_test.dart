/// An uninstantiated parameterised procedure in a linked program.
///
/// TGLP (parameterized-types.tex sec:abstract-parameters): "Where a program
/// contains a parameterised procedure that no call in it instantiates and that
/// is not parametrically well-typed, compilation rejects the program." A
/// procedure that inspects none of its parameters is parametrically well-typed
/// --- certified once by its abstract instance --- and is not rejected.
///
/// Until 2026-09-18 the linked check printed a `[TYPE] N parameterized
/// procedure(s) unchecked in this program` line and loaded the program.
///
/// Since 2026-10-02 the call that leaves the procedure uninstantiated is itself
/// refused, in the module's own check: no site of it supplies a type for the
/// parameters, and the callee is not parametrically well-typed (TGLP
/// appendix-implementation-notes.tex, "The instantiation of a call").
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  late GlpEngine engine;

  setUp(() {
    engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  });

  test('a parameter-inspecting procedure no call instantiates rejects the program',
      () {
    final dir = Directory('../programs/tests/param_unchecked').absolute.path;
    expect(
      () => engine.loadProgram(dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains(
                'No instantiation of tagger/1 is found for the call '
                'tagger(Ch?): no site of the call supplies a type for X, Y') &&
            s.contains('tagger/1 is not parametrically well-typed, so the '
                'call is refused');
      }, 'refuses the call, naming the parameters no site supplies')),
    );
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('a procedure that inspects no parameter keeps its abstract certificate',
      () {
    final dir =
        Directory('../programs/tests/param_abstract_linked').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });
}
