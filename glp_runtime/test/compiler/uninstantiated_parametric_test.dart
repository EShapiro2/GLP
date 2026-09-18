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
        return s.contains('code:tagger/1') &&
            s.contains('no call in the program instantiates it');
      }, 'names the procedure and the missing instantiation')),
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
