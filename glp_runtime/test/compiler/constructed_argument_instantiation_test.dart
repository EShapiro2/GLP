/// A constructed argument instantiates a type parameter.
///
/// TGLP (parameterized-types.tex sec:param-procedures): "The concrete type at
/// the call site of an argument that is a variable is its declared type, and of
/// an argument that is a constructed term the type of the term itself---its
/// functor, the types of its constants and the declared types of its
/// variables---so a constructed argument instantiates a parameter exactly as a
/// variable does."  And: "Two bindings for one parameter conflict when the
/// types they supply have different type automata".
///
/// Until 2026-09-20 only an argument that was a variable bound a parameter, so a
/// call passing a constructed term induced no instantiation: the first two
/// programs below were rejected as carrying a parameter-inspecting procedure
/// nothing instantiates, and the third --- two constructed arguments whose types
/// have different automata at one parameter --- loaded, nothing having bound the
/// parameter to compare the second against.
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

  test('a constructed argument instantiates the parameter, the paper\'s shape',
      () {
    final dir =
        Directory('../programs/tests/param_constructed_arg').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });

  test('a parameter binds through a term inside a term', () {
    final dir =
        Directory('../programs/tests/param_constructed_nested').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });

  test('two constructed arguments with different automata at one parameter are rejected',
      () {
    final dir =
        Directory('../programs/tests/param_constructed_conflict').absolute.path;
    expect(
      () => engine.loadProgram(dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains('No transition for g(1,1)') &&
            s.contains(r'$f<Constant>?');
      }, 'names the second argument and the type the first bound')),
    );
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });
}
