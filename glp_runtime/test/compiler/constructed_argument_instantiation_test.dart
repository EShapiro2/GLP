/// A call whose argument is a constructed term, and the parameter the callee's
/// own clauses fix.
///
/// TGLP (parameterized-types.tex def:instantiation): "A map \theta from those
/// parameters to types of the program is an instantiation of A if C and the
/// clauses of q are well-typed (Definition "Well-Typed Clause") when q's
/// declaration is replaced by its expansion under \theta."
///
/// A variable argument constrains the parameter by an equation; a constructed
/// term constrains it by containment, its type having to be admitted by whatever
/// the parameter is bound to.  Where the call's own arguments leave a parameter
/// open, the callee's clauses fix it: a variable pair of the callee's head that
/// the declaration types by the parameter on one side and by a concrete type on
/// the other must be dual (def:well-typed-clause 3(a)), and that is the
/// equation.
///
/// Where no equation reaches a parameter, coverage selects the map: def:instantiation
/// ends "...and every input path of that declaration is accepted by some clause of
/// q", so the map carries exactly the constructors the callee's heads match at that
/// position --- a map carrying an alternative no clause matches leaves an input path
/// unaccepted, and one missing an alternative a head matches makes that head
/// inconsistent.  The last two tests are that rule and its failure.
///
/// Until 2026-09-20 only a variable argument bound a parameter, so a call
/// passing a constructed term induced no instantiation and the first two
/// programs below were rejected as carrying a parameter-inspecting procedure
/// nothing instantiates.  Between fbd9040 and e56c303 the term's own type was
/// the binding, which is one alternative of the union the callee's clauses
/// require and broke duality in the callee's head.
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

  test('a constructed argument at a parameter the callee\'s clauses fix', () {
    final dir =
        Directory('../programs/tests/param_constructed_arg').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });

  test('a term inside a term at a parameter position', () {
    final dir =
        Directory('../programs/tests/param_constructed_nested').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });

  test('a constructed argument the parameter\'s binding does not admit is rejected',
      () {
    final dir =
        Directory('../programs/tests/param_constructed_conflict').absolute.path;
    expect(
      () => engine.loadProgram(dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains('No transition for bad(1,1)') &&
            s.contains('from state Msg?');
      }, 'names the argument and the type the parameter is bound to')),
    );
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('a parameter no equation reaches is the constructors its callee\'s heads match',
      () {
    final dir =
        Directory('../programs/tests/param_theta_covered').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
  });

  test('a map leaving an input path unaccepted is not an instantiation', () {
    final dir =
        Directory('../programs/tests/param_theta_uncovered').absolute.path;
    expect(
      () => engine.loadProgram(dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains('code:pick/3') &&
            s.contains('no call in the program instantiates it');
      }, 'names the procedure no map instantiates')),
    );
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });
}
