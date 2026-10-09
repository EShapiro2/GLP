/// An imported declaration types the cross-module call it names and nothing
/// else.
///
/// TGLP modules.tex, "Cross-module type checking": "A cross-module call
/// M # p(a1, ..., an) in module N is well-typed if N contains a declaration
/// imported procedure M#p(T1, ..., Tn)": the import declares M#p, not p.
/// Until 2026-10-09 the checker matched every declaration in scope to the
/// clauses of the unit it checks by name and arity alone (type_checker.dart,
/// `check`), so an `imported procedure other # q(Kind?)` was checked against
/// the clauses of any module defining its own q/1: the smallest case below, a
/// program whose self.glp holds the import and whose sub/m2.glp defines its own
/// q(String?), was refused at the import's line, reported as sub/m2.glp:2, x
/// and y uncovered (Code #6, 2026-10-07 09:34 UTC; GLP, 2026-10-09 18:38 UTC,
/// "09:34").
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

void main() {
  late Directory prog;

  setUp(() {
    // Under the root, programs/ (TGLP modules.tex, "Scope construction").
    prog = Directory('../programs/tests').createTempSync('glp_import_qual_');
    // The smallest case, and an entry point, without which the directory is
    // no program to load (modules.tex, "External access").
    File('${prog.path}/self.glp').writeAsStringSync(
        'Kind ::= x ; y.\nimported procedure other # q(Kind?).\n'
        'exported procedure ok(String).\nok(yes).\n');
    Directory('${prog.path}/sub').createSync();
    File('${prog.path}/sub/m2.glp').writeAsStringSync(
        'exported procedure q(String?).\nq(a).\nq(b).\n');
  });

  tearDown(() => prog.deleteSync(recursive: true));

  test("a module's own q/1 is not checked against an import of other#q/1", () {
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    expect(engine.loadProgram(prog.path), isTrue);
  });

  test("the module's own q/1 is still checked against its own declaration",
      () {
    File('${prog.path}/sub/m2.glp').writeAsStringSync(
        'exported procedure q(Kind?).\nq(x).\n');
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    expect(
        () => engine.loadProgram(prog.path),
        throwsA(predicate(
            (e) =>
                e.toString().contains('m2.glp') &&
                e.toString().contains('uncovered alternative "y"'),
            'refused, y uncovered by q(x) under its own q(Kind?)')));
  });

  group('one unit importing other#q and defining its own q', () {
    const unit = '''
Kind ::= x ; y.
imported procedure other#q(Kind?).
exported procedure q(String?).
q(a).
q(b).
procedure go.
''';

    test('its own clauses are checked against its own q(String?) alone', () {
      final result = checkSource('${unit}go :- other # q(x).\n');
      expect(result.errors, isEmpty, reason: '${result.errors}');
    });

    test('the import types the call other # q(...)', () {
      final result = checkSource('${unit}go :- other # q(a).\n');
      expect(result.errors, isNotEmpty,
          reason: 'a is no Kind, and other#q takes a Kind');
    });
  });
}
