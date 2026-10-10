/// `procedure` before "(" is a functor; `procedure p(X).` is a declaration.
///
/// GLP #3 Cowork, 2026-10-04 09:06 UTC, "23:49. Q2: `procedure` immediately
/// before "(" is a functor, as your item 1 reads every operator name;
/// `procedure p(X).` is a declaration; no word is reserved".  A declaration is
/// the keyword, its parameter list or none (TGLP parameterized-types.tex,
/// "Parameterised Procedure Declarations": "It names them in a list after the
/// keyword"), and a procedure name; any other item beginning `procedure` is a
/// clause of the procedure of that name, and `procedure` names a predicate in
/// a clause head and in a goal as any name does.  Until 2026-10-04 the reader
/// read it there as the declaration keyword, and a predicate named
/// `procedure` could not be written unquoted.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

Module _module(String source) =>
    Parser(Lexer(source).tokenize()).parseModule();

GlpEngine _engine() => GlpEngine(
    rootSelfGlpPath: File('../programs/self.glp').absolute.path);

/// A program whose predicate `procedure/1` is declared, defined and called,
/// in a head, a body after a guard, a body without one and a conjunction.
const String _program = '''
procedure procedure(Constant).
procedure(a).
procedure(b).

procedure(X) merge2(Stream(X)?, Stream(X)?, Stream(X)).
merge2([X | Xs], Ys, [X? | Zs?]) :- merge2(Ys?, Xs?, Zs).
merge2(Xs, [], Xs?).
merge2([], Ys, Ys?).

procedure first(Constant).
first(X?) :- procedure(X).

procedure guarded(Constant?, Constant).
guarded(K, X?) :- K? =?= go | procedure(X).
guarded(_, X?) :- otherwise | X = none.

procedure two(Constant, Constant).
two(X?, Y?) :- procedure(X), procedure(Y).
''';

void main() {
  group('the reader', () {
    test('procedure p(X). is a declaration, and its clauses follow', () {
      final m = _module('procedure p(Constant).\np(a).\n');
      expect(m.procDeclarations.map((d) => d.name), ['p']);
      expect(m.procedures.map((p) => '${p.name}/${p.arity}'), ['p/1']);
    });

    test('procedure(X) p(...). is a declaration with its parameter list', () {
      final m = _module(
          'procedure(X) id(X?, X).\nid(X, X?).\n'
          'exported procedure(X) id2(X?, X).\nid2(X, X?).\n');
      expect(m.procDeclarations.map((d) => d.name), ['id', 'id2']);
      expect(m.procDeclarations.map((d) => d.typeParams), [
        ['X'],
        ['X']
      ]);
    });

    test('imported procedure m#p(...) is a declaration', () {
      final m = _module('imported procedure lib#procedure(Constant).\n');
      expect(m.procDeclarations.single.name, 'procedure');
      expect(m.procDeclarations.single.imported, isTrue);
    });

    test('procedure(a). is a clause of procedure/1, declared by name', () {
      final m = _module(_program);
      expect(m.procDeclarations.map((d) => d.name),
          ['procedure', 'merge2', 'first', 'guarded', 'two']);
      final proc = m.procedures.firstWhere((p) => p.name == 'procedure');
      expect(proc.arity, 1);
      expect(proc.clauses.length, 2);
    });

    test('procedure(X) :- ... is a clause, its head the functor', () {
      final m = _module('procedure(X?) :- X = a.\n');
      expect(m.procDeclarations, isEmpty);
      expect(m.procedures.single.name, 'procedure');
      expect(m.procedures.single.clauses.single.head.args.length, 1);
    });

    test('procedure. is a clause of procedure/0', () {
      final m = _module('procedure.\n');
      expect(m.procDeclarations, isEmpty);
      expect(m.procedures.single.name, 'procedure');
      expect(m.procedures.single.arity, 0);
    });

    test('procedure(X) is a goal in a body, before and after a guard', () {
      final m = _module(_program);
      final goals = [
        for (final p in m.procedures)
          for (final c in p.clauses) ...?c.body
      ].where((g) => g.functor == 'procedure');
      expect(goals.length, 4);
      expect(goals.every((g) => g.args.length == 1), isTrue);
    });

    test('a declaration after clauses of procedure/1 is still a declaration',
        () {
      final m = _module('procedure procedure(Constant).\nprocedure(a).\n'
          'procedure q(Constant).\nq(b).\n');
      expect(m.procDeclarations.map((d) => d.name), ['procedure', 'q']);
      expect(m.procedures.map((p) => p.name), ['procedure', 'q']);
    });

    test('procedure(a) in a term is a structure, procedure alone a constant',
        () {
      final m = _module('procedure t(_).\nt(X?) :- X = f(procedure(a), procedure).\n');
      final goal = m.procedures.single.clauses.single.body!.single;
      final f = goal.args[1] as StructTerm;
      expect((f.args[0] as StructTerm).functor, 'procedure');
      expect((f.args[1] as ConstTerm).value, 'procedure');
    });
  });

  group('a program', () {
    late Directory dir;
    late String file;

    setUpAll(() {
      // Under the root, programs/ (TGLP modules.tex, "Scope construction").
      dir = Directory('../programs/tests').createTempSync('glp_procedure_');
      file = '${dir.path}/procs.glp';
      File(file).writeAsStringSync(_program);
    });

    tearDownAll(() => dir.deleteSync(recursive: true));

    test('loads and runs procedure/1 from a clause and from the prompt',
        () async {
      final engine = _engine();
      expect(engine.loadFile(file), isTrue);
      for (final (goal, name, value) in [
        ('first(X).', 'X', 'Const(a)'),
        ('guarded(go, X).', 'X', 'Const(a)'),
        ('guarded(stop, X).', 'X', 'Const(none)'),
        ('procedure(X).', 'X', 'Const(a)'),
        ('two(X, Y).', 'Y', 'Const(a)'),
        ('procedure(X), first(Y).', 'Y', 'Const(a)'),
      ]) {
        final r = await engine.runGoal(goal);
        expect(r.status, ExecutionStatus.succeeded, reason: '$goal ${r.error}');
        expect(r.bindings[name].toString(), value, reason: goal);
      }
    });
  });
}
