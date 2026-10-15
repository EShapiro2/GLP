// glp_runtime/test/vglp/modules_test.dart
//
// A vGLP program of several modules: the one-module rule lifted.
// Spec: vGLP at a89a751, sections/parts/canonical-compilation.tex: "A vGLP
// program is organised in modules as a GLP program is.  A .vglp source in
// the directory tree of a program is compiled into a module of that program,
// as a .glp source is, and the self.vglp of a directory holds the
// procedures, volitional or not, and the widget declarations shared by every
// module in that directory and below it, as its self.glp holds those of GLP.
// A procedure that reaches a question is exported, imported and shared as
// any procedure is, with its ask stream added, so the questions of a program
// are those of all its modules.  The compilation forms the type of the asks,
// the construct processes and the dispatcher for the linked program, and
// the GLP module that forms the program's initial goal imports the compiled
// procedures it calls, ask streams included, and the dispatcher."  vGLP's
// task of 2026-10-15 18:28 UTC, items 1--4.
//
// Programs: programs/tests/vglp/family (a self.vglp holding the chore card,
// approve, and two modules, parent and child, each calling it and asking a
// question of its own) and programs/tests/vglp/imports (home.vglp importing
// from responder.vglp a volitional procedure and an ordinary procedure that
// reaches a question), each run with a scripted person; the copies of a
// question's types and the refusals are written under programs/ for their
// runs.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

const _programs = '../programs';
final _root = File('$_programs/self.glp').absolute.path;
String _program(String name) =>
    Directory('$_programs/tests/vglp/$name').absolute.path;

/// A run of [goal] in [program]: its status and each binding as GLP text, an
/// unbound variable shown `_` and an open list's tail `| _`.
Future<(ExecutionStatus, Map<String, String>)> _play(
    String program, String goal) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)
    ..loadProgram(_program(program));
  final result = await engine.runGoal(goal);
  String show(rt.Term? t) {
    if (t == null) return '_';
    if (t is rt.VarRef) {
      final d = engine.runtime.heap.dereference(t);
      return d is rt.VarRef ? '_' : show(d);
    }
    if (t is rt.ConstTerm) {
      return t.value == null || t.value == rt.nil ? '[]' : '${t.value}';
    }
    if (t is rt.StructTerm) {
      if (t.functor == '.' && t.args.length == 2) {
        final items = <String>[];
        rt.Term? cur = t;
        while (true) {
          final c = cur;
          if (c is rt.StructTerm && c.functor == '.' && c.args.length == 2) {
            items.add(show(c.args[0]));
            cur = c.args[1];
            continue;
          }
          if (c is rt.VarRef) {
            final d = engine.runtime.heap.dereference(c);
            if (d is rt.VarRef) return '[${items.join(', ')} | _]';
            cur = d;
            continue;
          }
          if (c is rt.ConstTerm && (c.value == null || c.value == rt.nil)) {
            return '[${items.join(', ')}]';
          }
          return '[${items.join(', ')} | ${show(c)}]';
        }
      }
      return '${t.functor}(${t.args.map(show).join(', ')})';
    }
    return '$t';
  }

  return (
    result.status,
    {for (final e in result.bindings.entries) e.key: show(e.value)}
  );
}

void main() {
  group('a self.vglp holding a card, and two modules, parent and child, each '
      'calling it (item 4, first test)', () {
    test('the self.vglp is no module: it joins the module of the self.glp '
        'beside it, and its procedure is that module\'s', () {
      final modules = discoverProgram(_program('family'),
          rootSelfGlpPath: File(_root).absolute.path);
      expect(modules.any((m) => m.filePath.endsWith('self.vglp')), isFalse);
      final self = modules.singleWhere((m) =>
          m.filePath.endsWith('/family/self.glp'));
      expect(self.ast.procedures.map((p) => p.signature),
          containsAll(['approve/3', 'approve1/4', 'person/3']));
      expect(self.ast.typeDefs.map((t) => t.name),
          containsAll(['Chore', 'Done', 'YesNo', 'Question']));
    });

    test('both modules at once, their questions served by one dispatcher: '
        'each module\'s card and its own question answered', () async {
      final (status, b) = await _play('family', 'play_family(P, C, L)');
      expect(status, isNot(ExecutionStatus.failed));
      expect(b['P'], '[done(dishes, yes), allowed(5)]');
      expect(b['C'], '[done(homework, no), wished(bike)]');
      for (final seen in [
        'chore(dishes)',
        'chore(homework)',
        'allowance',
        'wish',
        'withdrawn(1)',
        'withdrawn(2)',
        'withdrawn(3)',
        'withdrawn(4)',
      ]) {
        expect(b['L'], contains(seen));
      }
    });

    test('the parent\'s module alone, served by its own dispatcher', () async {
      final (status, b) = await _play('family', 'play_parent(P, L)');
      expect(status, isNot(ExecutionStatus.failed));
      expect(b['P'], '[done(dishes, yes), allowed(5)]');
      expect(b['L'], contains('chore(dishes)'));
      expect(b['L'], contains('allowance'));
    });
  });

  group('a module importing from another a volitional procedure and an '
      'ordinary procedure that reaches a question (item 4, second test)', () {
    test('the person answers the imported card twice, through respond and '
        'through offer_from, and the module\'s own question', () async {
      final (status, b) = await _play('imports', 'play_befriend(F, L)');
      expect(status, isNot(ExecutionStatus.failed));
      expect(b['F'],
          '[met(accept(bob)), heard(refuse(carol)), confirmed(bob, yes)]');
      for (final seen in [
        'card(bob)',
        'card(carol)',
        'ok(bob)',
        'withdrawn(1)',
        'withdrawn(2)',
        'withdrawn(3)',
      ]) {
        expect(b['L'], contains(seen));
      }
    });
  });

  group('the questions of the linked program, formed over every module', () {
    late Directory fixture;
    setUp(() {
      fixture = Directory('$_programs/vglp_modules_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}');
      fixture.createSync();
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });
    void write(String name, String content) =>
        File('${fixture.path}/$name').writeAsStringSync(content);
    String read(String name) =>
        File('${fixture.path}/$name').readAsStringSync();

    const a = '''
YesNo ::= yes ; no.
Apple ::= apple(YesNo?).
procedure (Apple)*pick_a(Stream(YesNo)).
(apple(A))*pick_a([A?]).
exported procedure go_a(Stream(YesNo)).
go_a(Xs?) :- pick_a(Xs).
''';
    const b = '''
Pear ::= pear(Integer?).
procedure (Pear)*pick_b(Stream(Integer)).
(pear(N))*pick_b([N?]).
exported procedure go_b(Stream(Integer)).
go_b(Xs?) :- pick_b(Xs).
''';

    test('each module\'s questions are every module\'s, a type its scope does '
        'not define copied into it, and the program is well-typed', () {
      write('self.glp', 'exported procedure ping(Constant).\nping(a).\n');
      write('a.vglp', a);
      write('b.vglp', b);
      emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_root).absolute.path);
      final ga = read('a.glp'), gb = read('b.glp');
      const question = 'Question ::= apple_w(Apple) ; pear_w(Pear).';
      expect(ga, contains(question));
      expect(gb, contains(question));
      // Each a copy of the other's type, and of the types it names.
      expect(ga, contains('Pear ::= pear(Integer?).'));
      expect(gb, contains('Apple ::= apple(YesNo?).'));
      expect(gb, contains('YesNo ::= yes ; no.'));
      // Each module's dispatcher serves both questions.
      expect(ga, contains('construct(Id, pear_w(X), Gs, Ds?)'));
      expect(gb, contains('construct(Id, apple_w(X), Gs, Ds?)'));
      File('${fixture.path}/a.glp').deleteSync();
      File('${fixture.path}/b.glp').deleteSync();
      expect(
          () => typeCheckProgram(
              discoverProgram(fixture.path, rootSelfGlpPath: _root),
              rootDir: fixture.path),
          returnsNormally);
    });

    test('a self.vglp holding procedures in a directory with no self.glp is '
        'refused, naming it', () {
      Directory('${fixture.path}/sub').createSync();
      write('self.glp', 'exported procedure ping(Constant).\nping(a).\n');
      write('sub/self.vglp', a);
      expect(
          () => discoverProgram(fixture.path, rootSelfGlpPath: _root),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              allOf(contains('sub/self.vglp'),
                  contains('its directory has no self.glp')))));
    });

    test('a call of a self.vglp\'s volitional procedure from a GLP module '
        'below it holds a writer at each output position, or is refused, '
        'naming it', () {
      write('self.glp', 'exported procedure ping(Constant).\nping(a).\n');
      write('self.vglp', a);
      write('m.glp', 'procedure bad(_).\nbad(D?) :- pick_a([], D).\n');
      expect(
          () => typeCheckProgram(
              discoverProgram(fixture.path, rootSelfGlpPath: _root),
              rootDir: fixture.path),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              allOf(contains('pick_a([], D)'),
                  contains('which is not a writer')))));
    });

    test('a name the self.glp and the self.vglp beside it both define is '
        'refused, naming both', () {
      write('self.glp', 'YesNo ::= yes ; no.\n'
          'exported procedure ping(Constant).\nping(a).\n');
      write('self.vglp', a);
      expect(
          () => discoverProgram(fixture.path, rootSelfGlpPath: _root),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              allOf(contains('self.glp and '), contains('self.vglp both '
                  'define the type YesNo')))));
    });
  });
}
