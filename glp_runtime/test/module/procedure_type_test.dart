/// `M#Procedure`, the union of the declarations M exports (TGLP 44e1a60,
/// modules.tex, "Procedure declarations": "`M#Procedure` is the union of the
/// declarations `M` exports, the type `Procedure` of Section
/// sec:procedure-declarations restricted to them.  A meta-program is
/// parameterised by the type of the goals it interprets and called at it: a
/// meta-interpreter declared `procedure(G) reduce(Goal(G)?)`, with `Goal(G)
/// ::= true ; (Goal(G), Goal(G)) ; G`, is called as `reduce(Goal(M#Procedure)?)`
/// for the program `M` it interprets, and, `G` being an alternative of
/// `Goal(G)`, it is checked at each program it interprets, the functors of
/// `M`'s declarations distinct from `true` and `,`"; GLP #14, Code_inbox.md,
/// 2026-11-10 12:06 UTC; program_linker.dart, `_writeProcedureTypes`).
///
/// The linker writes the type into each module that names it, from the
/// exports of the module the qualifier names, M.glp or M/self.glp of the
/// referring module's directory; a qualifier naming no module, and a
/// parameterised export, are refused with the reason.  The last group runs the vanilla meta-interpreter
/// (programs/book/meta/plain/plain_meta.glp), one text, at two programs, each
/// typed by hand at the dual of its declaration, the form the checker types
/// today (Cloud #12 to Code, Code_inbox.md, 2026-11-10).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _rootSelf);

/// [t] dereferenced through, as functor(args) and constants.
String _shown(GlpEngine engine, rt.Term? t) {
  if (t == null) return '_';
  final d = engine.runtime.heap.dereference(t);
  if (d is rt.ConstTerm) return '${d.value}';
  if (d is rt.StructTerm) {
    return '${d.functor}(${d.args.map((a) => _shown(engine, a)).join(', ')})';
  }
  return '_';
}

/// The vanilla meta-interpreter, parameterised by the type of the goals it
/// interprets; the program's clauses as it reads them, reduce/2, are the
/// program's, in its self.glp, the ancestor scope, where Goal(G) is defined.
const String _meta = '''
exported procedure(G) run(Goal(G)?).
run(true).
run((A, B)) :- run(A?), run(B?).
run(A) :- otherwise | reduce(A?, B), run(B?).
''';

const String _prog = '''
exported procedure merge(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
exported procedure halt.
halt.
''';

void main() {
  late Directory prog;

  setUp(() {
    // Under the root, programs/ (TGLP modules.tex, "Scope construction").
    prog = Directory('../programs/tests').createTempSync('glp_metatypes_');
  });

  tearDown(() => prog.deleteSync(recursive: true));

  void write(String name, String text) =>
      File('${prog.path}/$name').writeAsStringSync(text);

  Matcher refused(String reason) => throwsA(predicate(
      (e) => e.toString().contains(reason), 'refused: $reason'));

  group('M#Procedure, the union of the declarations M exports', () {
    test('a program naming prog#Procedure loads, prog.glp exporting merge/3 and halt/0',
        () {
      write('prog.glp', _prog);
      write('self.glp', '''
procedure keep(prog#Procedure?).
keep(_).
exported procedure go.
go :- keep(halt).
''');
      expect(_engine().loadProgram(prog.path), isTrue);
    });

    test('and the directory prog, entered through its self.glp', () {
      Directory('${prog.path}/prog').createSync();
      write('prog/self.glp', _prog);
      write('self.glp', '''
procedure keep(prog#Procedure?).
keep(_).
exported procedure go.
go :- keep(halt).
''');
      expect(_engine().loadProgram(prog.path), isTrue);
    });

    test('a qualifier naming no module is refused, the module named', () {
      write('self.glp', '''
procedure keep(nowhere#Procedure?).
keep(_).
exported procedure go.
go :- keep(halt).
''');
      expect(() => _engine().loadProgram(prog.path),
          refused('nowhere#Procedure names no module nowhere'));
    });

    test('a parameterised export is refused, the declaration named', () {
      write('prog.glp', '''
exported procedure(X) id(X?, X).
id(X, X?).
''');
      write('self.glp', '''
procedure keep(prog#Procedure?).
keep(_).
exported procedure go.
go :- keep(id(1, _)).
''');
      expect(() => _engine().loadProgram(prog.path),
          refused('exports the parameterised declaration id/2'));
    });
  });

  group('the vanilla meta-interpreter at two programs, typed by hand at the dual of each declaration',
      () {
    test('merge', () async {
      write('meta.glp', _meta);
      write('prog.glp', _prog);
      write('self.glp', '''
Goal(G) ::= true ; (Goal(G), Goal(G)) ; G.
P ::= merge(Stream(Integer), Stream(Integer), Stream(Integer)?).
imported procedure meta#run(Goal(P)?).
procedure reduce(Goal(P)?, Goal(P)).
reduce(true, true).
reduce((A, B), (A?, B?)).
reduce(merge([X|Xs], Ys, [X?|Zs?]), merge(Xs?, Ys?, Zs)).
reduce(merge(Xs, [Y|Ys], [Y?|Zs?]), merge(Xs?, Ys?, Zs)).
reduce(merge([], [], []), true).
exported procedure go(Stream(Integer)).
go(Zs?) :- meta # run((merge([1, 2], [3], Xs), merge(Xs?, [4], Zs))).
''');
      final engine = _engine();
      expect(engine.loadProgram(prog.path), isTrue);
      final r = await engine.runGoal('go(Z).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_shown(engine, r.bindings['Z']), '.(1, .(2, .(3, .(4, []))))');
    });

    test('append', () async {
      write('meta.glp', _meta);
      write('prog.glp', '''
exported procedure append(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).
append([], Ys, Ys?).
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
''');
      write('self.glp', '''
Goal(G) ::= true ; (Goal(G), Goal(G)) ; G.
P ::= append(Stream(Integer), Stream(Integer), Stream(Integer)?).
imported procedure meta#run(Goal(P)?).
procedure reduce(Goal(P)?, Goal(P)).
reduce(true, true).
reduce((A, B), (A?, B?)).
reduce(append([], Ys, Ys?), true).
reduce(append([X|Xs], Ys, [X?|Zs?]), append(Xs?, Ys?, Zs)).
exported procedure go(Stream(Integer)).
go(Zs?) :- meta # run(append([1], [2, 3], Zs)).
''');
      final engine = _engine();
      expect(engine.loadProgram(prog.path), isTrue);
      final r = await engine.runGoal('go(Z).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_shown(engine, r.bindings['Z']), '.(1, .(2, .(3, [])))');
    });
  });
}
