/// `M#T`, a type that a declaration M exports references, under M's
/// definition of it (TGLP modules.tex, "Procedure declarations": "Types
/// referenced in an imported declaration are resolved against the caller's
/// type scope, or, written `M#T`, against the exports of module `M`: `M#T` is
/// the type `T` that a declaration `M` exports references, under `M`'s
/// definition of it, so a module importing a compiled procedure names its
/// types once, by the module that defines them, and writes no copy of
/// them"; GLP #14, Code_inbox.md, 2026-11-10 13:23 UTC; program_linker.dart,
/// `_writeQualifiedTypes`).  Read in an imported declaration and in every
/// type expression of the importing module, with type arguments; a type M
/// defines that M#T's definition names is named M#U, so it does not meet
/// the referring module's own type of that name; refused with the reason
/// where M defines no T, or no declaration M exports references it.
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

const String _colours = '''
Colour ::= red ; green.
exported procedure pick(Colour).
pick(red).
''';

void main() {
  late Directory prog;

  setUp(() {
    // Under the root, programs/ (TGLP modules.tex, "Scope construction").
    prog = Directory('../programs/tests').createTempSync('glp_qualified_');
  });

  tearDown(() => prog.deleteSync(recursive: true));

  void write(String name, String text) =>
      File('${prog.path}/$name').writeAsStringSync(text);

  Matcher refused(String reason) => throwsA(predicate(
      (e) => e.toString().contains(reason), 'refused: $reason'));

  Future<String> run(String goal) async {
    final engine = _engine();
    expect(engine.loadProgram(prog.path), isTrue);
    final r = await engine.runGoal('$goal.');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    return _shown(engine, r.bindings['C']);
  }

  test('an imported declaration names a#Colour (Cloud #16, Code_inbox.md, 2026-11-10 13:15 UTC, point 2)',
      () async {
    write('a.glp', _colours);
    write('self.glp', '''
imported procedure a#pick(a#Colour).
exported procedure go(a#Colour).
go(C?) :- a # pick(C).
''');
    expect(await run('go(C)'), 'red');
  });

  test('a type definition names a#Colour', () async {
    write('a.glp', _colours);
    write('self.glp', '''
Pair ::= pair(a#Colour, a#Colour).
imported procedure a#pick(a#Colour).
exported procedure go(Pair).
go(pair(C?, D?)) :- a # pick(C), a # pick(D).
''');
    expect(await run('go(C)'), 'pair(red, red)');
  });

  test('a#Box(Integer), with a type argument', () async {
    write('a.glp', '''
Box(X) ::= box(X).
exported procedure wrap(Box(Integer)).
wrap(box(1)).
''');
    write('self.glp', '''
imported procedure a#wrap(a#Box(Integer)).
exported procedure go(a#Box(Integer)).
go(B?) :- a # wrap(B).
''');
    final engine = _engine();
    expect(engine.loadProgram(prog.path), isTrue);
    final r = await engine.runGoal('go(B).');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_shown(engine, r.bindings['B']), 'box(1)');
  });

  test("a type M#T's definition names is M's, beside the referrer's own of that name",
      () async {
    write('a.glp', '''
Colour ::= red ; green.
Shade ::= shade(Colour).
exported procedure pick(Shade).
pick(shade(red)).
''');
    write('self.glp', '''
Colour ::= blue.
imported procedure a#pick(a#Shade).
exported procedure go(a#Shade).
go(C?) :- a # pick(C).
''');
    expect(await run('go(C)'), 'shade(red)');
  });

  test('a type M does not define is refused, M and the type named', () {
    write('a.glp', _colours);
    write('self.glp', '''
exported procedure go(a#Shade).
go(dark).
''');
    expect(() => _engine().loadProgram(prog.path),
        refused('a.glp defines no type Shade'));
  });

  test('a type no declaration M exports references is refused', () {
    write('a.glp', '''
Colour ::= red ; green.
Hidden ::= h.
exported procedure pick(Colour).
pick(red).
''');
    write('self.glp', '''
exported procedure go(a#Hidden).
go(h).
''');
    expect(() => _engine().loadProgram(prog.path),
        refused('no declaration'));
  });

  test('a qualifier naming no module is refused', () {
    write('self.glp', '''
exported procedure go(nowhere#Colour).
go(red).
''');
    expect(() => _engine().loadProgram(prog.path),
        refused('nowhere#Colour names no module nowhere'));
  });
}
