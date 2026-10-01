// glp_runtime/test/vglp/canonical_test.dart
//
// The canonical compilation of a vGLP program in the paper's syntax, in both
// modes, with person(T, X) in a program that declares a population.
// Spec: vGLP at 16b3b54 --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation" --- and sGLP, sections/simulation.tex, Definition "Simulation
// Program", with sections/implementation.tex.  vGLP's code task of
// 2026-10-01, Part 1, covering tests (i)--(iii).
//
// (i) The .vglp sources of Integration's hand-compiled fixtures,
//     programs/tests/vglp/person_asks*/, compile to the fixtures,
//     programs/tests/sglp/person_asks*.glp, up to the names of the asking
//     clause and of the (n+1)-ary procedure, and the compiled programs run as
//     the fixtures do, log line for log line.
// (ii) Outside a population a reader-mode question and a writer-mode one
//     compile to construct(T, X), and a (_) clause emits close.
// (iii) The nine .vglp sources in the old syntax are not in the paper's, and
//     keep their old compilation.
//
// Fixtures that must be programs are written under programs/ and removed
// again, as load_test.dart's are: the ancestor scope chain ends there.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/mediator.dart' show printTypeDef;
import 'package:glp_runtime/vglp/program_compilation.dart'
    show compiledHeader, compileVglpSource;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart'
    show ProcDecl;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

const _programs = '../programs';
final _root = File('$_programs/self.glp').absolute.path;

String _vglp(String name) =>
    File('$_programs/tests/vglp/$name/$name.vglp').readAsStringSync();
String _fixture(String name) =>
    File('$_programs/tests/sglp/$name.glp').absolute.path;

// ---------------------------------------------------------------------------
// A module up to the names of its procedures and of its clauses' variables
// ---------------------------------------------------------------------------

/// The normal form of [m], its procedures renamed by [names] and each
/// clause's variables named by first occurrence: what two modules that are
/// equal up to those names share.
Map<String, Object> _normalForm(Module m, Map<String, String> names) {
  String n(String s) => names[s] ?? s;
  String sig(String s) {
    final slash = s.lastIndexOf('/');
    return '${n(s.substring(0, slash))}${s.substring(slash)}';
  }

  ProcDecl decl(ProcDecl d) => ProcDecl(n(d.name), d.argTypes, d.line, d.column,
      typeParams: d.typeParams, exported: d.exported, imported: d.imported,
      modulePath: d.modulePath);

  final printer = SourcePrinter();
  return {
    'types': [for (final t in m.typeDefs) printTypeDef(t)],
    'declarations': {
      for (final d in m.procDeclarations) sig(d.key): printDeclaration(decl(d))
    },
    'procedures': {
      for (final p in m.procedures)
        sig(p.signature): [
          for (final c in p.clauses) printer.printClause(_renamed(c, n))
        ]
    },
    'kinds': [
      for (final k in m.kinds)
        [
          k.name,
          [for (final d in k.personDecls) '${d.typeKey} =::= ${d.procedure}'],
          (k.procedureSigs.map(sig).toList()..sort()),
        ]
    ],
    'run': m.runDecl == null
        ? 'none'
        : '${m.runDecl!.agents} ${m.runDecl!.untilSeconds} ${m.runDecl!.seed} '
            '${m.runDecl!.mixes.map((x) => '${x.dimension}~${x.entries.map((e) => '${e.kind}:${e.probability}').join(';')}').join(',')}',
  };
}

Clause _renamed(Clause c, String Function(String) n) {
  final vars = <String, String>{};
  Term term(Term t) {
    if (t is VarTerm) {
      return VarTerm(vars.putIfAbsent(t.name, () => 'V${vars.length + 1}'),
          t.isReader, t.line, t.column);
    }
    if (t is StructTerm) {
      return StructTerm(t.functor, t.args.map(term).toList(), t.line, t.column);
    }
    if (t is ListTerm) {
      if (t.isNil) return t;
      return ListTerm(t.head == null ? null : term(t.head!),
          t.tail == null ? null : term(t.tail!), t.line, t.column);
    }
    return t;
  }

  Goal goal(Goal g) {
    if (g is RatedGoal) return g.withInner(goal(g.innerGoal));
    return Goal(n(g.functor), g.args.map(term).toList(), g.line, g.column);
  }

  final head = Atom(n(c.head.functor), c.head.args.map(term).toList(),
      c.head.line, c.head.column);
  final guards = [
    for (final g in c.guards ?? const <Guard>[])
      Guard(g.predicate, g.args.map(term).toList(), g.line, g.column,
          negated: g.negated)
  ];
  final body = c.body?.map(goal).toList();
  return Clause(head, guards: guards, body: body, line: c.line, column: c.column);
}

Module _parse(String text) => Parser(Lexer(text).tokenize()).parseModule();

/// The names the fixtures give: the asking clause the source name, and the
/// (n+1)-ary procedure the source name with 1.
Map<String, String> _fixtureNames(CanonicalProgram c) =>
    {for (final v in c.volitional) v.guardedName: '${v.name}1'};

// ---------------------------------------------------------------------------
// Runs
// ---------------------------------------------------------------------------

/// A run's outcome as text: its status, its error, its bindings and its log.
Future<List<String>> _run(String path, String goal, List<int?> agents) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)..loadFile(path);
  final lines = <String>[];
  engine.onSimulationLog = lines.add;
  final r = await engine.runGoal(goal, agents: agents);
  return [
    'status ${r.status}',
    'error ${r.error}',
    for (final e in r.bindings.entries) 'binding ${e.key} = ${e.value}',
    ...lines,
  ];
}

const _runs = <String, (String, List<int?>)>{
  'person_asks': ('agent(1, 2, A), agent(2, 2, B), agent(3, 2, C)', [1, 2, 3]),
  'person_asks_writer': (
    'start(a, [c], [b, d], [msg(b, offer(b))], O1), '
        'start(b, [d], [a, c], [], O2)',
    [1, 2]
  ),
  'person_asks_coins': (
    'start(a, [b], O2?, O1), start(b, [a], O1?, O2)',
    [1, 2]
  ),
};

void main() {
  if (File(_root).existsSync()) {
    setRootScopeEnvironmentSource(File(_root).readAsStringSync());
  }

  final temporary = <Directory>[];
  Directory scratch(String stem) {
    final d = Directory('$_programs/${stem}_${pid}_'
        '${DateTime.now().microsecondsSinceEpoch}')
      ..createSync();
    temporary.add(d);
    return d;
  }

  tearDown(() {
    for (final d in temporary) {
      if (d.existsSync()) d.deleteSync(recursive: true);
    }
    temporary.clear();
  });

  group('(i) the fixtures\' sources compile to the fixtures', () {
    for (final name in _runs.keys) {
      test('$name.vglp compiles to $name.glp, up to the names of 2', () {
        final compiled = compileCanonical(_vglp(name));
        expect(compiled.population, isTrue,
            reason: 'the source declares a population');
        expect(_normalForm(compiled.module, _fixtureNames(compiled)),
            _normalForm(_parse(File(_fixture(name)).readAsStringSync()), {}));
      });

      test('$name: the compiled program, emitted by :emit, runs as the '
          'fixture does, log line for log line', () async {
        final dir = scratch('vglp_canonical_run');
        File('${dir.path}/$name.vglp').writeAsStringSync(_vglp(name));
        final written = emitVglpSources(dir.path, rootSelfGlpPath: _root);
        expect(written, hasLength(1));
        final emitted = File(written.single).readAsStringSync();
        expect(emitted, compileCanonical(_vglp(name)).source);

        final (goal, agents) = _runs[name]!;
        final fixture = await _run(_fixture(name), goal, agents);
        expect(fixture.first, isNot('status ${ExecutionStatus.failed}'));
        expect(fixture.length, greaterThan(3), reason: 'the run logs');
        expect(await _run(written.single, goal, agents), fixture);
      });
    }

    test('the asking clause, by mode: reader mode passes person/2 the writer '
        'and the clauses the reader; writer mode the other way about', () {
      final reader = compileCanonical(_vglp('person_asks')).source;
      expect(reader,
          contains("ask(S1, S2?) :- person('Pick?', X), ask1(S1?, S2, X?)."));
      expect(reader, contains('procedure ask(Integer?, Choice).'));
      expect(reader, contains('procedure ask1(Integer?, Choice, Pick?).'));

      final writer = compileCanonical(_vglp('person_asks_writer')).source;
      expect(writer, contains("ask(S1, S2, S3?) :- person('Menu', X?), "
          'ask1(S1?, S2?, S3, X).'));
      expect(writer, contains("respond(S1, S2?) :- person('Card', X?), "
          'respond1(S1?, S2, X).'));
      expect(writer, contains('procedure respond1(Peer?, YesNo, Card).'));
    });
  });

  group('(ii) outside a population', () {
    late String q;
    setUp(() => q = compileCanonical(_vglp('questions')).source);

    test('a reader-mode question, (Request?), compiles to construct', () {
      expect(
          q,
          contains("agent(S1, S2, S3?) :- construct('Request?', X), "
              'agent1(S1?, S2?, S3, X?).'));
      expect(q, contains('procedure agent(Peer?, Stream(Msg)?, Stream(String)).'));
      expect(q,
          contains('procedure agent1(Peer?, Stream(Msg)?, Stream(String), Request?).'));
      expect(q, contains('agent1(_, _, [], quit).'));
    });

    test('a writer-mode question, (Card), compiles to construct', () {
      expect(
          q,
          contains("respond_coldcall(S1, S2?) :- construct('Card', X?), "
              'respond_coldcall1(S1?, S2, X).'));
      expect(q, contains('procedure respond_coldcall1(Offer?, Response, Card).'));
      expect(
          q,
          contains('respond_coldcall1(offer(From), Resp?, card(From?, Answer)) '
              ':- ground(From?) | decide(Answer?, From?, Resp).'));
    });

    test('a (_) clause is given a fresh writer and the body goal close of '
        'its reader', () {
      expect(
          q,
          contains('agent1(Id, [msg(Id1, friend_request(From, Resp?)) | NetIn], '
              'Outs?, A) :- (Id? =?= Id1?), ground(From?) | '
              'respond_coldcall(offer(From?), Resp), agent(Id?, NetIn?, Outs), '
              'close(A?).'));
    });

    test('no person/2, and no "true |" in an asking clause', () {
      expect(q, isNot(contains('person(')));
      expect(q, isNot(contains(':- true |')));
      expect(q, startsWith(compiledHeader));
    });

    test('the same source in a program that declares a population elsewhere '
        'calls person/2', () {
      final p = compileCanonical(_vglp('questions'), population: true).source;
      expect(p, contains("person('Request?', X)"));
      expect(p, contains("person('Card', X?)"));
      expect(p, isNot(contains('construct(')));
    });
  });

  group('the paper\'s syntax', () {
    String compile(String s) => compileCanonical(s).source;

    test('the interactive type is named as written, its arguments and mode '
        'included', () {
      final s = compile('''
Peer ::= Constant.
Msg ::= msg(Peer, String).
procedure (Stream(String)?)*chat(Peer?, Stream(Msg)).
(Ms)*chat(Peer, Out?) :- ground(Peer?) |
    send_all(Peer?, Ms?, Out).

procedure send_all(Peer?, Stream(String)?, Stream(Msg)).
send_all(Peer, [M|Ms], [msg(Peer?, M?)|Out?]) :-
    ground(Peer?) | send_all(Peer?, Ms?, Out).
send_all(_, [], []).
''');
      expect(s, contains("chat(S1, S2?) :- construct('Stream(String)?', X), "
          'chat1(S1?, S2, X?).'));
      expect(s,
          contains('procedure chat1(Peer?, Stream(Msg), Stream(String)?).'));
      expect(s, contains('chat1(Peer, Out?, Ms) :- ground(Peer?) | '
          'send_all(Peer?, Ms?, Out).'));
    });

    test('a (_) unit clause, and a (_) clause whose body is true', () {
      final s = compile('''
T ::= t.
procedure (T?)*p(Integer?).
(_)*p(0).
(_)*p(N) :- N? > 0 | true.
''');
      expect(s, contains('p1(0, A) :- close(A?).'));
      expect(s, contains('p1(N, A) :- (N? > 0) | close(A?).'));
    });

    test('the fresh writer of a (_) clause is fresh in the clause', () {
      final s = compile('''
T ::= t.
procedure (T?)*p(Integer?, Integer).
(_)*p(A, A?).
''');
      expect(s, contains('p1(A, A?, A1) :- close(A1?).'));
    });

    test('a nullary volitional procedure, and exported, and a parameter '
        'list', () {
      final s = compile('''
T ::= t.
Box(X) ::= box(X).
exported procedure (T?)*go.
(t)*go.
procedure(X) (Box(X)?)*take(X).
(box(V))*take(V?).
''');
      expect(s, contains('exported procedure go().'));
      expect(s, contains("go :- construct('T?', X), go1(X?)."));
      expect(s, contains('procedure go1(T?).'));
      expect(s, contains('go1(t).'));
      expect(s, contains('procedure(X) take(X).'));
      expect(s, contains('procedure(X) take1(X, Box(X)?).'));
      expect(s, contains("take(S1?) :- construct('Box(X)?', X), take1(S1, X?)."));
    });

    test('the (n+1)-ary procedure\'s name is fresh against the program\'s', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*ask(Integer?).
(t)*ask(N) :- ground(N?) | ask1(N?).
procedure ask1(Integer?).
ask1(_).
''');
      expect(c.volitional.single.guardedName, 'ask1_1');
      expect(c.source, contains("ask(S1) :- construct('T?', X), ask1_1(S1?, X?)."));
      expect(c.source, contains('ask1_1(N, t) :- ground(N?) | ask1(N?).'));
    });

    group('is refused', () {
      void refused(String source, String why) => expect(
          () => compileCanonical(source),
          throwsA(isA<CompileError>()
              .having((e) => e.message, 'message', contains(why))));

      test('mixed with the old syntax', () {
        refused('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(_).
procedure q(Integer?).
*(X) q(X) :- true | true.
''', 'not both');
      });

      test('a clause (A)*p of no procedure declared with an interactive type',
          () {
        refused('''
T ::= t.
procedure p(Integer?, T?).
(t)*p(_).
''', 'of no procedure declared');
      });

      test('a clause of a volitional procedure written without its '
          'interactive term', () {
        refused('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(0).
p(1, t).
''', 'with its interactive term');
      });

      test('a volitional procedure also declared without its interactive type',
          () {
        refused('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(_).
procedure p(Integer?).
p(_).
''', 'also declared or defined without one');
      });

      test('a call of a volitional procedure with n+1 arguments', () {
        refused('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(_).
procedure q(T?).
q(X) :- p(0, X?).
''', 'until it is asked');
      });
    });
  });

  group('(iii) the old syntax keeps its old compilation', () {
    final sources = Directory(_programs)
        .listSync(recursive: true)
        .whereType<File>()
        .where((f) => f.path.endsWith('.vglp'))
        // Not the fixtures another test writes under programs/ and removes,
        // <stem>_<pid>_<time>/, which run beside this one.
        .where((f) => !RegExp(r'_\d+_\d+[/\\]').hasMatch(f.path))
        .toList();
    final paper = {
      'person_asks.vglp',
      'person_asks_writer.vglp',
      'person_asks_coins.vglp',
      'questions.vglp',
      'graph.vglp'
    };
    String base(File f) => f.path.split(Platform.pathSeparator).last;

    test('the nine old sources are not in the paper\'s syntax, the five new '
        'ones are', () {
      final old = sources.where((f) => !paper.contains(base(f))).toList();
      expect(old, hasLength(9), reason: old.map((f) => f.path).join('\n'));
      for (final f in old) {
        expect(isPaperSyntaxSource(f.readAsStringSync()), isFalse,
            reason: f.path);
      }
      final fresh = sources.where((f) => paper.contains(base(f))).toList();
      expect(fresh, hasLength(5));
      for (final f in fresh) {
        expect(isPaperSyntaxSource(f.readAsStringSync()), isTrue,
            reason: f.path);
      }
    });

    test('an old source with no mediator to compile against is refused, not '
        'compiled by the new compilation', () {
      final f = sources.firstWhere((f) => base(f) == 'responder.vglp');
      expect(() => compileVglpSource(f.readAsStringSync()),
          throwsA(isA<StateError>()));
    });
  });

  group('the loader', () {
    test('a directory program whose .vglp declares a population loads with '
        'person/2 in scope, and its question is asked of the person', () async {
      final dir = scratch('vglp_canonical_population');
      File('${dir.path}/self.glp').writeAsStringSync('''
Choice ::= left ; right.
Ack    ::= ok(Integer).
Pick   ::= pick(Choice, Ack?).

imported procedure picker#agent(Integer?, Stream(Choice)).
exported procedure agent(Integer?, Stream(Choice)).
agent(N, Cs?) :- picker # agent(N?, Cs).
''');
      File('${dir.path}/picker.vglp').writeAsStringSync('''
procedure (Pick?)*ask(Integer?, Choice).
(pick(C, ok(N?)))*ask(N, C?).

exported procedure agent(Integer?, Stream(Choice)).
agent(N, [C?]) :- ground(N?) | ask(N?, C).

procedure drop(Ack?).
drop(_).

person lefty.
Pick? =::= lefty_pick.
procedure lefty_pick(Pick, Integer?).
lefty_pick(pick(left, A), _) :- drop(A?).

run 1 agents [ hand ~ (lefty : 1.0) ] until 1 day seed 1.
''');
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(dir.path), isTrue);
      final lines = <String>[];
      engine.onSimulationLog = lines.add;
      final r = await engine.runGoal('agent(1, Cs)', agents: [1]);
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final heap = engine.runtime.heap;
      final cs = heap.dereference(r.bindings['Cs'] as rt.Term);
      expect(cs, isA<rt.StructTerm>());
      final first = heap.dereference((cs as rt.StructTerm).args[0]);
      expect((first as rt.ConstTerm).value, 'left');
      expect(lines.join('\n'), contains('pick(left,'));
      expect(lines.join('\n'), contains('ok(1)'));
    });
  });
}
