// glp_runtime/test/vglp/load_test.dart
//
// Loading a directory that holds .vglp sources: the loader compiles them and
// they join the program as modules.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".
//
// The fixture is written under programs/ and removed again, because a program
// must live there: ancestor self.glp collection walks up only as far as the
// programs directory, so the same directory elsewhere gets no ancestor scope.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/vglp/program_compilation.dart' show compiledHeader;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

const _programs = '../programs';
final _rootSelfGlp = '$_programs/self.glp';

void main() {
  if (File(_rootSelfGlp).existsSync()) {
    setRootScopeEnvironmentSource(File(_rootSelfGlp).readAsStringSync());
  }

  late Directory fixture;

  setUp(() {
    fixture = Directory('$_programs/vglp_load_fixture_${pid}_'
        '${DateTime.now().microsecondsSinceEpoch}');
    fixture.createSync();
  });

  tearDown(() {
    if (fixture.existsSync()) fixture.deleteSync(recursive: true);
  });

  void write(String name, String content) =>
      File('${fixture.path}/$name').writeAsStringSync(content);

  const selfGlp = '''
Decision      ::= yes ; no.
Offer         ::= offer(Constant).
Answered      ::= answered(Decision, Constant).
AnswerStream  ::= [] ; [Answered | AnswerStream].

imported procedure responder#respond(Offer?, AnswerStream).
exported procedure respond(Offer?, AnswerStream).
respond(O, As?) :- responder # respond(O?, As).
''';

  const responderVglp = '''
procedure respond(Offer?, AnswerStream).

*(Answer=yes, From?)
respond(offer(From), [answered(Answer?, From?)]) :-
    ground(From?) | true
*(no) true.

*(Answer=no, From?)
respond(offer(From), [answered(Answer?, From?)]) :-
    ground(From?) | true.
''';

  group('a .vglp source with no .glp of its name', () {
    test('it is compiled and joins the program', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);

      final modules =
          discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp);
      final responder =
          modules.where((m) => m.moduleName == 'responder').toList();
      expect(responder, hasLength(1),
          reason: 'the .vglp source should be a module of the program');
      expect(responder.single.filePath, endsWith('.vglp'));
    });

    test('what joins is the COMPILED module, not the source', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);

      final m = discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp)
          .firstWhere((m) => m.moduleName == 'responder');

      // No volition guard survives: GLP is vGLP without volition-guarded
      // clauses (Definition "GLP, maGLP, cGLP").
      for (final p in m.ast.procedures) {
        for (final c in p.clauses) {
          expect(c.isVolitionGuarded, isFalse);
        }
      }
      // The mediator came with it, and the compiled respond gained the channel
      // and its two slots.
      expect(m.ast.procedures.map((p) => p.name), contains('med'));
      final respond =
          m.ast.procedures.firstWhere((p) => p.name == 'respond');
      expect(respond.arity, 2 + 1 + 2);
    });

    test('it gets the exposed scope its sibling modules get', () {
      // The root self.glp exposes social/graph/routing, and every module under
      // programs/ resolves send_net and the rest of it unqualified.  The
      // compiled module joins the list after the exposes are resolved, so it
      // is given the same merge; without it a .vglp source calling send_net
      // compiled to a module in which send_net was undefined.
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);

      final modules =
          discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp);
      final compiled = modules.firstWhere((m) => m.moduleName == 'responder');
      final sibling = modules.firstWhere((m) => m.isSelfGlp);
      bool declares(DiscoveredModule m, String sig) =>
          m.ancestorScope.procedures.containsKey(sig) ||
          m.ancestorScope.paramProcDecls.containsKey(sig);
      for (final sig in ['send_net/3', 'send_user/3', 'send_friend/4']) {
        expect(declares(sibling, sig), isTrue,
            reason: '$sig should reach the self.glp module');
        expect(declares(compiled, sig), isTrue,
            reason: '$sig should reach the compiled module too');
      }
    });
  });

  group('a .vglp source beside a .glp of its name', () {
    test('the hand-written module stands and the .vglp is not compiled', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);
      write('responder.glp', '''
procedure respond(Offer?, AnswerStream).
respond(offer(From), [answered(no, From?)]) :- ground(From?) | true.
''');

      final modules =
          discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp);
      final responder =
          modules.where((m) => m.moduleName == 'responder').toList();
      expect(responder, hasLength(1));
      expect(responder.single.filePath, endsWith('.glp'),
          reason: 'switching a deployed program onto its compiled agent is its '
              'own change, not a side effect of loading it');
    });
  });

  group('the emit flag', () {
    test('it writes the compiled GLP beside the source', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);

      final written = emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_rootSelfGlp).absolute.path);
      expect(written, hasLength(1));
      expect(written.single, endsWith('responder.glp'));

      final emitted = File(written.single).readAsStringSync();
      expect(emitted, startsWith(compiledHeader));
      expect(emitted, contains('procedure med('));
      expect(emitted, isNot(contains('*(')));
    });

    test('it re-emits its own output', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);
      final first = emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_rootSelfGlp).absolute.path);
      final second = emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_rootSelfGlp).absolute.path);
      expect(second, equals(first));
    });

    test('it compiles in the scope the loader gives, exposes included', () {
      // The root self.glp exposes social/graph/routing; a .vglp source calls
      // send_net and types its answer writer by the message's shape at the
      // router's M position.  :emit must see the same scope as the load, or
      // it fails on a source the load compiles.
      write('self.glp', '''
Note      ::= msg(Constant, Constant).
Ent       ::= net_output(Stream(Note)).
Decision  ::= yes ; no.
Offer     ::= offer(Constant).
imported procedure greeter#greet(Offer?, Stream(Ent)?, Stream(Ent)).
exported procedure greet(Offer?, Stream(Ent)?, Stream(Ent)).
greet(O, Outs, Outs1?) :- greeter # greet(O?, Outs?, Outs1).
''');
      write('greeter.vglp', '''
procedure greet(Offer?, Stream(Ent)?, Stream(Ent)).
*(Target)
greet(offer(From), Outs, Outs1?) :-
    ground(From?) |
    send_net(msg(Target?, From?), Outs?, Outs1).
''');
      final written = emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_rootSelfGlp).absolute.path);
      expect(written, hasLength(1));
      final emitted = File(written.single).readAsStringSync();
      expect(emitted, contains('Xs_greet_1 ::= xs_greet_1(Constant).'));
      // And the load compiles the same source.
      final m = discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp)
          .firstWhere((m) => m.moduleName == 'greeter');
      expect(m.ast.procedures.map((p) => p.name), contains('greet'));
    });

    test('it never overwrites a hand-written module', () {
      write('self.glp', selfGlp);
      write('responder.vglp', responderVglp);
      const handWritten = '''
procedure respond(Offer?, AnswerStream).
respond(offer(From), [answered(no, From?)]) :- ground(From?) | true.
''';
      write('responder.glp', handWritten);

      final skipped = <String>[];
      final written = emitVglpSources(fixture.path,
          rootSelfGlpPath: File(_rootSelfGlp).absolute.path,
          onSkip: skipped.add);
      expect(written, isEmpty);
      expect(skipped, hasLength(1));
      expect(File('${fixture.path}/responder.glp').readAsStringSync(),
          handWritten);
    });
  });

  group('a directory with no .vglp at all', () {
    test('it loads exactly as before', () {
      write('self.glp', '''
exported procedure ping(Constant).
ping(a).
''');
      final modules =
          discoverProgram(fixture.path, rootSelfGlpPath: _rootSelfGlp);
      expect(modules.any((m) => m.filePath.endsWith('.vglp')), isFalse);
      expect(modules.any((m) => m.moduleName.isNotEmpty), isTrue);
    });
  });
}
