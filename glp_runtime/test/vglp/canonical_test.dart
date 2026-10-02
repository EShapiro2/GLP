// glp_runtime/test/vglp/canonical_test.dart
//
// The canonical compilation of a vGLP program in the paper's syntax, in both
// modes.
// Spec: vGLP at db03e2d --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation".  vGLP's code task of 2026-10-01, Part 1, tests (ii) and (iii).
//
// (ii) A reader-mode question and a writer-mode one compile to
//     construct(T, X), and a (_) clause keeps the anonymous variable at its
//     interactive position and adds no goal: no built-in, neither withdraw
//     nor the root's close/1 (vGLP's code task of 2026-10-02 00:13 UTC, B').
// (iii) The nine .vglp sources in the old syntax are not in the paper's, and
//     keep their old compilation.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart' show UnderscoreTerm;
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/program_compilation.dart'
    show compiledHeader, compileVglpSource;

const _programs = '../programs';

String _vglp(String name) =>
    File('$_programs/tests/vglp/$name/$name.vglp').readAsStringSync();

void main() {
  group('(ii) a reader-mode question and a writer-mode one', () {
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

    test('a (_) clause keeps the anonymous variable at its interactive '
        'position and adds no goal: neither withdraw nor the root\'s close/1 '
        'is called', () {
      expect(
          q,
          contains('agent1(Id, [msg(Id1, friend_request(From, Resp?)) | NetIn], '
              'Outs?, _) :- (Id? =?= Id1?), ground(From?) | '
              'respond_coldcall(offer(From?), Resp), agent(Id?, NetIn?, Outs).'));
      expect(q, isNot(contains('close(')));
      expect(q, isNot(contains('withdraw')));
    });

    test('no "true |" in an asking clause', () {
      expect(q, isNot(contains(':- true |')));
      expect(q, startsWith(compiledHeader));
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
      expect(s, contains('p1(0, _).'));
      expect(s, contains('p1(N, _) :- (N? > 0) | true.'));
      expect(s, isNot(contains('withdraw')));
    });

    test('a (_) clause adds no variable to the clause', () {
      final s = compile('''
T ::= t.
procedure (T?)*p(Integer?, Integer).
(_)*p(A, A?).
''');
      expect(s, contains('p1(A, A?, _).'));
      expect(s, isNot(contains('A1')));
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

    test('_? at a produced head position is emitted _?, TGLP\'s anonymous '
        'output, in a clause of a volitional procedure and in an ordinary '
        'one', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*p(Integer?, Integer).
(t)*p(_, _?).
procedure q(Integer?, Integer).
q(_, _?).
''');
      expect(c.source, contains('p1(_, _?, t).'));
      expect(c.source, contains('q(_, _?).'));
      for (final name in ['p1', 'q']) {
        final head = c.module.procedures
            .firstWhere((p) => p.name == name)
            .clauses
            .single
            .head;
        expect(head.args[0],
            isA<UnderscoreTerm>().having((u) => u.isReader, 'isReader', isFalse),
            reason: name);
        expect(head.args[1],
            isA<UnderscoreTerm>().having((u) => u.isReader, 'isReader', isTrue),
            reason: name);
      }
    });

    group('the anonymous variable as the interactive term, by the mode of '
        'the interactive type', () {
      Matcher refusal(String clause, int line) => throwsA(isA<CompileError>()
          .having((e) => e.message, 'message',
              allOf(contains('The clause $clause '), contains('writer mode')))
          .having((e) => e.line, 'line', line));

      test('in reader mode, (_) compiles, dropping the reader', () {
        final s = compile('''
YesNo ::= yes ; no.
procedure (YesNo?)*ask(Integer?, Integer).
(yes)*ask(N, N?).
(_)*ask(_, 0).
''');
        expect(s, contains('ask1(N, N?, yes).'));
        expect(s, contains('ask1(_, 0, _).'));
      });

      test('in writer mode, (_) is a compile error naming the clause', () {
        expect(() => compile('''
YesNo ::= yes ; no.
Note ::= note(YesNo).
procedure (Note)*tell(YesNo?).
(note(A?))*tell(A).
(_)*tell(no).
'''), refusal('(_)*tell(no)', 5));
      });

      test('in writer mode, (_?), the anonymous output, is refused as well', () {
        expect(() => compile('''
YesNo ::= yes ; no.
Note ::= note(YesNo).
procedure (Note)*tell(YesNo?).
(_?)*tell(no).
'''), refusal('(_?)*tell(no)', 4));
      });

      test('in writer mode, a nullary procedure\'s (_) clause is named too',
          () {
        expect(() => compile('''
Note ::= note.
procedure (Note)*tell.
(note)*tell.
(_)*tell :- true | true.
'''), refusal('(_)*tell', 4));
      });
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
    final paper = {'questions.vglp'};
    String base(File f) => f.path.split(Platform.pathSeparator).last;

    test('the nine old sources are not in the paper\'s syntax, the one new '
        'one is', () {
      final old = sources.where((f) => !paper.contains(base(f))).toList();
      expect(old, hasLength(9), reason: old.map((f) => f.path).join('\n'));
      for (final f in old) {
        expect(isPaperSyntaxSource(f.readAsStringSync()), isFalse,
            reason: f.path);
      }
      final fresh = sources.where((f) => paper.contains(base(f))).toList();
      expect(fresh, hasLength(1));
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
}
