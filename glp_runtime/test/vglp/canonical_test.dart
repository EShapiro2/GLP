// glp_runtime/test/vglp/canonical_test.dart
//
// The canonical compilation of a vGLP program in the paper's syntax, in both
// modes.
// Spec: vGLP at db03e2d --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation".  vGLP's code task of 2026-10-01, Part 1, tests (ii) and (iii),
// with their texts following the task of 2026-10-02 00:13 UTC, items B' and F.
//
// (ii) A reader-mode question and a writer-mode one compile to their asking
//     clauses, which send ask(T, X, W?) on the ask stream; a (_) clause keeps
//     the anonymous variable at its interactive position, binds the handle to
//     withdraw and adds no goal: no built-in, neither withdraw/1 nor the
//     root's close/1; and every procedure that reaches a question carries an
//     ask stream, the body's merged into the head's.
// F   The ask streams: which procedures reach a question, the merges, the
//     handle, and the types the compilation adds.
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

/// The .vglp sources under [dir], walked one directory at a time.  Not the
/// fixtures other tests write under programs/ and remove, `<stem>_<pid>_<time>/`,
/// which run beside this one: a recursive listing that meets one as it is
/// removed throws, and failed the load of this file (2026-10-02, beside
/// load_test.dart's vglp_load_fixture_*).  A directory gone before it is
/// listed is skipped for the same reason.
List<File> _vglpSources(Directory dir) {
  final found = <File>[];
  List<FileSystemEntity> entries;
  try {
    entries = dir.listSync(followLinks: false);
  } on FileSystemException {
    return found;
  }
  for (final e in entries) {
    final name = e.path.split(Platform.pathSeparator).last;
    if (e is Directory) {
      if (RegExp(r'_\d+_\d+$').hasMatch(name)) continue;
      found.addAll(_vglpSources(e));
    } else if (e is File && name.endsWith('.vglp')) {
      found.add(e);
    }
  }
  return found;
}

void main() {
  group('(ii) a reader-mode question and a writer-mode one', () {
    late String q;
    setUp(() => q = compileCanonical(_vglp('questions')).source);

    test('a reader-mode question, (Request?), sends its ask with the writer of '
        'its interactive variable, and the asked goal gets the reader', () {
      expect(
          q,
          contains("agent(S1, S2, S3?, [ask('Request?', request_r(X), W?) | "
              'D?]) :- agent1(S1?, S2?, S3, X?, W, D).'));
      expect(
          q,
          contains('procedure agent(Peer?, Stream(Msg)?, Stream(String), '
              'Stream(Ask)).'));
      expect(
          q,
          contains('procedure agent1(Peer?, Stream(Msg)?, Stream(String), '
              'Request?, Handle, Stream(Ask)).'));
      expect(q, contains('agent1(_, _, [], quit, _?, []).'));
      expect(
          q,
          contains('agent1(Id, NetIn, [Text? | Outs?], post(Text), _?, D?) :- '
              'ground(Id?) | agent(Id?, NetIn?, Outs, D).'));
    });

    test('a writer-mode question, (Card), sends its ask with the reader, and '
        'the asked goal gets the writer', () {
      expect(
          q,
          contains("respond_coldcall(S1, S2?, [ask('Card', card_w(X?), W?) | "
              'D?]) :- respond_coldcall1(S1?, S2, X, W, D).'));
      expect(
          q,
          contains('procedure respond_coldcall1(Offer?, Response, Card, '
              'Handle, Stream(Ask)).'));
      expect(
          q,
          contains('respond_coldcall1(offer(From), Resp?, card(From?, Answer), '
              '_?, []) :- ground(From?) | decide(Answer?, From?, Resp).'));
    });

    test('a (_) clause keeps the anonymous variable at its interactive '
        'position, binds the handle to withdraw, adds no goal, and merges its '
        'two calls\' ask streams into its own', () {
      expect(
          q,
          contains('agent1(Id, [msg(Id1, friend_request(From, Resp?)) | NetIn], '
              'Outs?, _, withdraw, D?) :- (Id? =?= Id1?), ground(From?) | '
              'respond_coldcall(offer(From?), Resp, D1), '
              'agent(Id?, NetIn?, Outs, D2), merge(D1?, D2?, D).'));
      expect(q, isNot(contains('close(')));
      expect(q, isNot(contains('withdraw(')));
    });

    test('a procedure that reaches no question is unchanged', () {
      expect(q, contains('procedure decide(YesNo?, Peer?, Response).'));
      expect(q, contains('decide(yes, From, accept(From?)).'));
      expect(q, contains('decide(no, From, refuse(From?)).'));
    });

    test('the types the compilation adds: the handle\'s, the questions, one '
        'functor per moded interactive type wrapping it as written, and one '
        'ask/3 over their union', () {
      expect(q, contains('Handle ::= withdraw.'));
      expect(q,
          contains('Question ::= request_r(Request?) ; card_w(Card).'));
      expect(q, contains('Ask ::= ask(Constant, Question, Handle).'));
    });

    test('no construct goal, and no "true |" in an asking clause', () {
      expect(q, isNot(contains('construct(')));
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
      expect(
          s,
          contains("chat(S1, S2?, [ask('Stream(String)?', "
              'stream_string_r(X), W?) | D?]) :- chat1(S1?, S2, X?, W, D).'));
      expect(
          s,
          contains('procedure chat1(Peer?, Stream(Msg), Stream(String)?, '
              'Handle, Stream(Ask)).'));
      expect(s, contains('chat1(Peer, Out?, Ms, _?, []) :- ground(Peer?) | '
          'send_all(Peer?, Ms?, Out).'));
      expect(s,
          contains('Question ::= stream_string_r(Stream(String)?).'));
      expect(s, contains('Ask ::= ask(Constant, Question, Handle).'));
    });

    test('a (_) unit clause, and a (_) clause whose body is true', () {
      final s = compile('''
T ::= t.
procedure (T?)*p(Integer?).
(_)*p(0).
(_)*p(N) :- N? > 0 | true.
''');
      expect(s, contains('p1(0, _, withdraw, []).'));
      expect(s, contains('p1(N, _, withdraw, []) :- (N? > 0) | true.'));
      expect(s, isNot(contains('withdraw(')));
    });

    test('a (_) clause adds no variable to the clause', () {
      final s = compile('''
T ::= t.
procedure (T?)*p(Integer?, Integer).
(_)*p(A, A?).
''');
      expect(s, contains('p1(A, A?, _, withdraw, []).'));
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
      // The ask type takes the parameter an interactive type names, and every
      // declaration with an ask stream takes it with it.
      expect(s, contains('exported procedure(X) go(Stream(Ask(X))).'));
      expect(s, contains("go([ask('T?', t_r(X), W?) | D?]) :- go1(X?, W, D)."));
      expect(s, contains('procedure(X) go1(T?, Handle, Stream(Ask(X))).'));
      expect(s, contains('go1(t, _?, []).'));
      expect(s, contains('procedure(X) take(X, Stream(Ask(X))).'));
      expect(s,
          contains('procedure(X) take1(X, Box(X)?, Handle, Stream(Ask(X))).'));
      expect(
          s,
          contains("take(S1?, [ask('Box(X)?', box_x_r(X), W?) | D?]) :- "
              'take1(S1, X?, W, D).'));
      expect(s, contains('Question(X) ::= t_r(T?) ; box_x_r(Box(X)?).'));
      expect(s, contains('Ask(X) ::= ask(Constant, Question(X), Handle).'));
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
      expect(
          c.source,
          contains("ask(S1, [ask('T?', t_r(X), W?) | D?]) :- "
              'ask1_1(S1?, X?, W, D).'));
      expect(c.source,
          contains('ask1_1(N, t, _?, []) :- ground(N?) | ask1(N?).'));
      expect(c.source, contains('procedure ask1(Integer?).'));
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
      expect(c.source, contains('p1(_, _?, t, _?, []).'));
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

      test('in reader mode, (_) compiles, dropping the reader and binding the '
          'handle to withdraw', () {
        final s = compile('''
YesNo ::= yes ; no.
procedure (YesNo?)*ask(Integer?, Integer).
(yes)*ask(N, N?).
(_)*ask(_, 0).
''');
        expect(s, contains('ask1(N, N?, yes, _?, []).'));
        expect(s, contains('ask1(_, 0, _, withdraw, []).'));
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

      // _Name is the anonymous variable too: TGLP's "Anonymous variables",
      // any variable whose name begins with _ (vGLP's task of 2026-10-02
      // 00:52 UTC, item 2).
      test('in reader mode, (_Name) compiles as (_) does, dropping the reader '
          'and binding the handle to withdraw', () {
        final s = compile('''
YesNo ::= yes ; no.
procedure (YesNo?)*ask(Integer?, Integer).
(yes)*ask(N, N?).
(_Answer)*ask(_, 0).
''');
        expect(s, contains('ask1(N, N?, yes, _?, []).'));
        expect(s, contains('ask1(_, 0, _Answer, withdraw, []).'));
      });

      test('in writer mode, (_Name) is a compile error naming the clause', () {
        expect(() => compile('''
YesNo ::= yes ; no.
Note ::= note(YesNo).
procedure (Note)*tell(YesNo?).
(note(A?))*tell(A).
(_Note)*tell(no).
'''), refusal('(_Note)*tell(no)', 5));
      });

      test('in writer mode, (_Name?) is refused as well', () {
        expect(() => compile('''
YesNo ::= yes ; no.
Note ::= note(YesNo).
procedure (Note)*tell(YesNo?).
(_Note?)*tell(no).
'''), refusal('(_Note?)*tell(no)', 4));
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

  group('F: the ask streams', () {
    test('a procedure reaches a question if it is volitional or a clause of '
        'it calls one that does, the least fixpoint; one that does not is '
        'unchanged', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*q(Integer?).
(t)*q(_).
procedure r(Integer?).
r(N) :- q(N?).
procedure p(Integer?).
p(N) :- r(N?).
p(0).
procedure s(Integer?).
s(N) :- s1(N?).
procedure s1(Integer?).
s1(_).
''');
      expect(c.reaching, {'q/1', 'r/1', 'p/1'});
      expect(c.source, contains('procedure p(Integer?, Stream(Ask)).'));
      expect(c.source, contains('p(N, D?) :- r(N?, D).'));
      expect(c.source, contains('p(0, []).'));
      expect(c.source, contains('procedure r(Integer?, Stream(Ask)).'));
      expect(c.source, contains('r(N, D?) :- q(N?, D).'));
      expect(c.source, contains('procedure s(Integer?).'));
      expect(c.source, contains('s(N) :- s1(N?).'));
      expect(c.source, contains('s1(_).'));
    });

    test('mutual recursion reaches through the cycle', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*q(Integer?).
(t)*q(_).
procedure a(Integer?).
a(N) :- b(N?).
procedure b(Integer?).
b(0) :- q(0).
b(N) :- N? > 0 | a(N?).
''');
      expect(c.reaching, {'q/1', 'a/1', 'b/1'});
      expect(c.source, contains('b(0, D?) :- q(0, D).'));
      expect(c.source, contains('b(N, D?) :- (N? > 0) | a(N?, D).'));
    });

    test('one call is given D, two are merged into it, three by a chain of '
        'merges, and the names are fresh in the clause', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*q(Integer?).
(t)*q(_).
procedure m(Integer?, Integer?).
m(A, D) :- q(A?), q(D?), q(0).
m(1, B) :- q(B?), q(1).
m(2, B) :- q(B?).
m(0, _).
''');
      expect(
          c.source,
          contains('m(A, D, D1?) :- q(A?, D2), q(D?, D3), q(0, D4), '
              'merge(D2?, D3?, D5), merge(D5?, D4?, D1).'));
      expect(c.source,
          contains('m(1, B, D?) :- q(B?, D1), q(1, D2), merge(D1?, D2?, D).'));
      expect(c.source, contains('m(2, B, D?) :- q(B?, D).'));
      expect(c.source, contains('m(0, _, []).'));
    });

    test('a remote call calls no procedure of the program and is not given '
        'an ask stream, though it names one that reaches a question', () {
      final c = compileCanonical('''
T ::= t.
procedure (T?)*q(Integer?).
(t)*q(_).
procedure p(Integer?).
p(N) :- lib # q(N?).
''');
      expect(c.reaching, {'q/1'});
      expect(c.source, contains('p(N) :- lib # q(N?).'));
    });

    test('an interactive type shared by two procedures is one alternative of '
        'the questions\' union', () {
      final s = compileCanonical('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(_).
procedure (T?)*q(Integer?).
(t)*q(_).
''').source;
      expect(s, contains('Question ::= t_r(T?).'));
      expect(s, contains('Ask ::= ask(Constant, Question, Handle).'));
    });

    test('the types it adds are named fresh against the program\'s', () {
      final c = compileCanonical('''
Ask ::= a.
Handle ::= h.
Question ::= q.
procedure (Ask?)*p(Handle?).
(a)*p(_).
''');
      expect(c.askType, 'Ask_1');
      expect(c.handleType, 'Handle_1');
      expect(c.questionType, 'Question_1');
      expect(c.source, contains('Handle_1 ::= withdraw.'));
      expect(c.source, contains('Question_1 ::= ask_r(Ask?).'));
      expect(c.source,
          contains('Ask_1 ::= ask(Constant, Question_1, Handle_1).'));
      expect(c.source,
          contains('procedure p1(Handle?, Ask?, Handle_1, Stream(Ask_1)).'));
    });

    test('an exported procedure that reaches a question keeps exported, and its '
        'ask stream is the last argument', () {
      final s = compileCanonical('''
T ::= t.
procedure (T?)*q(Integer?).
(t)*q(_).
exported procedure go(Integer?).
go(N) :- q(N?).
''').source;
      expect(s, contains('exported procedure go(Integer?, Stream(Ask)).'));
      expect(s, contains('go(N, D?) :- q(N?, D).'));
    });
  });

  group('(iii) the old syntax keeps its old compilation', () {
    final sources = _vglpSources(Directory(_programs));
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
