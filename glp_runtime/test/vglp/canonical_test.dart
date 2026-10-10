// glp_runtime/test/vglp/canonical_test.dart
//
// The canonical compilation of a vGLP program in the paper's syntax, in both
// modes.
// Spec: vGLP at c994328 --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation".  vGLP's code task of 2026-10-01, Part 1, tests (ii) and (iii),
// with their texts following the task of 2026-10-02 00:13 UTC, items B' and F,
// and vGLP #5 Cowork's of 2026-10-03 08:16 UTC, item 1: (_) and the handle
// are out of the language (Udi, 2026-10-03, as vGLP reports it).
//
// (ii) A reader-mode question and a writer-mode one compile to their asking
//     clauses, which send ask(T, t(X)) on the ask stream, and to their
//     clauses with the ask stream and nothing else added; every procedure
//     that reaches a question carries an ask stream, the body's merged into
//     the head's.  The anonymous variable as the interactive term is refused
//     in either mode.
// F   The ask streams: which procedures reach a question, the merges, and the
//     types the compilation adds.
// (iii) Each .vglp source in the tree, asked what it is: one in the old
//     syntax is not in the paper's, and keeps its old compilation; one
//     declaring a volitional procedure is in the paper's.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart' show UnderscoreTerm;
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/program_compilation.dart'
    show compiledHeader, compileVglpSource;

const _programs = '../programs';

String _vglp(String dir, String name) =>
    File('$_programs/tests/vglp/$dir/$name.vglp').readAsStringSync();

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

/// Whether [text] holds a volition guard, the old Definition's ("Guarded
/// Clause, Volition-Guarded Clause, Volition Guard, ..."): an item --- a
/// declaration, a definition or a clause --- beginning with `*`, bare or
/// `*(...)`, before the clause it guards.  Read off the tokens, since the
/// parser does not read every old source (CSSN's two, with guard negation,
/// which is not GLP syntax).
bool _holdsVolitionGuard(String text) {
  final tokens = Lexer(text).tokenize();
  var depth = 0;
  var start = true;
  for (final t in tokens) {
    if (start && t.type == TokenType.STAR) return true;
    start = false;
    switch (t.type) {
      case TokenType.LPAREN:
      case TokenType.LBRACKET:
        depth++;
      case TokenType.RPAREN:
      case TokenType.RBRACKET:
        depth--;
      case TokenType.DOT:
        if (depth == 0) start = true;
      default:
        break;
    }
  }
  return false;
}

/// Whether [text], its widget declarations set aside, declares a volitional
/// procedure, `procedure (T)*p(...)` (vGLP, Definition "Guarded Clause,
/// Volitional Procedure, ..."), as the parser's vGLP mode reads it.
bool _declaresVolitionalProcedure(String text) {
  final stripped =
      text.contains('=::=') ? extractWidgetDeclarations(text).stripped : text;
  return Parser(Lexer(stripped).tokenize(), vglp: true)
      .parseModule()
      .volitionalDeclarations
      .isNotEmpty;
}

void main() {
  group('(ii) a reader-mode question and a writer-mode one', () {
    late String q;
    setUp(() => q = compileCanonical(_vglp('fragments', 'questions')).source);

    test('a reader-mode question, (Stream(Request)?), sends its ask with the '
        'writer of its interactive variable, and the asked goal gets the '
        'reader', () {
      expect(
          q,
          contains("agent(S1, S2, S3?, [ask('Stream(Request)?', "
              'stream_request_r(X)) | D?]) :- agent1(S1?, S2?, S3, X?, D).'));
      expect(
          q,
          contains('exported procedure agent(Peer?, Stream(Msg)?, '
              'Stream(String), Stream(Ask(Question))).'));
      expect(
          q,
          contains('procedure agent1(Peer?, Stream(Msg)?, Stream(String), '
              'Stream(Request)?, Stream(Ask(Question))).'));
      expect(
          q,
          contains('agent1(Id, NetIn, Outs?, Reqs, D?) :- '
              'serve(Reqs?, Id?, NetIn?, Outs, D).'));
    });

    test('a writer-mode question, (Card), sends its ask with the reader, and '
        'the asked goal gets the writer', () {
      expect(
          q,
          contains("respond_coldcall(S1, S2?, [ask('Card', card_w(X?)) | "
              'D?]) :- respond_coldcall1(S1?, S2, X, D).'));
      expect(
          q,
          contains('procedure respond_coldcall1(Offer?, Response, Card, '
              'Stream(Ask(Question))).'));
      expect(
          q,
          contains('respond_coldcall1(offer(From), Resp?, card(From?, Answer), '
              '[]) :- ground(From?) | decide(Answer?, From?, Resp).'));
    });

    test('serve, ordinary GLP, reaches a question: its message clause, which '
        'reads no request, merges its two calls\' ask streams into its own, '
        'and a clause that calls none carries []', () {
      expect(
          q,
          contains('procedure serve(Stream(Request)?, Peer?, Stream(Msg)?, '
              'Stream(String), Stream(Ask(Question))).'));
      expect(
          q,
          contains('serve(Reqs, Id, [msg(Id1, friend_request(From, Resp?)) | '
              'NetIn], Outs?, D?) :- (Id? =?= Id1?), ground(From?) | '
              'respond_coldcall(offer(From?), Resp, D1), '
              'serve(Reqs?, Id?, NetIn?, Outs, D2), merge(D1?, D2?, D).'));
      expect(q, contains('serve([quit | _], _, _, [], []).'));
      expect(q, isNot(contains('close(')));
      expect(q, isNot(contains('withdraw')));
    });

    test('a procedure that reaches no question is unchanged', () {
      expect(q, contains('procedure decide(YesNo?, Peer?, Response).'));
      expect(q, contains('decide(yes, From, accept(From?)).'));
      expect(q, contains('decide(no, From, refuse(From?)).'));
    });

    test('the types the compilation adds: the questions, one functor per '
        'moded interactive type wrapping it as written, and, with no '
        'dispatcher\'s generic source to define it, one ask/2 over their '
        'union; no handle', () {
      expect(
          q,
          contains('Question ::= stream_request_r(Stream(Request)?) ; '
              'card_w(Card).'));
      expect(q, contains('Ask(Q) ::= ask(Constant, Q).'));
      expect(q, isNot(contains('Handle')));
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
              'stream_string_r(X)) | D?]) :- chat1(S1?, S2, X?, D).'));
      expect(
          s,
          contains('procedure chat1(Peer?, Stream(Msg), Stream(String)?, '
              'Stream(Ask(Question))).'));
      expect(s, contains('chat1(Peer, Out?, Ms, []) :- ground(Peer?) | '
          'send_all(Peer?, Ms?, Out).'));
      expect(s,
          contains('Question ::= stream_string_r(Stream(String)?).'));
      expect(s, contains('Ask(Q) ::= ask(Constant, Q).'));
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
      // The questions take the parameter an interactive type names, and every
      // declaration with an ask stream takes it with them.
      expect(s,
          contains('exported procedure(X) go(Stream(Ask(Question(X)))).'));
      expect(s, contains("go([ask('T?', t_r(X)) | D?]) :- go1(X?, D)."));
      expect(s, contains('procedure(X) go1(T?, Stream(Ask(Question(X)))).'));
      expect(s, contains('go1(t, []).'));
      expect(s, contains('procedure(X) take(X, Stream(Ask(Question(X)))).'));
      expect(s,
          contains('procedure(X) take1(X, Box(X)?, '
              'Stream(Ask(Question(X)))).'));
      expect(
          s,
          contains("take(S1?, [ask('Box(X)?', box_x_r(X)) | D?]) :- "
              'take1(S1, X?, D).'));
      expect(s, contains('Question(X) ::= t_r(T?) ; box_x_r(Box(X)?).'));
      expect(s, contains('Ask(Q) ::= ask(Constant, Q).'));
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
          contains("ask(S1, [ask('T?', t_r(X)) | D?]) :- "
              'ask1_1(S1?, X?, D).'));
      expect(c.source,
          contains('ask1_1(N, t, []) :- ground(N?) | ask1(N?).'));
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
      expect(c.source, contains('p1(_, _?, t, []).'));
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

    group('the anonymous variable as the interactive term is refused, in '
        'either mode, with the reason', () {
      // "the interactive term A is a term of type T, possibly a variable but
      // not the anonymous variable" (Definition "Guarded Clause, ..." at
      // c994328); an anonymous variable is any variable whose name begins
      // with _ (GLP-Spec, Remark "Anonymous Variables"), so _Name is refused
      // as _ is.  vGLP #5 Cowork 2026-10-03 08:16 UTC, item 1: c641358a's
      // writer-mode refusal widened to both modes, and 80953e04's treatment of
      // _Name as the anonymous variable made the same refusal.
      Matcher refusal(String clause, String mode, int line) =>
          throwsA(isA<CompileError>()
              .having(
                  (e) => e.message,
                  'message',
                  allOf(
                      contains('The clause $clause '),
                      contains('is in $mode mode'),
                      contains('the interactive term is not the anonymous '
                          'variable (vGLP, Definition "Guarded Clause, '
                          'Volitional Procedure, ...")')))
              .having((e) => e.line, 'line', line));

      const reader = '''
YesNo ::= yes ; no.
procedure (YesNo?)*ask(Integer?, Integer).
(yes)*ask(N, N?).
''';
      const writer = '''
YesNo ::= yes ; no.
Note ::= note(YesNo).
procedure (Note)*tell(YesNo?).
(note(A?))*tell(A).
''';

      test('in reader mode, (_)', () {
        expect(() => compile('$reader(_)*ask(_, 0).\n'),
            refusal('(_)*ask(_, 0)', 'reader', 4));
      });

      test('in reader mode, (_?)', () {
        expect(() => compile('$reader(_?)*ask(_, 0).\n'),
            refusal('(_?)*ask(_, 0)', 'reader', 4));
      });

      test('in reader mode, (_Name)', () {
        expect(() => compile('$reader(_Answer)*ask(_, 0).\n'),
            refusal('(_Answer)*ask(_, 0)', 'reader', 4));
      });

      test('in reader mode, (_Name?)', () {
        expect(() => compile('$reader(_Answer?)*ask(_, 0).\n'),
            refusal('(_Answer?)*ask(_, 0)', 'reader', 4));
      });

      test('in reader mode, a (_) unit clause, a (_) clause whose body is '
          'true, and one whose head passes a pair', () {
        expect(() => compile('''
T ::= t.
procedure (T?)*p(Integer?).
(_)*p(0).
'''), refusal('(_)*p(0)', 'reader', 3));
        expect(() => compile('''
T ::= t.
procedure (T?)*p(Integer?).
(t)*p(1).
(_)*p(N) :- N? > 0 | true.
'''), refusal('(_)*p(N)', 'reader', 4));
        expect(() => compile('''
T ::= t.
procedure (T?)*p(Integer?, Integer).
(_)*p(A, A?).
'''), refusal('(_)*p(A, A?)', 'reader', 3));
      });

      test('in writer mode, (_)', () {
        expect(() => compile('$writer(_)*tell(no).\n'),
            refusal('(_)*tell(no)', 'writer', 5));
      });

      test('in writer mode, (_?)', () {
        expect(() => compile('$writer(_?)*tell(no).\n'),
            refusal('(_?)*tell(no)', 'writer', 5));
      });

      test('in writer mode, (_Name)', () {
        expect(() => compile('$writer(_Note)*tell(no).\n'),
            refusal('(_Note)*tell(no)', 'writer', 5));
      });

      test('in writer mode, (_Name?)', () {
        expect(() => compile('$writer(_Note?)*tell(no).\n'),
            refusal('(_Note?)*tell(no)', 'writer', 5));
      });

      test('a nullary procedure\'s (_) clause is named too, in either mode',
          () {
        expect(() => compile('''
Note ::= note.
procedure (Note)*tell.
(note)*tell.
(_)*tell :- true | true.
'''), refusal('(_)*tell', 'writer', 4));
        expect(() => compile('''
Note ::= note.
procedure (Note?)*hear.
(_)*hear.
'''), refusal('(_)*hear', 'reader', 3));
      });

      test('the anonymous variable inside the interactive term is not the '
          'interactive term, and is not refused: the clause drops the reader '
          'of a question inside its output', () {
        // "A reader the program drops leaves its construct on screen, showing
        // what the program writes into it" (sections/elicitation.tex, the
        // paragraph before Definition "Canonical Compilation").
        final s = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Card ::= card(Peer, YesNo?).
procedure (Card)*show(Peer?).
(card(P?, _))*show(P).
''');
        expect(s, contains('show1(P, card(P?, _), []).'));
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
      expect(c.source,
          contains('procedure p(Integer?, Stream(Ask(Question))).'));
      expect(c.source, contains('p(N, D?) :- r(N?, D).'));
      expect(c.source, contains('p(0, []).'));
      expect(c.source,
          contains('procedure r(Integer?, Stream(Ask(Question))).'));
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
      expect(s, contains('Ask(Q) ::= ask(Constant, Q).'));
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
      expect(c.questionType, 'Question_1');
      expect(c.source, contains('Question_1 ::= ask_r(Ask?).'));
      expect(c.source, contains('Ask_1(Q) ::= ask(Constant, Q).'));
      expect(c.source,
          contains('procedure p1(Handle?, Ask?, Stream(Ask_1(Question_1))).'));
      // The program's Handle is its own; the compilation adds none.
      expect(c.source, isNot(contains('Handle_1')));
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
      expect(s,
          contains('exported procedure go(Integer?, Stream(Ask(Question))).'));
      expect(s, contains('go(N, D?) :- q(N?, D).'));
    });
  });

  group('(iii) the old syntax keeps its old compilation', () {
    final sources = _vglpSources(Directory(_programs));
    String base(File f) => f.path.split(Platform.pathSeparator).last;

    // Each source in the tree is asked what it is, and none is named or
    // counted here (GLP #3 Cowork, 2026-10-10 07:48 UTC, "00:45. Q2").  Until
    // 2026-10-10 the test counted nine old sources and named the two new
    // ones, and went red when sGLP's two sources in the paper's syntax
    // joined the tree (f264ccc6).
    test('each source in the tree is asked what it is: one holding a '
        'volition guard is not in the paper\'s syntax, one declaring a '
        'volitional procedure is', () {
      expect(sources, isNotEmpty);
      for (final f in sources) {
        final text = f.readAsStringSync();
        final old = _holdsVolitionGuard(text);
        final paper = !old && _declaresVolitionalProcedure(text);
        expect(old || paper, isTrue,
            reason: '${f.path} holds no volition guard and declares no '
                'volitional procedure');
        expect(isPaperSyntaxSource(text), paper, reason: f.path);
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
