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
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/mediator.dart' show MediatorSource;
import 'package:glp_runtime/vglp/program_compilation.dart'
    show compiledHeader, compileVglpSource;

const _programs = '../programs';

String _vglp(String dir, String name) =>
    File('$_programs/tests/vglp/$dir/$name.vglp').readAsStringSync();

/// The exports of the modules [sources], each N's source by N, as the
/// compilation reads them: N's own canonical compilation, its imports read
/// from the same sources (vGLP, Definition "Canonical Compilation": the
/// imports of procedures that reach a question "are compiled as their
/// exports are"; vGLP's task of 2026-10-10 11:43 UTC).  A module not among
/// them has no vGLP export.
ExportReader _exportsOf(Map<String, String> sources) {
  late final ExportReader reader;
  reader = (module) {
    final text = sources[module];
    return text == null ? null : compileCanonical(text, exports: reader);
  };
  return reader;
}

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

  group('an imported volitional procedure (vGLP at 7bf50ee, Definition '
      '"Canonical Compilation", its last sentence)', () {
    // "A declaration imported procedure (T)*N#p(T1, ..., Tn). of a volitional
    // procedure of another module N is compiled as its export is, to the
    // import of p with its ask stream added, and T joins the interactive
    // types of M: the type of the asks has its functor and ⌈M⌉ its construct
    // process, so an ask of p on a caller's ask stream is served as any
    // other."  vGLP's task of 2026-10-10 09:06 UTC.  The runs, the question
    // answered through the importer's own dispatcher, are elicitation_test's.
    // Since vGLP's task of 11:43 UTC the compilation reads N's export, so
    // each import here is given its module's source, N's own canonical
    // compilation its export (the next group).
    const types = '''
Peer     ::= Constant.
YesNo    ::= yes ; no.
Offer    ::= offer(Peer).
Response ::= accept(Peer) ; refuse(Peer).
Card     ::= card(Peer, YesNo?).
''';
    const responder = '''
$types
exported procedure (Card)*respond(Offer?, Response).
(card(From?, Answer))*respond(offer(From), Resp?) :-
    ground(From?) | decide(Answer?, From?, Resp).
procedure decide(YesNo?, Peer?, Response).
decide(yes, From, accept(From?)).
decide(no, From, refuse(From?)).
''';
    final modules = _exportsOf({
      'responder': responder,
      'agent': '''
$types
Request ::= post(String) ; quit.
exported procedure (Stream(Request)?)*agent(Peer?, Stream(String)).
(Rs)*agent(P, Outs?) :- ground(P?) | serve(Rs?, Outs).
procedure serve(Stream(Request)?, Stream(String)).
serve([post(S) | Rs], [S? | Outs?]) :- serve(Rs?, Outs).
serve([quit | _], []).
serve([], []).
''',
      'n': '''
exported procedure (Stream(String))*q(Integer?).
(["hi"])*q(_).
''',
      'm': '''
exported procedure(X) (Stream(X)?)*take(X?).
(Xs)*take(_).
''',
      'n1': '''
Stream_string ::= s(String).
exported procedure (Stream_string)*p(Integer?).
(s(_))*p(_).
''',
      'n2': '''
exported procedure (Stream(String))*q(Integer?).
(["hi"])*q(_).
''',
    });

    test('is compiled as its export is, to the import of p with its ask stream '
        'added; a call of it reaches a question and is given an ask stream, '
        'keeping p\'s name; and its type joins the questions', () {
      final c = compileCanonical('''
$types
imported procedure (Card)*responder#respond(Offer?, Response).

exported procedure befriend(Peer?, Response).
befriend(P, R?) :- ground(P?) | responder # respond(offer(P?), R).
''', exports: modules);
      expect(
          c.source,
          contains('imported procedure responder#respond(Offer?, Response, '
              'Stream(Ask(Question))).'));
      expect(c.source, isNot(contains('respond(Offer?, Response, Card)')));
      expect(
          c.source,
          contains('exported procedure befriend(Peer?, Response, '
              'Stream(Ask(Question))).'));
      expect(
          c.source,
          contains('befriend(P, R?, D?) :- ground(P?) | '
              'responder # respond(offer(P?), R, D).'));
      expect(c.source, contains('Question ::= card_w(Card).'));
      expect(c.reaching, {'befriend/2'});
      expect(c.functors, {'Card': 'card_w'});
      expect(c.volitional, isEmpty);
      // No asking clause and no clauses of p: they are the module N's.
      expect(c.module.procedures.map((p) => p.signature), ['befriend/3']);
    });

    test('its interactive type joins the program\'s own, in the order of the '
        'declarations, a reader-mode one t_r; calls of both merged', () {
      final c = compileCanonical('''
$types
Note    ::= note(String).
Request ::= post(String) ; quit.

exported procedure (Note?)*jot(Note).
(N)*jot(N?).

imported procedure (Stream(Request)?)*agent#agent(Peer?, Stream(String)).

procedure go(Peer?, Note, Stream(String)).
go(P, N?, Outs?) :- agent # agent(P?, Outs), jot(N).
''', exports: modules);
      expect(c.source,
          contains('Question ::= note_r(Note?) ; '
              'stream_request_r(Stream(Request)?).'));
      expect(
          c.source,
          contains('imported procedure agent#agent(Peer?, Stream(String), '
              'Stream(Ask(Question))).'));
      expect(
          c.source,
          contains('go(P, N?, Outs?, D?) :- agent # agent(P?, Outs, D1), '
              'jot(N, D2), merge(D1?, D2?, D).'));
      expect(c.functors,
          {'Note?': 'note_r', 'Stream(Request)?': 'stream_request_r'});
      expect(c.reaching, {'jot/1', 'go/3'});
    });

    test('a type shared with a volitional procedure of the program\'s own is '
        'one alternative of the questions', () {
      final s = compileCanonical('''
$types
exported procedure (Card)*show(Offer?).
(card(P?, _))*show(offer(P)).

imported procedure (Card)*responder#respond(Offer?, Response).
''', exports: modules).source;
      expect(s, contains('Question ::= card_w(Card).'));
      expect(
          s,
          contains('imported procedure responder#respond(Offer?, Response, '
              'Stream(Ask(Question))).'));
    });

    test('an import keeps the Definition\'s functor, the one its module\'s '
        'asking clause writes, and a type of the program\'s own that coincides '
        'with it is told apart', () {
      final c = compileCanonical('''
Stream_string ::= s(String).
exported procedure (Stream_string)*p(Integer?).
(s(_))*p(_).
imported procedure (Stream(String))*n#q(Integer?).
''', exports: modules);
      expect(c.functors,
          {'Stream(String)': 'stream_string_w', 'Stream_string': 'stream_string_w_2'});
      expect(c.source,
          contains("p(S1, [ask('Stream_string', stream_string_w_2(X?)) | D?])"));
    });

    test('a type parameter of the import\'s declaration is the questions\' '
        'and the compiled import\'s', () {
      final s = compileCanonical('''
imported procedure(X) (Stream(X)?)*m#take(X?).
''', exports: modules).source;
      expect(s, contains('Question(X) ::= stream_x_r(Stream(X)?).'));
      expect(
          s,
          contains('imported procedure(X) m#take(X?, '
              'Stream(Ask(Question(X)))).'));
    });

    group('is refused', () {
      void refused(String source, String why) => expect(
          () => compileCanonical(source, exports: modules),
          throwsA(isA<CompileError>()
              .having((e) => e.message, 'message', contains(why))));

      test('naming no module', () {
        refused('''
$types
imported procedure (Card)*respond(Offer?, Response).
''', 'names the module it is of');
      });

      test('called with n+1 arguments', () {
        refused('''
$types
imported procedure (Card)*responder#respond(Offer?, Response).
procedure go(Response, Card?).
go(R?, C) :- responder # respond(offer(bob), R, C?).
''', 'until it is asked');
      });

      // Two types of ONE module whose functors coincide are told apart in
      // it, `_2`, and travel so (the next group); two of two modules are
      // refused.
      test('two imported types with one functor', () {
        refused('''
Stream_string ::= s(String).
imported procedure (Stream_string)*n1#p(Integer?).
imported procedure (Stream(String))*n2#q(Integer?).
''', 'so has another imported interactive type');
      });
    });
  });

  group('the import of a procedure of another module that reaches a question '
      '(vGLP, Definition "Canonical Compilation", the import sentence; '
      'vGLP\'s task of 2026-10-10 11:43 UTC)', () {
    // "A declaration imported procedure (T)*N#p(T1,...,Tn). of a volitional
    // procedure of another module N, and a declaration imported procedure
    // N#p(T1,...,Tn). of an ordinary procedure of N that reaches a question,
    // are compiled as their exports are, to the import of p with its ask
    // stream added, and the interactive types of N join those of M, each with
    // the functor its export gives it and its widget: the type of the asks
    // has their functors and ⌈M⌉ their construct processes, so an ask on a
    // caller's ask stream, of p or of a question p reaches in N, is served
    // as any other."  The construct processes and widgets are
    // constructs_test's; the runs, through the importer's own dispatcher,
    // elicitation_test's.
    const types = '''
Peer      ::= Constant.
YesNo     ::= yes ; no.
Offer     ::= offer(Peer).
Response  ::= accept(Peer) ; refuse(Peer).
Card      ::= card(Peer, YesNo?).
Note      ::= note(String).
Box(X)    ::= box(X).
Box_yesNo ::= box_yn(YesNo).
''';
    // N: respond asks a card and, on yes, a note through jot, which N does
    // not export; offer_from, ordinary, reaches both questions; echo reaches
    // none.
    const responder = '''
$types
exported procedure (Card)*respond(Offer?, Response, Note).
(card(From?, Answer))*respond(offer(From), Resp?, N?) :-
    ground(From?) | decide(Answer?, From?, Resp, N).

procedure decide(YesNo?, Peer?, Response, Note).
decide(yes, From, accept(From?), N?) :- jot(N).
decide(no, From, refuse(From?), note("")).

procedure (Note?)*jot(Note).
(N)*jot(N?).

exported procedure offer_from(Peer?, Response, Note).
offer_from(P, R?, N?) :- ground(P?) | respond(offer(P?), R, N).

exported procedure echo(Peer?, Peer).
echo(P, P?).
''';
    // N: two interactive types whose functors coincide, box_yesNo_r, told
    // apart in N, the second declared box_yesNo_r_2.
    const boxes = '''
$types
exported procedure (Box(YesNo)?)*pick(Peer?, YesNo).
(box(A))*pick(P, A?) :- ground(P?) | true.

exported procedure (Box_yesNo?)*confirm(Peer?, YesNo).
(box_yn(A))*confirm(P, A?) :- ground(P?) | true.
''';
    // N importing from K: relay calls boxes' confirm.
    const relay = '''
$types
imported procedure (Box_yesNo?)*boxes#confirm(Peer?, YesNo).
exported procedure check(Peer?, YesNo).
check(P, A?) :- ground(P?) | boxes # confirm(P?, A).
''';
    final modules = _exportsOf(
        {'responder': responder, 'boxes': boxes, 'relay': relay});

    test('N\'s export: its interactive types, with their functors in N, and '
        'the procedures it exports that reach a question', () {
      final n = modules('responder')!;
      expect(n.interactive.map((t) => '${t.functor}(${t.written})'),
          ['card_w(Card)', 'note_r(Note?)']);
      expect(n.exportedReaching, {'respond/3', 'offer_from/3'});
      expect(modules('boxes')!.interactive.map((t) => '${t.functor}(${t.written})'),
          ['box_yesNo_r(Box(YesNo)?)', 'box_yesNo_r_2(Box_yesNo?)']);
      expect(modules('lib'), isNull);
    });

    test('(Q1) an import of p brings every interactive type of N, one p '
        'does not name and a question p reaches in N among them, each with '
        'N\'s functor', () {
      final c = compileCanonical('''
$types
imported procedure (Card)*responder#respond(Offer?, Response, Note).

exported procedure befriend(Peer?, Response, Note).
befriend(P, R?, N?) :- ground(P?) | responder # respond(offer(P?), R, N).
''', exports: modules);
      expect(c.source, contains('Question ::= card_w(Card) ; note_r(Note?).'));
      expect(
          c.source,
          contains('imported procedure responder#respond(Offer?, Response, '
              'Note, Stream(Ask(Question))).'));
      expect(
          c.source,
          contains('befriend(P, R?, N?, D?) :- ground(P?) | '
              'responder # respond(offer(P?), R, N, D).'));
      expect(c.functors, {'Card': 'card_w', 'Note?': 'note_r'});
      expect(c.interactive.map((t) => t.from), ['responder', 'responder']);
      expect(c.exportedReaching, {'befriend/3'});
    });

    test('(Q1) the interactive types of N are those N imports too', () {
      final c = compileCanonical('''
$types
imported procedure relay#check(Peer?, YesNo).

exported procedure go(Peer?, YesNo).
go(P, A?) :- ground(P?) | relay # check(P?, A).
''', exports: modules);
      expect(
          c.source,
          contains('Question ::= box_yesNo_r(Box(YesNo)?) ; '
              'box_yesNo_r_2(Box_yesNo?).'));
      expect(
          c.source,
          contains('imported procedure relay#check(Peer?, YesNo, '
              'Stream(Ask(Question))).'));
    });

    test('(Q2) an ordinary procedure of N that reaches a question, imported '
        'in vGLP arity, is compiled with its ask stream added, and a program '
        'with no volitional procedure of its own reaches a question through '
        'it', () {
      const home = '''
$types
imported procedure responder#offer_from(Peer?, Response, Note).

exported procedure befriend(Peer?, Response, Note).
befriend(P, R?, N?) :- ground(P?) | responder # offer_from(P?, R, N).
''';
      expect(isPaperSyntaxSource(home), isFalse);
      expect(importsReachingProcedure(home, modules), isTrue);
      final c = compileCanonical(home, exports: modules);
      expect(
          c.source,
          contains('imported procedure responder#offer_from(Peer?, Response, '
              'Note, Stream(Ask(Question))).'));
      expect(
          c.source,
          contains('befriend(P, R?, N?, D?) :- ground(P?) | '
              'responder # offer_from(P?, R, N, D).'));
      expect(c.source, contains('Question ::= card_w(Card) ; note_r(Note?).'));
      expect(c.reaching, {'befriend/3'});
      expect(c.volitional, isEmpty);
    });

    test('(Q2) an ordinary procedure of N that reaches no question, and a '
        'procedure of a module with no vGLP export, are imported as GLP\'s '
        'are, and bring no question', () {
      const home = '''
$types
imported procedure responder#echo(Peer?, Peer).
imported procedure lib#twice(Peer?, Peer).

exported procedure (Note?)*jot(Note).
(N)*jot(N?).

procedure go(Peer?, Peer).
go(P, Q?) :- responder # echo(P?, Q).
procedure go2(Peer?, Peer).
go2(P, Q?) :- lib # twice(P?, Q).
''';
      final c = compileCanonical(home, exports: modules);
      expect(c.source, contains('imported procedure responder#echo(Peer?, Peer).'));
      expect(c.source, contains('imported procedure lib#twice(Peer?, Peer).'));
      expect(c.source, contains('go(P, Q?) :- responder # echo(P?, Q).'));
      expect(c.source, contains('go2(P, Q?) :- lib # twice(P?, Q).'));
      expect(c.source, contains('Question ::= note_r(Note?).'));
      expect(c.reaching, {'jot/1'});
      expect(
          importsReachingProcedure('''
$types
imported procedure responder#echo(Peer?, Peer).
''', modules),
          isFalse);
    });

    test('(Q3) a `_2` functor of N travels with its export, and a type of the '
        'program\'s own that is that one takes it', () {
      final c = compileCanonical('''
$types
imported procedure (Box_yesNo?)*boxes#confirm(Peer?, YesNo).

exported procedure (Box_yesNo?)*recheck(Peer?, YesNo).
(box_yn(A))*recheck(P, A?) :- ground(P?) | true.

exported procedure ask_confirm(Peer?, YesNo).
ask_confirm(P, A?) :- ground(P?) | boxes # confirm(P?, A).
''', exports: modules);
      expect(c.functors,
          {'Box(YesNo)?': 'box_yesNo_r', 'Box_yesNo?': 'box_yesNo_r_2'});
      expect(
          c.source,
          contains('Question ::= box_yesNo_r(Box(YesNo)?) ; '
              'box_yesNo_r_2(Box_yesNo?).'));
      expect(c.source,
          contains("[ask('Box_yesNo?', box_yesNo_r_2(X)) | D?]"));
      expect(
          c.source,
          contains('imported procedure boxes#confirm(Peer?, YesNo, '
              'Stream(Ask(Question))).'));
      expect(c.interactive, hasLength(2));
      expect(c.interactive.last.alsoOwn, isTrue);
    });

    group('is refused', () {
      void refused(String source, String why, {ExportReader? exports}) =>
          expect(
              () => compileCanonical(source, exports: exports),
              throwsA(isA<CompileError>()
                  .having((e) => e.message, 'message', contains(why))));

      test('an import of a volitional procedure, no export being given', () {
        refused('''
$types
imported procedure (Card)*responder#respond(Offer?, Response, Note).
''', 'none is given it');
      });

      test('an import of a volitional procedure of a module with no vGLP '
          'export', () {
        refused('''
$types
imported procedure (Card)*lib#respond(Offer?, Response, Note).
''', 'lib is no vGLP module whose export the compilation reads',
            exports: modules);
      });

      test('naming an interactive type N does not give p', () {
        refused('''
$types
imported procedure (Note?)*boxes#confirm(Peer?, YesNo).
''', 'and boxes declares (Box_yesNo?)*confirm', exports: modules);
      });

      test('an ordinary procedure of N imported as a volitional one', () {
        refused('''
$types
imported procedure (Card)*responder#offer_from(Peer?, Response, Note).
''', 'is an ordinary procedure that reaches a question', exports: modules);
      });

      test('a volitional procedure N does not export', () {
        refused('''
$types
imported procedure (Note?)*responder#jot(Note).
''', 'which responder does not export', exports: modules);
      });

      test('a volitional procedure of N imported as an ordinary one', () {
        refused('''
$types
imported procedure responder#respond(Offer?, Response, Note).
''', 'as an ordinary one', exports: modules);
      });

      test('an ordinary import that reaches a question called with n+1 '
          'arguments', () {
        refused('''
$types
imported procedure responder#offer_from(Peer?, Response, Note).
procedure go(Response, Note, Stream(_)).
go(R?, N?, D?) :- responder # offer_from(bob, R, N, D).
''', 'its ask stream is the compilation\'s', exports: modules);
      });

      test('one type joining from two modules with two functors', () {
        // one gives Box_yesNo? box_yesNo_r; two, which imports boxes' and
        // declares its own of the type before, box_yesNo_r_2.
        final three = _exportsOf({
          'boxes': boxes,
          'one': '''
$types
exported procedure (Box_yesNo?)*one(Peer?, YesNo).
(box_yn(A))*one(P, A?) :- ground(P?) | true.
''',
          'two': '''
$types
exported procedure (Box_yesNo?)*two(Peer?, YesNo).
(box_yn(A))*two(P, A?) :- ground(P?) | true.
imported procedure (Box(YesNo)?)*boxes#pick(Peer?, YesNo).
''',
        });
        expect(three('two')!.interactive.map((t) => '${t.functor}(${t.written})'),
            ['box_yesNo_r_2(Box_yesNo?)', 'box_yesNo_r(Box(YesNo)?)']);
        refused('''
$types
imported procedure (Box_yesNo?)*one#one(Peer?, YesNo).
imported procedure (Box_yesNo?)*two#two(Peer?, YesNo).
''', 'joins the questions with the functor box_yesNo_r from one and '
            'box_yesNo_r_2 from two', exports: three);
      });
    });

    group('read beside the source (compileVglpSource)', () {
      late Directory dir;
      setUp(() => dir = Directory.systemTemp.createTempSync('vglp_exports_'));
      tearDown(() => dir.deleteSync(recursive: true));
      String write(String name, String text) {
        final f = File('${dir.path}/$name')..writeAsStringSync(text);
        return f.path;
      }

      const home = '''
$types
imported procedure (Card)*responder#respond(Offer?, Response, Note).
exported procedure befriend(Peer?, Response, Note).
befriend(P, R?, N?) :- ground(P?) | responder # respond(offer(P?), R, N).
''';

      test('the module file N.vglp beside it, and its compiled module N.glp '
          'beside that; an N.glp written by hand stands, a GLP module', () {
        write('responder.vglp', responder);
        final path = write('home.vglp', home);
        final s = compileVglpSource(home, path: path);
        expect(s, contains('Question ::= card_w(Card) ; note_r(Note?).'));
        write('responder.glp', compileVglpSource(responder));
        expect(compileVglpSource(home, path: path), s);
        write('responder.glp', '%% written by hand\n');
        expect(
            () => compileVglpSource(home, path: path),
            throwsA(isA<CompileError>().having((e) => e.message, 'message',
                contains('responder is no vGLP module'))));
      });

      test('a source in neither syntax that imports a procedure reaching a '
          'question is compiled by the canonical compilation', () {
        write('responder.vglp', responder);
        const plain = '''
$types
imported procedure responder#offer_from(Peer?, Response, Note).
exported procedure befriend(Peer?, Response, Note).
befriend(P, R?, N?) :- ground(P?) | responder # offer_from(P?, R, N).
''';
        final s = compileVglpSource(plain, path: write('home.vglp', plain));
        expect(s, startsWith(compiledHeader));
        expect(
            s,
            contains('imported procedure responder#offer_from(Peer?, '
                'Response, Note, Stream(Ask(Question))).'));
      });

      // "Modules that import from each other procedures that reach a
      // question are compiled together, their interactive types joined
      // once" (vGLP at 66b6500, the Definition; vGLP #7 Cowork, 2026-10-11
      // 14:19 UTC, item 2).  Until then the compilation refused them.
      test('modules importing from each other are compiled together, their '
          'interactive types joined once', () {
        const a = '''
$types
imported procedure (Note?)*b#jot_b(Note).
exported procedure (Note?)*jot_a(Note).
(N)*jot_a(N?).
''';
        const b = '''
$types
imported procedure (Note?)*a#jot_a(Note).
exported procedure (Note?)*jot_b(Note).
(N)*jot_b(N?).
''';
        final pb = write('b.vglp', b);
        final sa = compileVglpSource(a, path: write('a.vglp', a));
        final sb = compileVglpSource(b, path: pb);
        expect(sa, contains('Question ::= note_r(Note?).'));
        expect(sb, contains('Question ::= note_r(Note?).'));
        expect(
            sa,
            contains('imported procedure b#jot_b(Note, '
                'Stream(Ask(Question))).'));
        expect(
            sb,
            contains('imported procedure a#jot_a(Note, '
                'Stream(Ask(Question))).'));
      });

      // Two modules of two interactive types, each importing a procedure of
      // the other that reaches a question: cards a volitional one, notes an
      // ordinary one.
      const cards = '''
$types
imported procedure notes#noting(Peer?, Note).
exported procedure (Card)*respond(Offer?, Response).
(card(From?, Answer))*respond(offer(From), Resp?) :-
    ground(From?) | decide(Answer?, From?, Resp).
procedure decide(YesNo?, Peer?, Response).
decide(yes, From, accept(From?)).
decide(no, From, refuse(From?)).
exported procedure befriend(Peer?, Response, Note).
befriend(P, R?, N?) :-
    ground(P?) | respond(offer(P?), R), notes # noting(P?, N).
''';
      const notes = '''
$types
exported procedure (Note?)*jot(Note).
(N)*jot(N?).
exported procedure noting(Peer?, Note).
noting(P, N?) :- ground(P?) | jot(N).
imported procedure (Card)*cards#respond(Offer?, Response).
exported procedure noted(Peer?, Response, Note).
noted(P, R?, N?) :-
    ground(P?) | cards # respond(offer(P?), R), noting(P?, N).
''';

      test('two modules each importing a procedure of the other that reaches '
          'a question: each has every question of both, each type with one '
          'functor, and the procedures that reach one through the other '
          'have their ask streams', () {
        final pn = write('notes.vglp', notes);
        final sc = compileVglpSource(cards, path: write('cards.vglp', cards));
        final sn = compileVglpSource(notes, path: pn);
        // The questions in each module's order of its declarations, of one
        // union.
        expect(sc, contains('Question ::= card_w(Card) ; note_r(Note?).'));
        expect(sn, contains('Question ::= note_r(Note?) ; card_w(Card).'));
        expect(
            sc,
            contains('imported procedure notes#noting(Peer?, Note, '
                'Stream(Ask(Question))).'));
        expect(
            sc,
            contains('befriend(P, R?, N?, D?) :- ground(P?) | '
                'respond(offer(P?), R, D1), notes # noting(P?, N, D2), '
                'merge(D1?, D2?, D).'));
        expect(
            sn,
            contains('imported procedure cards#respond(Offer?, Response, '
                'Stream(Ask(Question))).'));
        expect(sn, contains("noting(P, N?, D?) :- ground(P?) | jot(N, D)."));
        expect(sn, contains("[ask('Note?', note_r(X)) | D?]"));
        expect(sc, contains("[ask('Card', card_w(X?)) | D?]"));
      });

      test('a cycle of three: each module has the questions of all three, '
          'each type with one functor', () {
        // x imports from y, y from z, z from x.
        const x = '''
$types
imported procedure (Note?)*y#jot(Note).
exported procedure (Card)*respond(Offer?, Response).
(card(From?, Answer))*respond(offer(From), Resp?) :-
    ground(From?) | decide(Answer?, From?, Resp).
procedure decide(YesNo?, Peer?, Response).
decide(yes, From, accept(From?)).
decide(no, From, refuse(From?)).
''';
        const y = '''
$types
exported procedure (Note?)*jot(Note).
(N)*jot(N?).
imported procedure z#check(Peer?, YesNo).
exported procedure both(Peer?, Note, YesNo).
both(P, N?, A?) :- ground(P?) | jot(N), z # check(P?, A).
''';
        const z = '''
$types
exported procedure (Box_yesNo?)*confirm(Peer?, YesNo).
(box_yn(A))*confirm(P, A?) :- ground(P?) | true.
imported procedure (Card)*x#respond(Offer?, Response).
exported procedure check(Peer?, YesNo).
check(P, A?) :- ground(P?) | confirm(P?, A).
exported procedure checked(Peer?, Response, YesNo).
checked(P, R?, A?) :- ground(P?) | x # respond(offer(P?), R), check(P?, A).
''';
        final px = write('x.vglp', x), py = write('y.vglp', y);
        final pz = write('z.vglp', z);
        const all = ['card_w(Card)', 'note_r(Note?)', 'box_yesNo_r(Box_yesNo?)'];
        for (final (path, text) in [(px, x), (py, y), (pz, z)]) {
          final s = compileVglpSource(text, path: path);
          final q = RegExp(r'^Question ::= (.*)\.$', multiLine: true)
              .firstMatch(s)!
              .group(1)!
              .split(' ; ');
          expect(q.toSet(), all.toSet(), reason: path);
          expect(q, hasLength(3), reason: path);
        }
      });

      test('a module beside them, importing from one of them, compiled as its '
          'own: its questions are all of theirs', () {
        write('cards.vglp', cards);
        write('notes.vglp', notes);
        const home = '''
$types
imported procedure cards#befriend(Peer?, Response, Note).
exported procedure go(Peer?, Response, Note).
go(P, R?, N?) :- ground(P?) | cards # befriend(P?, R, N).
''';
        final s = compileVglpSource(home, path: write('home.vglp', home));
        expect(s, contains('Question ::= card_w(Card) ; note_r(Note?).'));
        expect(
            s,
            contains('imported procedure cards#befriend(Peer?, Response, Note, '
                'Stream(Ask(Question))).'));
        expect(s, contains('go(P, R?, N?, D?) :- ground(P?) | '
            'cards # befriend(P?, R, N, D).'));
      });

      test('modules importing from each other, one way only reaching a '
          'question: each is compiled as its own, as beside a module that '
          'imports nothing from it', () {
        // a imports b's echo, which reaches no question; b imports a's jot,
        // which does.
        const a = '''
$types
imported procedure b#echo(Peer?, Peer).
exported procedure (Note?)*jot_a(Note).
(N)*jot_a(N?).
procedure go(Peer?, Peer).
go(P, Q?) :- b # echo(P?, Q).
''';
        const b = '''
$types
exported procedure echo(Peer?, Peer).
echo(P, P?).
imported procedure (Note?)*a#jot_a(Note).
exported procedure noting(Note).
noting(N?) :- a # jot_a(N).
''';
        final pa = write('a.vglp', a);
        final pb = write('b.vglp', b);
        final sa = compileVglpSource(a, path: pa);
        final sb = compileVglpSource(b, path: pb);
        expect(sa, contains('imported procedure b#echo(Peer?, Peer).'));
        expect(sb, contains('Question ::= note_r(Note?).'));
        // b beside an a that imports nothing from it compiles to the same
        // text, and a beside a b that imports nothing from it.
        write('a.vglp', '''
$types
exported procedure (Note?)*jot_a(Note).
(N)*jot_a(N?).
''');
        expect(compileVglpSource(b, path: pb), sb);
        write('a.vglp', a);
        write('b.vglp', '''
$types
exported procedure echo(Peer?, Peer).
echo(P, P?).
''');
        expect(compileVglpSource(a, path: pa), sa);
      });

      // a and b import from each other jot_a and jot_b.
      String a(String more) => '''
$types
imported procedure (Note?)*b#jot_b(Note).
exported procedure (Note?)*jot_a(Note).
(N)*jot_a(N?).
$more''';
      String b(String more) => '''
$types
imported procedure (Note?)*a#jot_a(Note).
exported procedure (Note?)*jot_b(Note).
(N)*jot_b(N?).
$more''';

      // a's pick and b's confirm, two types of two of them with one functor,
      // box_yesNo_r.
      final pick = a('exported procedure (Box(YesNo)?)*pick(Peer?, '
          'YesNo).\n(box(A))*pick(P, A?) :- ground(P?) | true.\n');
      final confirm = b('exported procedure (Box_yesNo?)*confirm(Peer?, '
          'YesNo).\n(box_yn(A))*confirm(P, A?) :- ground(P?) | true.\n');

      // The line of [text] holding [what], from 1.
      int lineOf(String text, String what) =>
          text.split('\n').indexWhere((l) => l.contains(what)) + 1;

      group('compiled together, is refused', () {
        void refusedTogether(Map<String, String> files, String entry,
            String why) {
          files.forEach(write);
          final path = '${dir.path}/$entry';
          expect(
              () => compileVglpSource(File(path).readAsStringSync(),
                  path: path),
              throwsA(isA<CompileError>()
                  .having((e) => e.message, 'message', contains(why))));
        }

        // Refused with [why], at [line] and [column] of [entry].
        void refusedAt(Map<String, String> files, String entry, String why,
            int line, int column) {
          files.forEach(write);
          final path = '${dir.path}/$entry';
          expect(
              () => compileVglpSource(File(path).readAsStringSync(),
                  path: path),
              throwsA(isA<CompileError>()
                  .having((e) => e.message, 'message', contains(why))
                  .having((e) => (e.line, e.column), 'line and column',
                      (line, column))));
        }

        test('one type joining from modules outside them with two functors',
            () {
          // one gives Box_yesNo? box_yesNo_r; two, which declares its own of
          // the type before it imports boxes' pick, box_yesNo_r_2.
          refusedTogether({
            'boxes.vglp': boxes,
            'one.vglp': '''
$types
exported procedure (Box_yesNo?)*one(Peer?, YesNo).
(box_yn(A))*one(P, A?) :- ground(P?) | true.
''',
            'two.vglp': '''
$types
exported procedure (Box_yesNo?)*two(Peer?, YesNo).
(box_yn(A))*two(P, A?) :- ground(P?) | true.
imported procedure (Box(YesNo)?)*boxes#pick(Peer?, YesNo).
''',
            'a.vglp': a('imported procedure (Box_yesNo?)*one#one('
                'Peer?, YesNo).\n'),
            'b.vglp': b('imported procedure (Box_yesNo?)*two#two('
                'Peer?, YesNo).\n'),
          }, 'a.vglp',
              'joins the questions of the modules compiled together, a, b, '
              'with the functor box_yesNo_r from one and box_yesNo_r_2 '
              'from two');
        });

        // "the module first in import order keeps the functor and the
        // other takes _2" (vGLP #8 Cowork, 2026-10-11 16:44 UTC, Q1), the
        // module first in import order "the one the program's entry imports
        // first": where there is no entry beside them, or one importing none
        // of them, the order is undecided and the two are refused, at the
        // declaration of the second.  Until then the two were refused
        // whatever the entry (the next group).
        test('two types of two of the modules with one functor, and no '
            'self.glp beside them, the program\'s entry: their import order '
            'is undecided', () {
          refusedAt(
              {'a.vglp': pick, 'b.vglp': confirm},
              'b.vglp',
              'The interactive types Box(YesNo)? of a and Box_yesNo? of b, '
              'of the modules compiled together, a, b, both have the '
              'functor box_yesNo_r: "the module first in import order keeps '
              'the functor and the other takes _2" (vGLP #8 Cowork, '
              '2026-10-11 16:44 UTC, Q1), and their import order is '
              'undecided: there is no self.glp beside them, the program\'s '
              'entry',
              lineOf(confirm, '*confirm('),
              1);
        });

        test('the same, the self.glp beside them importing none of them',
            () {
          refusedAt(
              {
                'a.vglp': pick,
                'b.vglp': confirm,
                'self.glp': 'exported procedure go(Integer).\ngo(1).\n',
              },
              'a.vglp',
              'and their import order is undecided: the self.glp beside them, '
              'the program\'s entry, imports none of them, directly or '
              'through the modules beside it',
              lineOf(pick, 'b#jot_b('),
              1);
        });

        test('one of them refused refuses the others', () {
          refusedTogether({
            'a.vglp': a('procedure go(Note).\ngo(N?) :- jot_a(N, _).\n'),
            'b.vglp': b(''),
          }, 'b.vglp',
              'The module a, compiled together with this one');
        });
      });

      // vGLP #8 Cowork, 2026-10-11 16:44 UTC (Udi approved), answering Code
      // #7's 16:19 UTC: "Q1: yes, the module first in import order keeps the
      // functor and the other takes _2.  Q2: yes, the module that defines a
      // type declares its widget, and the others declare none for it."
      // Until then both were refused (the group above).
      group('compiled together, by the rulings of vGLP #8 Cowork, 2026-10-11 '
          '16:44 UTC', () {
        String compiled(String name) {
          final path = '${dir.path}/$name';
          return compileVglpSource(File(path).readAsStringSync(), path: path);
        }

        // The questions of a and of b, each one union.
        void questions(String expected) {
          for (final m in ['a.vglp', 'b.vglp']) {
            expect(compiled(m), contains('Question ::= $expected.'),
                reason: m);
          }
        }

        test('(Q1) two types of two of them with one functor: the module the '
            'self.glp beside them, the program\'s entry, imports first keeps '
            'it, and the other takes _2', () {
          write('a.vglp', pick);
          write('b.vglp', confirm);
          // The entry imports a's pick first.
          write('self.glp', '''
$types
imported procedure a#pick(Peer?, YesNo, Stream(_)).
imported procedure b#confirm(Peer?, YesNo, Stream(_)).
''');
          questions('note_r(Note?) ; box_yesNo_r(Box(YesNo)?) ; '
              'box_yesNo_r_2(Box_yesNo?)');
          expect(compiled('a.vglp'),
              contains("[ask('Box(YesNo)?', box_yesNo_r(X)) | D?]"));
          expect(compiled('b.vglp'),
              contains("[ask('Box_yesNo?', box_yesNo_r_2(X)) | D?]"));
          // The entry imports b's confirm first.
          write('self.glp', '''
$types
imported procedure b#confirm(Peer?, YesNo, Stream(_)).
imported procedure a#pick(Peer?, YesNo, Stream(_)).
''');
          questions('note_r(Note?) ; box_yesNo_r_2(Box(YesNo)?) ; '
              'box_yesNo_r(Box_yesNo?)');
          expect(compiled('a.vglp'),
              contains("[ask('Box(YesNo)?', box_yesNo_r_2(X)) | D?]"));
          expect(compiled('b.vglp'),
              contains("[ask('Box_yesNo?', box_yesNo_r(X)) | D?]"));
        });

        test('(Q1) the same, the entry importing b first through a module '
            'beside them, before it imports a', () {
          write('a.vglp', pick);
          write('b.vglp', confirm);
          write('home.vglp', '''
$types
imported procedure (Box_yesNo?)*b#confirm(Peer?, YesNo).
exported procedure go(Peer?, YesNo).
go(P, A?) :- ground(P?) | b # confirm(P?, A).
''');
          write('self.glp', '''
$types
imported procedure home#go(Peer?, YesNo, Stream(_)).
imported procedure a#pick(Peer?, YesNo, Stream(_)).
''');
          questions('note_r(Note?) ; box_yesNo_r_2(Box(YesNo)?) ; '
              'box_yesNo_r(Box_yesNo?)');
          // home, beside them, has their questions as b's export gives
          // them.
          expect(
              compiled('home.vglp'),
              contains('Question ::= note_r(Note?) ; '
                  'box_yesNo_r_2(Box(YesNo)?) ; box_yesNo_r(Box_yesNo?).'));
        });

        test('(Q2) a widget declaration of one of them for a type it defines '
            'gives the type its widget in each of them, another that '
            'defines it too declaring none', () {
          // a and b both define Note?, jot_a and jot_b; a declares its
          // widget.
          final mediator = MediatorSource.fromDirectory('$_programs/vglp');
          final scope = rootScope(File('$_programs/self.glp').absolute.path);
          write('a.vglp', a('Note? =::= note_pad.\n'));
          write('b.vglp', b(''));
          for (final m in ['a.vglp', 'b.vglp']) {
            final path = '${dir.path}/$m';
            final s = compileVglpSource(File(path).readAsStringSync(),
                mediator: mediator, scope: scope, path: path);
            expect(
                s,
                contains('construct(Id, note_r(X?), Gs, Ds?) :- '
                    'answer_note([], X, Gs?, _, Done), '
                    'run(Id?, note_pad, [input([])], Done?, Ds).'),
                reason: m);
          }
        });
      });

      test('a type of theirs is told apart from a functor imported from '
          'outside them, and a type imported from outside them is one of '
          'theirs too', () {
        // one gives Box_yesNo? box_yesNo_r; a's own Box(YesNo)? is told
        // apart, box_yesNo_r_2, in a and in b.
        write('one.vglp', '''
$types
exported procedure (Box_yesNo?)*one(Peer?, YesNo).
(box_yn(A))*one(P, A?) :- ground(P?) | true.
''');
        const a = '''
$types
imported procedure (Note?)*b#jot_b(Note).
exported procedure (Box(YesNo)?)*pick(Peer?, YesNo).
(box(A))*pick(P, A?) :- ground(P?) | true.
''';
        const b = '''
$types
imported procedure (Box(YesNo)?)*a#pick(Peer?, YesNo).
imported procedure (Box_yesNo?)*one#one(Peer?, YesNo).
exported procedure (Note?)*jot_b(Note).
(N)*jot_b(N?).
''';
        final pa = write('a.vglp', a), pb = write('b.vglp', b);
        expect(
            compileVglpSource(a, path: pa),
            contains('Question ::= box_yesNo_r_2(Box(YesNo)?) ; '
                'box_yesNo_r(Box_yesNo?) ; note_r(Note?).'));
        expect(
            compileVglpSource(b, path: pb),
            contains('Question ::= box_yesNo_r_2(Box(YesNo)?) ; '
                'box_yesNo_r(Box_yesNo?) ; note_r(Note?).'));
      });
    });
  });

  group('(iii) the old syntax keeps its old compilation', () {
    final sources = _vglpSources(Directory(_programs));

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

    // Until 2026-10-12 the old source was the one named responder.vglp,
    // IGLP's one-clause fixture, which is now in the paper's syntax (GLP,
    // 2026-10-11 12:59 UTC, "03:24"); each old source is now found by what
    // it is, as above.
    test('an old source with no mediator to compile against is refused, not '
        'compiled by the new compilation', () {
      for (final f in sources) {
        final text = f.readAsStringSync();
        if (!_holdsVolitionGuard(text)) continue;
        expect(() => compileVglpSource(text), throwsA(isA<StateError>()),
            reason: f.path);
      }
    });
  });
}
