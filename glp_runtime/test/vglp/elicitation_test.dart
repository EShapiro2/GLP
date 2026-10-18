// glp_runtime/test/vglp/elicitation_test.dart
//
// The interactive runtime with a scripted person: a vGLP program compiled on
// load, its compiled initial goal spawned with the dispatcher on its ask
// stream and on a person channel, and a person procedure that reads the draws
// and grants input by construct identifier.
// Spec: vGLP at c994328 --- sections/elicitation.tex, from "GLP already
// connects the program to the person" to Definition "Implementation of vGLP
// by GLP"; Definition "Construct, Submission, Complete Widget"; Remark
// "Persistence"; sections/vglp.tex, Definition "vmaGLP Transition System"
// (Present: every output before the inputs inside it), and the agent of
// Sections 1 and 3, whose question is the stream of the person's requests.
// vGLP's code task of 2026-10-01 16:30 UTC, tests (iv) and (v); the messages
// of 2026-10-01 23:55 UTC (E); vGLP #5 Cowork's task of 2026-10-03 08:16 UTC,
// with (_) and the handle out of the language (Udi, 2026-10-03, as vGLP
// reports it): (v)'s second half, once a construct withdrawing on its handle,
// is the agent's message clause reducing without a request.  The person
// channel never closes (vGLP at 7838827, Definition "Person Channel, Person
// Writer, GLP with Persons, Grant"; vGLP #5 Cowork, 2026-10-03 21:13 UTC, Q1):
// the dispatcher never terminates, the scripted person holds its writer for
// good and goes on watching, so every log stays open; a withdrawn construct's
// grant writer goes to Deads, read here from the dispatcher's last suspension
// in the trace.  The grant names its question (vGLP #5 Cowork, 2026-10-04
// 09:05 UTC, C): a grant is input(Id, P, R), P the path of argument indices
// from the root of the interactive variable's term to the position the
// person writes, [] the root, a stream the person writes having the
// stream's own path; the view marks each such position input(P), and the
// scripted person sends back the P it reads there.
// Programs: programs/tests/vglp/fragments (questions.vglp, the fragments of
// Sections 1 and 3, and its plays), programs/tests/vglp/stream (chat.vglp,
// the chat input of Section 3, and its play) and programs/tests/vglp/cards
// (cards.vglp, a stream and a list the program writes whose elements hold
// questions, and its plays; Task D) and programs/tests/vglp/pending
// (pending.vglp, positions the program writes after the ask, drawn pending,
// and its plays) and programs/tests/vglp/recursive (speech.vglp, recursive
// interactive types with no mode inversion, written whole by the person or
// by the program, and its plays); the Real question and the card of two
// questions are written under programs/tests/vglp/ for their runs.

import 'dart:async';
import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart' show emitVglpSources;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
String _program(String name) =>
    Directory('../programs/tests/vglp/$name').absolute.path;

/// A run of [goal] in [program], and its bindings as GLP text.
class _Run {
  final ExecutionResult result;
  final GlpEngine engine;
  _Run(this.result, this.engine);

  ExecutionStatus get status => result.status;
  String? get error => result.error;

  /// The binding of [name], dereferenced through the heap; an unbound
  /// variable, at the top or inside, shown `_`.
  String operator [](String name) => _show(result.bindings[name]);

  String _show(rt.Term? t) {
    if (t == null) return '_';
    if (t is rt.VarRef) {
      final d = engine.runtime.heap.dereference(t);
      return d is rt.VarRef ? '_' : _show(d);
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
            items.add(_show(c.args[0]));
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
          return '[${items.join(', ')} | ${_show(c)}]';
        }
      }
      return '${t.functor}(${t.args.map(_show).join(', ')})';
    }
    return '$t';
  }
}

Future<_Run> _play(String program, String goal) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)
    ..loadProgram(_program(program));
  return _Run(await engine.runGoal(goal), engine);
}

/// A traced run of [goal] in [program], and the dispatcher's serve/7 as it
/// last suspended: its arguments as the trace prints them.
Future<(_Run, List<String>)> _servedPlay(String program, String goal) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)
    ..loadProgram(_program(program));
  engine.debugTrace = true;
  final lines = <String>[];
  final result = await runZoned(() => engine.runGoal(goal),
      zoneSpecification: ZoneSpecification(
          print: (self, parent, zone, line) => lines.add(line)));
  List<String>? serve;
  final call = RegExp(r'(?:^|:)serve(?:_\d+)?\(');
  for (final l in lines) {
    if (!l.endsWith(' → suspended')) continue;
    final m = call.firstMatch(l);
    if (m == null) continue;
    final args = _args(l.substring(m.end - 1, l.length - ' → suspended'.length));
    if (args.length == 7) serve = args;
  }
  expect(serve, isNotNull, reason: 'the dispatcher\'s serve/7 never suspended');
  return (_Run(result, engine), serve!);
}

/// The top-level arguments of a printed call's argument list, `(a, b, ...)`.
List<String> _args(String parens) => _split(parens.substring(1, parens.length - 1), ',');

/// [s] split at [sep] where no bracket is open.
List<String> _split(String s, String sep) {
  final out = <String>[];
  var depth = 0, from = 0;
  for (var i = 0; i < s.length; i++) {
    final c = s[i];
    if (c == '(' || c == '[') depth++;
    if (c == ')' || c == ']') depth--;
    if (depth == 0 && c == sep) {
      out.add(s.substring(from, i).trim());
      from = i + 1;
    }
  }
  out.add(s.substring(from).trim());
  return out;
}

/// The elements of a printed proper list, `[a | [b | []]]` or `[a, b]`.
List<String> _elements(String list) {
  if (list == '[]') return const [];
  final inner = list.substring(1, list.length - 1);
  final bar = _split(inner, '|');
  final items = _split(bar.first, ',');
  return bar.length == 1 ? items : [...items, ..._elements(bar[1])];
}

void main() {
  group('(iv) Section 3\'s responder', () {
    test('the card shows the offering peer before any input, and one grant of '
        'yes completes it', () async {
      final r = await _play('fragments', 'play_card(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // The person granted yes on seeing the card drawn with bob in it and
      // its own position input; the responder took the answer.
      expect(r['Resp'], 'accept(bob)');
      expect(
          r['Log'],
          '[drawn(1, form(card, [field(Peer, shown), field(YesNo, '
          'buttons([yes, no]))]), card(bob, input([2]))), withdrawn(1) | _]');
    });

    test('a grant that forms no term of the question\'s type answers nothing, '
        'and the card stays', () async {
      final r = await _play('fragments', 'play_card_refused(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Resp'], '_');
      expect(r['Log'], '[drawn(1, card(bob, input([2]))) | _]');
    });

    test('a refused grant leaves its question open, and the next grant that '
        'forms a term of its type answers it', () async {
      final r =
          await _play('fragments', 'play_card_refused_then_yes(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // maybe answered nothing; yes, the next grant, answered the card.
      expect(r['Resp'], 'accept(bob)');
      expect(r['Log'], '[drawn(1, card(bob, input([2]))), withdrawn(1) | _]');
    });
  });

  group('(v) a reader-mode stream question, and the agent\'s message clause',
      () {
    test('a stream question takes one element per submission, a submission '
        'of no element of its type taking none', () async {
      final r = await _play('stream', 'play_chat(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'],
          '[msg(bob, hello), msg(bob, world) | _]');
      // One construct, drawn once, an input box that stays open, the
      // stream's own position the root, [].
      expect(r['Log'], '[drawn(1, input_box(text), input([])) | _]');
      expect(r['Log'], isNot(contains('withdrawn')));
    });

    test('a stream question takes two grants as two elements and stays open',
        () async {
      final r = await _play('stream', 'play_chat_two(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'], '[msg(carol, one), msg(carol, two) | _]');
      expect(r['Log'], '[drawn(1, input_box(text), input([])) | _]');
    });

    test('the agent\'s message clause reads no request: it reduces on an '
        'arriving friend offer without waiting, the card of the offer is '
        'drawn, and the request form stays on screen', () async {
      final r = await _play('fragments', 'play_offer(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      final log = r['Log'];
      expect(
          log,
          contains('drawn(1, input_box(menu([form(post, [field(String, '
              'text)]), button(quit)])), input([]))'));
      expect(log,
          contains('drawn(2, form(card, [field(Peer, shown), field(YesNo, '
              'buttons([yes, no]))]), card(carol, input([2])))'));
      expect(log, isNot(contains('withdrawn')));
    });
  });

  group('the agent\'s question, a stream of requests asked once', () {
    test('the person posts a text, a post of a number refused on the way, and '
        'quits, all on one construct, drawn once and kept open', () async {
      final r = await _play('fragments', 'play_request(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Outs'], '[hi]');
      expect(r['Log'],
          '[drawn(1, input_box(menu([form(post, [field(String, text)]), '
          'button(quit)])), input([])) | _]');
    });
  });

  group('the person channel never closes (Q1)', () {
    test('a construct withdrawn while a grant for it is in flight: the grant '
        'reaches no construct, and the next construct\'s grants are '
        'unaffected', () async {
      final (r, serve) =
          await _servedPlay('fragments', 'play_inflight(R1, R2, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // bob's card took yes, and the stale no after its withdrawal took
      // nothing; carol's card took its own no.
      expect(r['R1'], 'accept(bob)');
      expect(r['R2'], 'refuse(carol)');
      for (final e in ['drawn(1)', 'withdrawn(1)', 'drawn(2)', 'withdrawn(2)']) {
        expect(r['Log'], contains(e));
      }
      expect(r['Log'], endsWith('| _]'));
      // No live route is left, and Deads holds one grant writer per
      // withdrawn construct, neither written after its withdrawal: the stale
      // grant reached no construct.
      expect(serve[3], '[]');
      final deads = _elements(serve[4]);
      expect(deads, hasLength(2), reason: serve[4]);
      for (final d in deads) {
        expect(d, matches(RegExp(r'^X\d+$')), reason: serve[4]);
      }
    });

    test('after a play, Deads holds one entry per withdrawn construct', () async {
      // One card, answered and withdrawn.
      final (card, s1) =
          await _servedPlay('fragments', 'play_card(Resp, Log)');
      expect(card['Resp'], 'accept(bob)');
      expect(_elements(s1[4]), hasLength(1), reason: s1[4]);
      expect(s1[3], '[]');
      // A card refused and so never withdrawn: its route live, Deads empty.
      final (refused, s2) =
          await _servedPlay('fragments', 'play_card_refused(Resp, Log)');
      expect(refused['Resp'], '_');
      expect(s2[4], '[]');
      expect(_elements(s2[3]), hasLength(1), reason: s2[3]);
      // The request form, a stream question, and a card nobody answers: two
      // live routes, none dead.
      final (offer, s3) =
          await _servedPlay('fragments', 'play_offer(Outs, Log)');
      expect(offer.status, isNot(ExecutionStatus.failed));
      expect(s3[4], '[]');
      expect(_elements(s3[3]), hasLength(2), reason: s3[3]);
    });
  });

  group('the dispatcher takes asks, grants and draws in turn (vGLP '
      '2026-10-14 23:05 UTC)', () {
    // "The dispatcher takes asks, grants and draws in turn, so a grant is
    // routed and a draw is passed on however many asks arrive" (vGLP,
    // Section 6.3): fair/5 of programs/vglp/dispatcher.glp, which merges the
    // three into the one stream serve/7 reads, takes the input whose turn it
    // is, else the next after it that has come.  Asks keep arriving, the
    // stream's tail open, a grant and a draw and a withdrawal waiting.
    test('with asks arriving without end, the grant and the draws are taken '
        'between them, not after them', () async {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(
          engine.loadFile(
              File('../programs/vglp/dispatcher.glp').absolute.path),
          isTrue);
      final r = _Run(
          await engine.runGoal('fair(ask, [ask(a, 1), ask(a, 2), ask(a, 3), '
              'ask(a, 4), ask(a, 5) | As?], [input(1, [], yes) | Is?], '
              '[draw(1, w, v), withdraw(1) | Ds?], Es)'),
          engine);
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(
          r['Es'],
          '[ask(a, 1), input(1, [], yes), draw(1, w, v), ask(a, 2), '
          'withdraw(1), ask(a, 3), ask(a, 4), ask(a, 5) | _]');
    });
  });

  group('a question the person writes of type Real (item 6)', () {
    // vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 6: formed by real/1, its
    // default widget the number field (Definition "Widget Declaration,
    // Default Widget" at c2e8b57, "a number (Integer or Real)").  The program
    // is written under programs/tests/vglp/ for the run and removed after, a
    // program having to live under programs/ for its scope.
    late Directory fixture;
    setUp(() {
      fixture = Directory('../programs/tests/vglp/real_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}')
        ..createSync();
      File('${fixture.path}/quote.vglp').writeAsStringSync('''
Price ::= price(Real).
exported procedure (Price?)*quote(Price).
(P)*quote(P?).
''');
      File('${fixture.path}/self.glp').writeAsStringSync('''
Price    ::= price(Real).
Question ::= price_r(Price?).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= price_r(_) ; Price.

imported procedure quote#quote(Price, Stream(Ask(Question))).
imported procedure quote#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

exported procedure play_quote(Price, Stream(_)).
play_quote(P?, Log?) :-
    quote # quote(P, Asks),
    quote # dispatch(Asks?, ch(Gs?, Ds), _),
    pricing_person(Ds?, Gs, Log).

procedure pricing_person(_?, PersonIn(Drawn), Stream(_)).
pricing_person([draw(Id, W, price_r(input(Q))) | Ds],
               [input(Id?, Q?, price_r(3)), input(Id?, Q?, price(2.5)) | Gs?],
               [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    pricing_person(Ds?, Gs, Log).
pricing_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    pricing_person(Ds?, Gs, Log).
''');
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    // The grant's raw input is of the person channel's type, the program's
    // draws and its questions' types (vGLP, Definition "Construct
    // Process"), so price(3), whose argument is no Real, is not granted from
    // GLP: the refused grant is price_r(3), of that type and no Price.
    test('a grant that forms no Price is refused; a grant of a '
        'real answers it; the construct is a number field', () async {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(fixture.absolute.path), isTrue);
      final r = _Run(await engine.runGoal('play_quote(P, Log)'), engine);
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['P'], 'price(2.5)');
      expect(r['Log'],
          '[drawn(1, form(price, [field(Real, number)]), input([])), '
          'withdrawn(1) | _]');
    });
  });

  group('a functor that is not a name (vGLP #6 Cowork, 2026-10-11 12:55 UTC, '
      'Task A)', () {
    // Pick ::= 'odd one'(Choice): the join procedure of its alternative is
    // named by the functor's characters made a name, join_pick_odd_one_1, so
    // the module :emit writes reads back, and runs.  The program is written
    // under programs/tests/vglp/ for the run and removed after, as the Real
    // question's is; its module is emitted beside the source, which the load
    // then skips for the module (vGLP CLAUDE.md, "The REPL loads them").
    late Directory fixture;
    setUp(() {
      fixture = Directory('../programs/tests/vglp/odd_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}')
        ..createSync();
      File('${fixture.path}/pick.vglp').writeAsStringSync('''
Choice ::= a ; b.
Pick ::= 'odd one'(Choice).
exported procedure (Pick?)*pick(Pick).
(P)*pick(P?).
''');
      File('${fixture.path}/self.glp').writeAsStringSync('''
Choice   ::= a ; b.
Pick     ::= 'odd one'(Choice).
Question ::= pick_r(Pick?).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= pick_r(_) ; Pick.

imported procedure pick#pick(Pick, Stream(Ask(Question))).
imported procedure pick#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

exported procedure play_pick(Pick, Stream(_)).
play_pick(P?, Log?) :-
    pick # pick(P, Asks),
    pick # dispatch(Asks?, ch(Gs?, Ds), _),
    picking_person(Ds?, Gs, Log).

procedure picking_person(_?, PersonIn(Drawn), Stream(_)).
picking_person([draw(Id, W, pick_r(input(Q))) | Ds],
               [input(Id?, Q?, 'odd one'(b)) | Gs?],
               [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    picking_person(Ds?, Gs, Log).
picking_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    picking_person(Ds?, Gs, Log).
''');
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    test('its emitted module reads back and runs: the person\'s grant forms '
        "'odd one'(b)", () async {
      final written =
          emitVglpSources(fixture.absolute.path, rootSelfGlpPath: _root);
      final emitted = File('${fixture.absolute.path}/pick.glp');
      expect(written, [emitted.path]);
      final text = emitted.readAsStringSync();
      expect(text, contains('join_pick_odd_one_1('));
      expect(text, isNot(contains('join_pick_odd one')));
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(fixture.absolute.path), isTrue);
      final r = _Run(await engine.runGoal('play_pick(P, Log)'), engine);
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['P'], 'odd one(b)');
      expect(r['Log'], startsWith('[drawn(1, '));
      expect(r['Log'], endsWith(', input([])), withdrawn(1) | _]'));
    });
  });

  group('Task B (vGLP #6 Cowork, 2026-10-11 12:55 UTC): lists, streams in '
      'forms, keys', () {
    // Definition "Widget Declaration, Default Widget" and the Answer rule of
    // Definition "vmaGLP Transition System" (vGLP 90988ea): a list the person
    // writes is ended by the person, [] its answer, on the grant of its end
    // button, [end], a term no element can be (vGLP #7 Cowork, 2026-10-11
    // 14:31 UTC, item 3), and a stream is not; an alias of a stream draws as
    // the stream it names (item 2); a
    // stream or a list inside a form the person writes is a question of its
    // own, the form's answer carrying a fresh variable there, drawn under the
    // form; a Key is a peer field.  The program is written under
    // programs/tests/vglp/ for the run and removed after, as the Real
    // question's is.
    late Directory fixture;
    setUp(() {
      fixture = Directory('../programs/tests/vglp/taskb_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}')
        ..createSync();
      const types = '''
List(X) ::= [] ; [X | List(X)].
Post    ::= post(String, Stream(String)) ; tags(List(Constant)) ; quit.
Peer    ::= Constant.
Reply   ::= reply(String, Stream(String)).
Card    ::= card(Peer, Reply?).
Issuer  ::= Key.
Lot     ::= lot(Issuer, Integer).
Opt     ::= none ; [].
Lines   ::= Stream(String).
''';
      File('${fixture.path}/b.vglp').writeAsStringSync('''
$types
exported procedure (List(Integer)?)*numbers(List(Integer)).
(Ns)*numbers(Ns?).

exported procedure (Stream(Integer)?)*counts(Stream(Integer)).
(Cs)*counts(Cs?).

exported procedure (Post?)*poster(Post).
(P)*poster(P?).

exported procedure (Card)*respond(Reply).
(card(bob, R))*respond(R?).

exported procedure (Lot?)*lot(Lot).
(L)*lot(L?).

exported procedure (List(Opt)?)*opts(List(Opt)).
(Os)*opts(Os?).

exported procedure (Lines?)*write(Lines).
(Ls)*write(Ls?).
''');
      File('${fixture.path}/self.glp').writeAsStringSync('''
$types
Question ::= list_integer_r(List(Integer)?) ; stream_integer_r(Stream(Integer)?)
           ; post_r(Post?) ; card_w(Card) ; lot_r(Lot?) ; list_opt_r(List(Opt)?)
           ; lines_r(Lines?).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= list_integer_r(_) ; stream_integer_r(_) ; post_r(_) ; card_w(_)
           ; lot_r(_) ; list_opt_r(_) ; lines_r(_) ; Post ; Constant ; Reply
           ; Lot ; Opt ; [DrawnEnd | DrawnNil].
DrawnEnd ::= end.
DrawnNil ::= [].

imported procedure b#numbers(List(Integer), Stream(Ask(Question))).
imported procedure b#counts(Stream(Integer), Stream(Ask(Question))).
imported procedure b#poster(Post, Stream(Ask(Question))).
imported procedure b#respond(Reply, Stream(Ask(Question))).
imported procedure b#lot(Lot, Stream(Ask(Question))).
imported procedure b#opts(List(Opt), Stream(Ask(Question))).
imported procedure b#write(Lines, Stream(Ask(Question))).
imported procedure b#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

%% A list: two numbers, then its end, [end].
exported procedure play_numbers(List(Integer), Stream(_)).
play_numbers(Ns?, Log?) :-
    b # numbers(Ns, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    numbers_person(Ds?, Gs, Log).

procedure numbers_person(_?, PersonIn(Drawn), Stream(_)).
numbers_person([draw(Id, W, list_integer_r(input(Q))) | Ds],
               [input(Id?, Q?, 1), input(Id?, Q?, 2), input(Id?, Q?, [end]) | Gs?],
               [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    numbers_person(Ds?, Gs, Log).
numbers_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    numbers_person(Ds?, Gs, Log).

%% A stream: a number, [end], [], and a number.
exported procedure play_counts(Stream(Integer), Stream(_)).
play_counts(Cs?, Log?) :-
    b # counts(Cs, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    counts_person(Ds?, Gs, Log).

procedure counts_person(_?, PersonIn(Drawn), Stream(_)).
counts_person([draw(Id, W, stream_integer_r(input(Q))) | Ds],
              [input(Id?, Q?, 1), input(Id?, Q?, [end]), input(Id?, Q?, []),
               input(Id?, Q?, 2) | Gs?],
              [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    counts_person(Ds?, Gs, Log).
counts_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    counts_person(Ds?, Gs, Log).

%% A post: the form with [] at its stream, then two lines into the stream
%% at the position the redrawn view marks.
exported procedure play_post(Post, Stream(_)).
play_post(P?, Log?) :-
    b # poster(P, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    post_person(Ds?, Gs, Log).

procedure post_person(_?, PersonIn(Drawn), Stream(_)).
post_person([draw(Id, W, post_r(input(Q))) | Ds],
            [input(Id?, Q?, post(hi, [])) | Gs?],
            [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    post_person(Ds?, Gs, Log).
post_person([draw(Id, _, post_r(post(S, input(Q)))) | Ds],
            [input(Id?, Q?, x), input(Id?, Q?, y) | Gs?],
            [drawn(Id?, post(S?, input(Q?))) | Log?]) :-
    ground(Id?), ground(S?), ground(Q?) |
    post_person(Ds?, Gs, Log).
post_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    post_person(Ds?, Gs, Log).

%% Tags: the form with [] at its list, then a tag and the list's end.
exported procedure play_tags(Post, Stream(_)).
play_tags(P?, Log?) :-
    b # poster(P, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    tags_person(Ds?, Gs, Log).

procedure tags_person(_?, PersonIn(Drawn), Stream(_)).
tags_person([draw(Id, _, post_r(input(Q))) | Ds],
            [input(Id?, Q?, tags([])) | Gs?],
            [drawn(Id?, input(Q?)) | Log?]) :-
    ground(Id?), ground(Q?) |
    tags_person(Ds?, Gs, Log).
tags_person([draw(Id, _, post_r(tags(input(Q)))) | Ds],
            [input(Id?, Q?, a), input(Id?, Q?, [end]) | Gs?],
            [drawn(Id?, tags(input(Q?))) | Log?]) :-
    ground(Id?), ground(Q?) |
    tags_person(Ds?, Gs, Log).
tags_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    tags_person(Ds?, Gs, Log).

%% A card the program writes holding the person's reply, which holds a
%% stream: the reply, then a line into its stream.
exported procedure play_respond(Reply, Stream(_)).
play_respond(R?, Log?) :-
    b # respond(R, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    respond_person(Ds?, Gs, Log).

procedure respond_person(_?, PersonIn(Drawn), Stream(_)).
respond_person([draw(Id, _, card_w(card(F, input(Q)))) | Ds],
               [input(Id?, Q?, reply(hi, [])) | Gs?],
               [drawn(Id?, card(F?, input(Q?))) | Log?]) :-
    ground(Id?), ground(F?), ground(Q?) |
    respond_person(Ds?, Gs, Log).
respond_person([draw(Id, _, card_w(card(F, reply(S, input(Q))))) | Ds],
               [input(Id?, Q?, x) | Gs?],
               [drawn(Id?, card(F?, reply(S?, input(Q?)))) | Log?]) :-
    ground(Id?), ground(F?), ground(S?), ground(Q?) |
    respond_person(Ds?, Gs, Log).
respond_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    respond_person(Ds?, Gs, Log).

%% A lot: its issuer a key.
exported procedure play_lot(Lot, Stream(_)).
play_lot(L?, Log?) :-
    b # lot(L, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    lot_person(Ds?, Gs, Log).

procedure lot_person(_?, PersonIn(Drawn), Stream(_)).
lot_person([draw(Id, W, lot_r(input(Q))) | Ds],
           [input(Id?, Q?, lot('0a1b', 5)) | Gs?],
           [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    lot_person(Ds?, Gs, Log).
lot_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    lot_person(Ds?, Gs, Log).

%% A list whose element type has [], a constant: none, [], then its end.
exported procedure play_opts(List(Opt), Stream(_)).
play_opts(Os?, Log?) :-
    b # opts(Os, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    opts_person(Ds?, Gs, Log).

procedure opts_person(_?, PersonIn(Drawn), Stream(_)).
opts_person([draw(Id, W, list_opt_r(input(Q))) | Ds],
            [input(Id?, Q?, none), input(Id?, Q?, []), input(Id?, Q?, [end]) | Gs?],
            [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    opts_person(Ds?, Gs, Log).
opts_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    opts_person(Ds?, Gs, Log).

%% An alias of a stream: two lines, and [end] answering nothing.
exported procedure play_write(Lines, Stream(_)).
play_write(Ls?, Log?) :-
    b # write(Ls, Asks), b # dispatch(Asks?, ch(Gs?, Ds), _),
    write_person(Ds?, Gs, Log).

procedure write_person(_?, PersonIn(Drawn), Stream(_)).
write_person([draw(Id, W, lines_r(input(Q))) | Ds],
             [input(Id?, Q?, one), input(Id?, Q?, [end]), input(Id?, Q?, two) | Gs?],
             [drawn(Id?, W?, input(Q?)) | Log?]) :-
    ground(Id?), ground(W?), ground(Q?) |
    write_person(Ds?, Gs, Log).
write_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    write_person(Ds?, Gs, Log).
''');
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    Future<_Run> play(String goal) async {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(fixture.absolute.path), isTrue);
      return _Run(await engine.runGoal(goal), engine);
    }

    test('(1) a list the person writes takes one element per grant and ends '
        'on the grant of its end button, [end], and its construct withdraws',
        () async {
      final r = await play('play_numbers(Ns, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Ns'], '[1, 2]');
      expect(r['Log'],
          '[drawn(1, input_list(number), input([])), withdrawn(1) | _]');
    });

    test('(1) a stream the person writes is not ended: a grant of [end] or '
        'of [] answers nothing, and the stream stays open', () async {
      final r = await play('play_counts(Cs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Cs'], '[1, 2 | _]');
      expect(r['Log'], '[drawn(1, input_box(number), input([])) | _]');
    });

    test('(1) the end\'s grant is no element\'s: a list whose element type '
        'has [], a constant, takes [] as an element and ends on [end] (vGLP #7 '
        'Cowork, 2026-10-11 14:31 UTC, item 3)', () async {
      final r = await play('play_opts(Os, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Os'], '[none, []]');
      expect(r['Log'],
          '[drawn(1, input_list(menu([button(none), button([])])), '
          'input([])), withdrawn(1) | _]');
    });

    test('an alias of a stream draws as the stream it names and is answered '
        'as it (vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 2)', () async {
      final r = await play('play_write(Ls, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Ls'], '[one, two | _]');
      expect(r['Log'], '[drawn(1, input_box(text), input([])) | _]');
    });

    test('(3) a stream inside a form: the form answered with a fresh stream, '
        'drawn again with the answer, the stream taking its lines at its own '
        'position, and the construct staying', () async {
      final r = await play('play_post(P, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['P'], 'post(hi, [x, y | _])');
      expect(
          r['Log'],
          "[drawn(1, menu([form(post, [field(String, text), "
          "question(Stream(String), input_box(text))]), form(tags, "
          "[question(List(Constant), input_list(text))]), button(quit)]), "
          'input([])), drawn(1, post(hi, input([2]))) | _]');
    });

    test('(3) a list inside a form: answered, then ended, and the construct '
        'withdraws', () async {
      final r = await play('play_tags(P, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['P'], 'tags([a])');
      expect(r['Log'],
          '[drawn(1, input([])), drawn(1, tags(input([1]))), withdrawn(1) | _]');
    });

    test('(3) in a question of a structure the program writes: the card drawn '
        'again with the reply, its stream at the path down to it', () async {
      final r = await play('play_respond(R, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['R'], 'reply(hi, [x | _])');
      expect(
          r['Log'],
          '[drawn(1, card(bob, input([2]))), '
          'drawn(1, card(bob, reply(hi, input([2, 2])))) | _]');
    });

    test('(5) and (6) a key is a peer field, each field labelled by its '
        'declared type\'s name, and the lot formed', () async {
      final r = await play('play_lot(L, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['L'], 'lot(0a1b, 5)');
      expect(
          r['Log'],
          '[drawn(1, form(lot, [field(Issuer, peer), field(Integer, '
          'number)]), input([])), withdrawn(1) | _]');
    });
  });

  group('the grant names its question (C)', () {
    // vGLP #5 Cowork, 2026-10-04 09:05 UTC, C: a construct may hold two
    // questions of one type, so a grant is input(Id, P, R), P the question's
    // position, and answer_N guards on its own P, a grant for another P going
    // on.  The card here holds two questions of one type, at [2] and [3].
    // The program is written under programs/tests/vglp/ for the run and
    // removed after, as the Real question's is.
    late Directory fixture;
    setUp(() {
      fixture = Directory('../programs/tests/vglp/two_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}')
        ..createSync();
      File('${fixture.path}/two.vglp').writeAsStringSync('''
Peer  ::= Constant.
YesNo ::= yes ; no.
Offer ::= offer(Peer).
Both  ::= both(Peer, YesNo, YesNo).
Card  ::= card(Peer, YesNo?, YesNo?).

%% Show the person who offers, and ask two questions of one type.
exported procedure (Card)*respond(Offer?, Both).
(card(From?, A, B))*respond(offer(From), Both?) :-
    ground(From?) | both(From?, A?, B?, Both).

procedure both(Peer?, YesNo?, YesNo?, Both).
both(P, A, B, both(P?, A?, B?)).

%% The same card one level down, its questions at [2, 2] and [2, 3].
Nest  ::= nest(Peer, Card).
exported procedure (Nest)*nested(Both).
(nest(carol, card(carol, A, B)))*nested(both(carol, A?, B?)).
''');
      File('${fixture.path}/self.glp').writeAsStringSync('''
Peer     ::= Constant.
YesNo    ::= yes ; no.
Offer    ::= offer(Peer).
Both     ::= both(Peer, YesNo, YesNo).
Card     ::= card(Peer, YesNo?, YesNo?).
Nest     ::= nest(Peer, Card).
Question ::= card_w(Card) ; nest_w(Nest).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= card_w(_) ; nest_w(_) ; YesNo.

imported procedure two#respond(Offer?, Both, Stream(Ask(Question))).
imported procedure two#nested(Both, Stream(Ask(Question))).
imported procedure two#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

%% The person grants no at the second question and then yes at the first,
%% each at the position the view marks.
exported procedure play_each(Both, Stream(_)).
play_each(B?, Log?) :-
    two # respond(offer(bob), B, Asks),
    two # dispatch(Asks?, ch(Gs?, Ds), _),
    each_person(Ds?, Gs, Log).

procedure each_person(_?, PersonIn(Drawn), Stream(_)).
each_person([draw(Id, W, card_w(card(P, input(Q2), input(Q3)))) | Ds],
            [input(Id?, Q3?, no), input(Id?, Q2?, yes) | Gs?],
            [drawn(Id?, W?, card(P?, input(Q2?), input(Q3?))) | Log?]) :-
    ground(Id?), ground(W?), ground(P?), ground(Q2?), ground(Q3?) |
    each_person(Ds?, Gs, Log).
each_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    each_person(Ds?, Gs, Log).

%% The person grants no at the second question only.
exported procedure play_second(Both, Stream(_)).
play_second(B?, Log?) :-
    two # respond(offer(bob), B, Asks),
    two # dispatch(Asks?, ch(Gs?, Ds), _),
    second_person(Ds?, Gs, Log).

procedure second_person(_?, PersonIn(Drawn), Stream(_)).
second_person([draw(Id, _, card_w(card(P, input(Q2), input(Q3)))) | Ds],
              [input(Id?, Q3?, no) | Gs?],
              [drawn(Id?, card(P?, input(Q2?), input(Q3?))) | Log?]) :-
    ground(Id?), ground(P?), ground(Q2?), ground(Q3?) |
    second_person(Ds?, Gs, Log).
second_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    second_person(Ds?, Gs, Log).

%% The person grants terms of the questions' type at positions at which the
%% card holds no question: the peer's, [1]; past the card's arguments, [4];
%% the root, []; and below a question, [2, 1].
exported procedure play_nowhere(Both, Stream(_)).
play_nowhere(B?, Log?) :-
    two # respond(offer(bob), B, Asks),
    two # dispatch(Asks?, ch(Gs?, Ds), _),
    nowhere_person(Ds?, Gs, Log).

procedure nowhere_person(_?, PersonIn(Drawn), Stream(_)).
nowhere_person([draw(Id, _, card_w(V)) | Ds],
               [input(Id?, [1], yes), input(Id?, [4], no), input(Id?, [], yes),
                input(Id?, [2, 1], yes) | Gs?],
               [drawn(Id?, V?) | Log?]) :-
    ground(Id?), ground(V?) |
    nowhere_person(Ds?, Gs, Log).
nowhere_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    nowhere_person(Ds?, Gs, Log).

%% The nested card: the person grants no at its second question and then yes
%% at its first, each at the position the view marks.
exported procedure play_nested(Both, Stream(_)).
play_nested(B?, Log?) :-
    two # nested(B, Asks),
    two # dispatch(Asks?, ch(Gs?, Ds), _),
    nest_person(Ds?, Gs, Log).

procedure nest_person(_?, PersonIn(Drawn), Stream(_)).
nest_person([draw(Id, _, nest_w(nest(P, card(P1, input(Q2), input(Q3))))) | Ds],
            [input(Id?, Q3?, no), input(Id?, Q2?, yes) | Gs?],
            [drawn(Id?, nest(P?, card(P1?, input(Q2?), input(Q3?)))) | Log?]) :-
    ground(Id?), ground(P?), ground(P1?), ground(Q2?), ground(Q3?) |
    nest_person(Ds?, Gs, Log).
nest_person([withdraw(Id) | Ds], Gs?, [withdrawn(Id?) | Log?]) :-
    ground(Id?) |
    nest_person(Ds?, Gs, Log).
''');
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    Future<_Run> play(String goal) async {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(fixture.absolute.path), isTrue);
      return _Run(await engine.runGoal(goal), engine);
    }

    test('a card with two questions of one type: each is answered by its own '
        'grant and not by the other\'s', () async {
      final r = await play('play_each(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // The view marks the two questions input([2]) and input([3]).  The
      // person granted no at [3] first and yes at [2] after: by the order of
      // the view alone, the first grant would have answered [2].
      expect(r['B'], 'both(bob, yes, no)');
      expect(
          r['Log'],
          '[drawn(1, form(card, [field(Peer, shown), field(YesNo, '
          'buttons([yes, no])), field(YesNo, buttons([yes, no]))]), '
          'card(bob, input([2]), input([3]))), withdrawn(1) | _]');
    });

    test('a grant at the second question answers it and leaves the first, '
        'before it in the view, open', () async {
      final r = await play('play_second(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(bob, _, no)');
      expect(r['Log'], '[drawn(1, card(bob, input([2]), input([3]))) | _]');
    });

    test('a grant with a position at which the construct has no question is '
        'refused, and the questions stay open', () async {
      final r = await play('play_nowhere(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(bob, _, _)');
      expect(r['Log'], '[drawn(1, card(bob, input([2]), input([3]))) | _]');
    });

    test('a question below the root of a structure the program writes is at '
        'the path down to it, and each is answered by its own grant', () async {
      final r = await play('play_nested(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(carol, yes, no)');
      expect(
          r['Log'],
          '[drawn(1, nest(carol, card(carol, input([2, 2]), '
          'input([2, 3])))), withdrawn(1) | _]');
    });
  });

  // vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D: a stream or a list
  // the program writes whose elements hold questions, Present handing the
  // person each element with the writers inside it (vGLP at 66b6500,
  // Definition "vmaGLP Transition System"), each a question (Definition
  // "Question"), each element drawn as it is presented and its questions
  // answered as any grant.  programs/tests/vglp/cards: cards.vglp, and the
  // plays of its self.glp, whose scripted person logs each view it reads,
  // view(V), and answers once the cards it waits for are drawn.  The stream
  // is drawn as its elements so far (vGLP 118e04d, Definition "Construct
  // Process"): a list the program has written whole when it is drawn is
  // drawn whole, with no view before it.
  group('a stream or a list the program writes whose elements hold questions '
      '(Task D)', () {
    test('a panel of two cards, a list the program writes at once: drawn '
        'with both cards, both open together, each answered at its own '
        'position, the program receiving both, and the construct '
        'withdrawn', () async {
      final r = await _play('cards', 'play_panel(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      final log = r['Log'];
      expect(log, startsWith('[both(1, [1, 1, 2], [1, 2, 1, 2]), '));
      expect(log, contains('got(1, yes)'));
      expect(log, contains('got(2, no)'));
      expect(log, contains('withdrawn(1)'));
    });

    test('a stream of cards: the third drawn only once the first two are '
        'answered, then answered as they were, and the construct withdrawn '
        'with the stream closed', () async {
      final r = await _play('cards', 'play_deal(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      final log = r['Log'];
      // The first two cards drawn together, the stream's unwritten tail not
      // drawn.
      expect(log, startsWith('[two([1, 2], [2, 1, 2]), '));
      // No view holds the third card before the person answers the first
      // two, which they do on reading two(...).
      expect(log.substring(0, log.indexOf('two(')), isNot(contains('card(3')));
      expect(log, contains('three([2, 2, 1, 2])'));
      expect(log.indexOf('three('), greaterThan(log.indexOf('two(')));
      for (final got in ['got(1, yes)', 'got(2, yes)', 'got(3, no)']) {
        expect(log, contains(got));
      }
      expect(log.indexOf('got(3, no)'), greaterThan(log.indexOf('three(')));
      expect(log, contains('withdrawn(1)'));
    });
  });

  // vGLP #8 Cowork, 2026-10-11 20:48 UTC ("19:09 (D, Q1)") and 22:51 UTC,
  // Udi approved; vGLP 118e04d, Definition "Construct Process": "It has the
  // bridge draw the widget of T with the output so far, again as more
  // arrives, each widget position the program has not yet written whole ---
  // the root, a field, an element of any stream, the thread's included ---
  // drawn as pending, and a stream's unwritten tail not drawn, so that the
  // questions beside it are offered at once".  programs/tests/vglp/pending:
  // pending.vglp, and the plays of its self.glp, whose scripted person logs
  // what it reads and sends the program its word, peer(P) or word(W), which
  // writes what the program has left unwritten.
  group('a position the program has not yet written whole is drawn pending',
      () {
    test('a card whose Peer the program writes after the ask: drawn with its '
        'Peer pending(P) and its question input(Q) at once, drawn again once '
        'the Peer is written, and answered', () async {
      final r = await _play('pending', 'play_late(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // The Peer pending at [1], the question at [2]; the person sends bob,
      // the card is drawn again with bob in it, and the person answers yes.
      expect(r['Log'],
          '[pending(1, [1], [2]), whole(1, [2]), got(0, yes), withdrawn(1) | _]');
    });

    test('its question is answerable at once: answered while its Peer is '
        'still pending, the program receives the answer, and the construct '
        'withdraws, holding no question', () async {
      final r = await _play('pending', 'play_late_answered(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Log'], '[pending(1, [1], [2]), got(0, no), withdrawn(1) | _]');
    });

    test('a stream partly written: its first element pending, its second\'s '
        'Peer pending, its third whole and its tail not drawn; the third '
        'answered first, its grant passing the pending element and the '
        'question before it; the second answered while its Peer is pending; '
        'each drawn again as it is written; the stream closed and the '
        'construct withdrawn', () async {
      final r = await _play('pending', 'play_deal(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // third(P1, P2, Q2, Q3): the view [pending(P1), card(pending(P2),
      // input(Q2)), card(bob, input(Q3))], three elements and no more.
      expect(
          r['Log'],
          '[third([1], [2, 1, 1], [2, 1, 2], [2, 2, 1, 2]), got(3, yes), '
          'second([1, 2], [2, 1, 1], [2, 1, 2]), got(2, no), first([1, 2]), '
          'got(1, yes), withdrawn(1) | _]');
    });

    test('a thread element written before its argument is bound: drawn '
        'pending(P) at its own position beside the question, the thread\'s '
        'unwritten tail not drawn, then drawn whole', () async {
      final r = await _play('pending', 'play_board(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // The second note at [1, 2, 1]: the thread at [1], its second cell at
      // [1, 2], that cell's element at [1, 2, 1].
      expect(r['Log'],
          '[pending(1, [1, 2, 1], [2]), whole(1, [2]), got(0, yes), '
          'withdrawn(1) | _]');
    });
  });

  // vGLP 092c9fe, Definition "Volitional Procedure, Interactive Type": "a
  // named type in it is recursive only if it has no mode inversion", and the
  // paragraph after it: "A speech act forwarded with its provenance is signed
  // again at each forward, so its type is recursive, and a request to
  // forward it carries it.  With no mode inversion, a recursive type is
  // written whole by the program or whole by the person" (vGLP #14 Cowork,
  // 2026-10-18 11:50 UTC, Udi approved).  programs/tests/vglp/recursive:
  // speech.vglp, SpeechAct ::= speech(Utterance, Key) and Utterance ::=
  // text(String) ; SpeechAct, recursive through each other, and the plays of
  // its self.glp.  The widget of a recursive type is finite by a reference
  // back to itself, rec(T, W) binding T to W around the ref(T) in it.
  group('recursive interactive types with no mode inversion', () {
    const widget = 'rec(SpeechAct, form(speech, [field(Utterance, '
        'rec(Utterance, menu([form(text, [field(String, text)]), '
        'form(speech, [field(Utterance, ref(Utterance)), field(Key, '
        'peer)])]))), field(Key, peer)]))';

    test('a speech act the person writes, signed twice: formed whole from '
        'one grant by recursion on the type\'s definition, received by the '
        'program, and the construct withdrawn', () async {
      final r = await _play('recursive', 'play_say(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(
          r['Log'],
          '[drawn(1, $widget, input([])), '
          'said(speech(speech(text(hi), alice), bob)), withdrawn(1) | _]');
    });

    test('a grant that forms no speech act answers nothing, and the next '
        'one, a speech act, answers the question', () async {
      final r = await _play('recursive', 'play_say_refused(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(
          r['Log'],
          '[drawn(1, $widget, input([])), said(speech(text(hi), carol)), '
          'withdrawn(1) | _]');
    });

    test('a speech act the program writes, signed twice: drawn whole beside '
        'the person\'s yes or no, by its widget, a form whose field of the '
        'recursive type is drawn by rec(T, W); answered, and the construct '
        'withdrawn', () async {
      final r = await _play('recursive', 'play_forward(Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(
          r['Log'],
          '[drawn(1, form(forward, [field(SpeechAct, rec(SpeechAct, '
          'form(speech, [field(Utterance, rec(Utterance, menu([form(text, '
          '[field(String, shown)]), form(speech, [field(Utterance, '
          'ref(Utterance)), field(Key, shown)])]))), field(Key, shown)]))), '
          'field(YesNo, buttons([yes, no]))]), '
          'speech(speech(text(hi), alice), bob), input([2])), '
          'forwarded(yes), withdrawn(1) | _]');
    });
  });
}
