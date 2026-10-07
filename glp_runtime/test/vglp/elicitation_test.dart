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
// Sections 1 and 3, and its plays) and programs/tests/vglp/stream (chat.vglp,
// the chat input of Section 3, and its play); the Real question and the card
// of two questions are written under programs/tests/vglp/ for their runs.

import 'dart:async';
import 'dart:io';

import 'package:test/test.dart';
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

/// A traced run of [goal] in [program], and the dispatcher's serve/9 as it
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
    if (args.length == 9) serve = args;
  }
  expect(serve, isNotNull, reason: 'the dispatcher\'s serve/9 never suspended');
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
          '[drawn(0, form(card, [shown, buttons([yes, no])]), '
          'card(bob, input([2]))), withdrawn(0) | _]');
    });

    test('a grant that forms no term of the question\'s type answers nothing, '
        'and the card stays', () async {
      final r = await _play('fragments', 'play_card_refused(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Resp'], '_');
      expect(r['Log'], '[drawn(0, card(bob, input([2]))) | _]');
    });

    test('a refused grant leaves its question open, and the next grant that '
        'forms a term of its type answers it', () async {
      final r =
          await _play('fragments', 'play_card_refused_then_yes(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // maybe answered nothing; yes, the next grant, answered the card.
      expect(r['Resp'], 'accept(bob)');
      expect(r['Log'], '[drawn(0, card(bob, input([2]))), withdrawn(0) | _]');
    });
  });

  group('(v) a reader-mode stream question, and the agent\'s message clause',
      () {
    test('a stream question takes one element per submission, a submission '
        'of no element of its type taking none', () async {
      final r = await _play('stream', 'play_chat(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'],
          '[msg(bob, "hello"), msg(bob, "world") | _]');
      // One construct, drawn once, an input box that stays open, the
      // stream's own position the root, [].
      expect(r['Log'], '[drawn(0, input_box(text), input([])) | _]');
      expect(r['Log'], isNot(contains('withdrawn')));
    });

    test('a stream question takes two grants as two elements and stays open',
        () async {
      final r = await _play('stream', 'play_chat_two(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'], '[msg(carol, "one"), msg(carol, "two") | _]');
      expect(r['Log'], '[drawn(0, input_box(text), input([])) | _]');
    });

    test('the agent\'s message clause reads no request: it reduces on an '
        'arriving friend offer without waiting, the card of the offer is '
        'drawn, and the request form stays on screen', () async {
      final r = await _play('fragments', 'play_offer(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      final log = r['Log'];
      expect(
          log,
          contains('drawn(0, input_box(menu([form(post, [text]), '
              'button(quit)])), input([]))'));
      expect(log,
          contains('drawn(1, form(card, [shown, buttons([yes, no])]), '
              'card(carol, input([2])))'));
      expect(log, isNot(contains('withdrawn')));
    });
  });

  group('the agent\'s question, a stream of requests asked once', () {
    test('the person posts a text, a post of a number refused on the way, and '
        'quits, all on one construct, drawn once and kept open', () async {
      final r = await _play('fragments', 'play_request(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Outs'], '["hi"]');
      expect(r['Log'],
          '[drawn(0, input_box(menu([form(post, [text]), button(quit)])), '
          'input([])) | _]');
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
      for (final e in ['drawn(0)', 'withdrawn(0)', 'drawn(1)', 'withdrawn(1)']) {
        expect(r['Log'], contains(e));
      }
      expect(r['Log'], endsWith('| _]'));
      // No live route is left, and Deads holds one grant writer per
      // withdrawn construct, neither written after its withdrawal: the stale
      // grant reached no construct.
      expect(serve[5], '[]');
      final deads = _elements(serve[6]);
      expect(deads, hasLength(2), reason: serve[6]);
      for (final d in deads) {
        expect(d, matches(RegExp(r'^X\d+$')), reason: serve[6]);
      }
    });

    test('after a play, Deads holds one entry per withdrawn construct', () async {
      // One card, answered and withdrawn.
      final (card, s1) =
          await _servedPlay('fragments', 'play_card(Resp, Log)');
      expect(card['Resp'], 'accept(bob)');
      expect(_elements(s1[6]), hasLength(1), reason: s1[6]);
      expect(s1[5], '[]');
      // A card refused and so never withdrawn: its route live, Deads empty.
      final (refused, s2) =
          await _servedPlay('fragments', 'play_card_refused(Resp, Log)');
      expect(refused['Resp'], '_');
      expect(s2[6], '[]');
      expect(_elements(s2[5]), hasLength(1), reason: s2[5]);
      // The request form, a stream question, and a card nobody answers: two
      // live routes, none dead.
      final (offer, s3) =
          await _servedPlay('fragments', 'play_offer(Outs, Log)');
      expect(offer.status, isNot(ExecutionStatus.failed));
      expect(s3[6], '[]');
      expect(_elements(s3[5]), hasLength(2), reason: s3[5]);
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
PersonIn ::= [_ | PersonIn].

imported procedure quote#quote(Price, Stream(Ask(Question))).
imported procedure quote#dispatch(Stream(Ask(Question))?,
    Channel(PersonIn, Stream(_))?, Channel(Stream(_), Stream(_))).

exported procedure play_quote(Price, Stream(_)).
play_quote(P?, Log?) :-
    quote # quote(P, Asks),
    quote # dispatch(Asks?, ch(Gs?, Ds), _),
    pricing_person(Ds?, Gs, Log).

procedure pricing_person(_?, PersonIn, Stream(_)).
pricing_person([draw(Id, W, input(Q)) | Ds],
               [input(Id?, Q?, price(3)), input(Id?, Q?, price(2.5)) | Gs?],
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

    test('a grant of an integer forms no Real and is refused; a grant of a '
        'real answers it; the construct is a number field', () async {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(fixture.absolute.path), isTrue);
      final r = _Run(await engine.runGoal('play_quote(P, Log)'), engine);
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['P'], 'price(2.5)');
      expect(r['Log'],
          '[drawn(0, form(price, [number]), input([])), withdrawn(0) | _]');
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
PersonIn ::= [_ | PersonIn].

imported procedure two#respond(Offer?, Both, Stream(Ask(Question))).
imported procedure two#nested(Both, Stream(Ask(Question))).
imported procedure two#dispatch(Stream(Ask(Question))?,
    Channel(PersonIn, Stream(_))?, Channel(Stream(_), Stream(_))).

%% The person grants no at the second question and then yes at the first,
%% each at the position the view marks.
exported procedure play_each(Both, Stream(_)).
play_each(B?, Log?) :-
    two # respond(offer(bob), B, Asks),
    two # dispatch(Asks?, ch(Gs?, Ds), _),
    each_person(Ds?, Gs, Log).

procedure each_person(_?, PersonIn, Stream(_)).
each_person([draw(Id, W, card(P, input(Q2), input(Q3))) | Ds],
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

procedure second_person(_?, PersonIn, Stream(_)).
second_person([draw(Id, _, card(P, input(Q2), input(Q3))) | Ds],
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

procedure nowhere_person(_?, PersonIn, Stream(_)).
nowhere_person([draw(Id, _, V) | Ds],
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

procedure nest_person(_?, PersonIn, Stream(_)).
nest_person([draw(Id, _, nest(P, card(P1, input(Q2), input(Q3)))) | Ds],
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
          '[drawn(0, form(card, [shown, buttons([yes, no]), buttons([yes, '
          'no])]), card(bob, input([2]), input([3]))), withdrawn(0) | _]');
    });

    test('a grant at the second question answers it and leaves the first, '
        'before it in the view, open', () async {
      final r = await play('play_second(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(bob, _, no)');
      expect(r['Log'], '[drawn(0, card(bob, input([2]), input([3]))) | _]');
    });

    test('a grant with a position at which the construct has no question is '
        'refused, and the questions stay open', () async {
      final r = await play('play_nowhere(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(bob, _, _)');
      expect(r['Log'], '[drawn(0, card(bob, input([2]), input([3]))) | _]');
    });

    test('a question below the root of a structure the program writes is at '
        'the path down to it, and each is answered by its own grant', () async {
      final r = await play('play_nested(B, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['B'], 'both(carol, yes, no)');
      expect(
          r['Log'],
          '[drawn(0, nest(carol, card(carol, input([2, 2]), '
          'input([2, 3])))), withdrawn(0) | _]');
    });
  });
}
