// glp_runtime/test/vglp/constructs_test.dart
//
// The dispatcher and the construct processes of the canonical compilation, as
// emitted.
// Spec: vGLP at db03e2d --- sections/elicitation.tex, Definition "Canonical
// Compilation" and the paragraph before it, Definition "Construct,
// Submission, Complete Widget", Definition "Widget Declaration, Default
// Widget"; sections/vglp.tex, Definition "vmaGLP Transition System".  vGLP's
// code task of 2026-10-02 00:13 UTC, Part 2, with the answers of 2026-10-01
// 23:55 UTC (E) and of 2026-10-02 08:26 UTC; the generic source's form of
// 2026-10-02 21:02 UTC (B) without the handle (vGLP #5 Cowork, 2026-10-03
// 08:16 UTC, item 2); the forming result carrying the term and the grants
// that never close (item 3, with the answers of 21:13 UTC, Q1 and Q2, from
// vGLP at 7838827, Definition "Person Channel, Person Writer, GLP with
// Persons, Grant"); the grant naming its question, input(Id, P, R), P the
// question's position in its construct (vGLP #5 Cowork, 2026-10-04 09:05
// UTC, C).  The runs are elicitation_test.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart'
    show buildAncestorScope, rootScope;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/compiler/ast.dart' show ConstTerm;
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/dispatcher.dart';
import 'package:glp_runtime/vglp/constructs.dart' show listEnd;

const _programs = '../programs';

void main() {
  final dir = Directory('$_programs/vglp');
  if (!File('${dir.path}/${DispatcherSource.fileName}').existsSync()) {
    // The generic source is the compilation's input; without it there is
    // nothing to emit, and these tests would check nothing.
    return;
  }
  // The scope of a module directly under the root, programs/self.glp its one
  // layer, passed in.
  final scope = rootScope(File('$_programs/self.glp').absolute.path);
  final dispatcher = DispatcherSource.fromDirectory(dir.path);

  CanonicalProgram compile(String text) =>
      compileCanonical(text, dispatcher: dispatcher, scope: scope);

  Matcher refused(String why) => throwsA(isA<CompileError>()
      .having((e) => e.message, 'message', contains(why)));

  // The line and column, from 1, at which [needle] first occurs in [text].
  (int, int) at(String text, String needle) {
    final i = text.indexOf(needle);
    final before = text.substring(0, i).split('\n');
    return (before.length, before.last.length + 1);
  }

  // A refusal with [why] in its message, at [line] and [column].
  Matcher refusedAt(String why, int line, int column) => throwsA(
      isA<CompileError>()
          .having((e) => e.message, 'message', contains(why))
          .having((e) => e.line, 'line', line)
          .having((e) => e.column, 'column', column));

  const card = '''
Peer ::= Constant.
Offer ::= offer(Peer).
Response ::= accept(Peer) ; refuse(Peer).
YesNo ::= yes ; no.
Card ::= card(Peer, YesNo?).
exported procedure (Card)*respond_coldcall(Offer?, Response).
(card(From?, Answer))*respond_coldcall(offer(From), Resp?) :-
    ground(From?) | decide(Answer?, From?, Resp).
procedure decide(YesNo?, Peer?, Response).
decide(yes, From, accept(From?)).
decide(no, From, refuse(From?)).
''';

  group('the dispatcher', () {
    test('its generic source is a module that loads and type-checks by '
        'itself, parameterised in the program\'s questions and naming no '
        'construct process (B)', () {
      final engine = GlpEngine(
          rootSelfGlpPath: File('$_programs/self.glp').absolute.path);
      expect(
          engine.loadFile(
              File('${dir.path}/${DispatcherSource.fileName}').absolute.path),
          isTrue);
      // No clause calls a construct process, and no type is left for the
      // compilation to supply but the questions, the parameter Q.
      final calls = {
        for (final p in dispatcher.module.procedures)
          for (final c in p.clauses)
            for (final g in c.body ?? const []) g.functor
      };
      expect(calls, isNot(contains('construct')));
      expect(dispatcher.module.typeDefs.map((t) => t.name),
          containsAll(['Ask', 'Spawn', 'Inputs', 'Route', 'Routes', 'Deads']));
      // The person channel, a grant, its path and a draw are the root's
      // (programs/self.glp; vGLP, Section 6.3).
      expect(dispatcher.module.typeDefs.map((t) => t.name),
          isNot(anyOf(contains('Input'), contains('Path'), contains('Draw'),
              contains('PersonIn'), contains('PersonOut'))));
      expect(dispatcher.module.typeDefs.map((t) => t.name),
          isNot(contains('Handle')));
    });

    test('is emitted from programs/vglp/dispatcher.glp: the program\'s '
        'dispatch/3 exported, reading the ask stream and the person channel '
        'and giving the program its end of one, and spawning the generic '
        'dispatch/4 and constructs/1', () {
      final c = compile(card);
      expect(c.dispatchName, 'dispatch');
      expect(
          c.source,
          contains('exported procedure dispatch(Stream(Ask(Question))?, '
              'PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).'));
      expect(
          c.source,
          contains('dispatch(Asks, PCh, MCh?) :- dispatch(Asks?, PCh?, MCh, '
              'Ss), constructs(Ss?).'));
      // The generic dispatch/4, parameterised in the questions and in the
      // person channel's type, unexported.
      expect(
          c.source,
          contains('\nprocedure(Q, T) dispatch(Stream(Ask(Q))?, '
              'PersonChannel(T)?, Channel(Stream(_), Stream(_)), '
              'Stream(Spawn(Q, T))).'));
      expect(c.source, isNot(contains('Draw ::= ')));
      expect(c.source, contains('Ask(Q) ::= ask(Constant, Q).'));
      expect(
          c.source,
          contains('Spawn(Q, T) ::= spawn(Integer, Q, Inputs(T), '
              'Stream(Draw(T))?).'));
      // The person channel's type, the values the program draws under the
      // functors of its asks and the grants' raw inputs (vGLP, Definition
      // "Construct Process").
      expect(c.source, contains('Drawn ::= card_w(_) ; YesNo.'));
      // On an ask it writes the spawn of the construct process with the next
      // identifier, keeping the writer of its grants and the reader of its
      // draws; constructs/1 calls construct/4 on each spawn.
      expect(
          c.source,
          contains('serve([ask(_, Q) | Es], CInto?, DOut?, Rs, Xs, N, '
              '[spawn(N?, Q?, Gs?, Ds) | Ss?]) :- ground(N?) | '
              'merge(Ds?, CInto1?, CInto)'));
      // The asks, the grants and the draws taken in turn, merged into one
      // stream by fair/5 (vGLP 2026-10-14 23:05 UTC).
      expect(c.source,
          contains('fair(ask, Asks?, Inputs?, COut?, Es), serve(Es?, COut, '));
      expect(
          c.source,
          contains('constructs([spawn(Id, Q, Gs, Ds?) | Ss]) :- '
              'construct(Id?, Q?, Gs?, Ds), constructs(Ss?).'));
      expect(c.source, contains('constructs([]).'));
      // A grant is input(Id, P, R), routed whole by Id, and the question at
      // P reads the input from it (C).
      expect(c.source, isNot(contains('Input ::= ')));
      expect(c.source, isNot(contains('Path ::= ')));
      expect(
          c.source,
          contains('split_grants([input(Id, P, R) | Gs], '
              '[input(Id?, P?, R?) | Is?], Ms?) :- split_grants(Gs?, Is, '
              'Ms).'));
      expect(
          c.source,
          contains('route_grant(input(Id, P, R), [route(Id1, '
              '[input(Id?, P?, R?) | Gs1?]) | Rs], [route(Id1?, Gs1) | '
              'Rs?]) :- (Id? =?= Id1?) | true.'));
      expect(
          c.source,
          contains('answer_yesNo(P, X?, [input(Id, P1, R) | Gs], Gs1?, '
              'Done?) :- P1? =?= P?, ground(R?) | form_yesNo(R?, F), '
              'take_yesNo(F?, P?, X, input(Id?, P1?, R?), Gs?, Gs1, Done).'));
    });

    test('its person channel and a construct\'s grants never close: they are '
        'typed without [], no clause reads a [] of them, and a withdrawn '
        'construct\'s grant writer goes to Deads (Q1)', () {
      final c = compile(card);
      expect(c.source, contains('Inputs(T) ::= [Input(T) | Inputs(T)].'));
      expect(c.source, contains('Route(T) ::= route(Integer, Inputs(T)?).'));
      expect(c.source,
          contains('Deads(T) ::= [] ; [Inputs(T)? | Deads(T)].'));
      expect(
          c.source,
          contains('procedure(T) split_grants(PersonIn(T)?, Inputs(T), '
              'Stream(_)).'));
      expect(
          c.source,
          contains('procedure(Q, T) serve(Events(Q, T)?, Stream(Draw(T)), '
              'PersonOut(T), Routes(T)?, Deads(T)?, Integer?, '
              'Stream(Spawn(Q, T))).'));
      // No clause for a closed person channel or closed grants, and nothing
      // that closes a construct's grants.
      expect(c.source, isNot(contains('split_grants([], ')));
      expect(c.source, isNot(contains('serve([], [], ')));
      expect(c.source, isNot(contains('drain(')));
      expect(c.source, isNot(contains('close_routes(')));
      expect(c.source, isNot(contains('answer_yesNo(_?, [], ')));
      expect(
          c.source,
          contains('remove_route(Id, [route(Id1, Gs?) | Rs], Rs?, Xs, '
              '[Gs | Xs?]) :- (Id? =?= Id1?) | true.'));
    });

    test('no clause it emits has an anonymous reader (GLP-Spec\'s Remark '
        '"Anonymous Variables", c3d3fc6)', () {
      final c = compile(card);
      final clauses = c.source.split('\n').where((l) =>
          l.isNotEmpty &&
          !l.startsWith('%') &&
          !l.startsWith('procedure') &&
          !l.startsWith('exported') &&
          !l.contains('::='));
      for (final l in clauses) {
        expect(l, isNot(contains('_?')), reason: l);
      }
    });

    test('its names, and the construct processes\', are fresh against the '
        'program\'s', () {
      final c = compile('''
T ::= t.
Draw ::= d.
Ask ::= a.
procedure (T?)*dispatch(Draw?).
(t)*dispatch(_).
procedure construct(Draw?).
construct(_).
procedure constructs(Draw?).
constructs(_).
procedure run(Draw?).
run(_).
''');
      expect(c.dispatchName, 'dispatch_1');
      expect(c.constructName, 'construct_1');
      expect(c.askType, 'Ask_1');
      expect(c.source, contains('exported procedure dispatch_1('));
      expect(c.source, contains('procedure(Q, T) dispatch_1('));
      expect(c.source, contains('Ask_1(Q) ::= ask(Constant, Q).'));
      expect(c.source, contains('procedure run_t_r(Integer?, _?, Stream(_)?, '
          'Done?, Stream(Draw(Drawn))).'));
      expect(c.source, contains('constructs_1([spawn(Id, Q, Gs, Ds?) | Ss]) '
          ':- construct_1(Id?, Q?, Gs?, Ds), constructs_1(Ss?).'));
      expect(c.source, contains('construct_1(Id, t_r(X?), Gs, Ds?) :- '));
      // The program's own are untouched.
      expect(c.source, contains('procedure construct(Draw?).'));
      expect(c.source, contains('procedure constructs(Draw?).'));
      expect(c.source, contains('procedure run(Draw?).'));
      expect(c.source, contains('Ask ::= a.'));
    });

    test('construct/4 is declared over the program\'s questions and its '
        'grants', () {
      final c = compile('''
T ::= t.
procedure (T?)*p.
(t)*p.
''');
      expect(c.source, contains('procedure construct(Integer?, Question?, '
          'Inputs(Drawn)?, Stream(Draw(Drawn))).'));
      expect(c.source,
          contains('procedure constructs(Stream(Spawn(Question, Drawn))?).'));
    });

    test('is not emitted without the generic source, and the compilation '
        'defines the asks itself', () {
      final c = compileCanonical(card, scope: scope);
      expect(c.dispatchName, isNull);
      expect(c.source, isNot(contains('procedure dispatch(')));
      expect(c.source, isNot(contains('construct(')));
      expect(c.source, isNot(contains('constructs(')));
      expect(c.source, contains('Ask(Q) ::= ask(Constant, Q).'));
    });
  });

  group('the construct process of a writer-mode question, Section 3\'s card',
      () {
    late String s;
    setUp(() => s = compile(card).source);

    test('shows the card the program writes, the person\'s position as input, '
        'and draws it with the default widget, a form', () {
      expect(
          s,
          contains('construct(Id, card_w(X), Gs, Ds?) :- '
              'present_card([], X?, Gs?, _, Vs, Done), '
              'run_card_w(Id?, form(card, [field(\'Peer\', shown), field(\'YesNo\', '
              'buttons([yes, no]))]), Vs?, Done?, '
              'Ds).'));
      // The peer is shown at its position, [1], pending(P) until the
      // program writes it, and the question beside it is offered at once,
      // marked with its position, the card's at the root [] and the question
      // its argument 2, [2] (C); the card drawn again as the peer is
      // written (vGLP 118e04d, Definition "Construct Process").
      expect(
          s,
          contains('present_card(P, card(H1, H2?), Gs, Gs1?, Vs?, D2?) :- '
              'ground(P?) | path(P?, 1, Q1), shown(Q1?, H1?, V1), '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2), '
              'start_card_card_2(V1?, [input(M2?)], Vs).'));
      expect(
          s,
          contains('comb_card_card_2(_, V2, [V1 | S1], S2, '
              '[card(V1?, V2?) | Vs?]) :- ground(V1?), ground(V2?) | '
              'comb_card_card_2(V1?, V2?, S1?, S2?, Vs).'));
      // The card itself not yet written when it is drawn: pending at the
      // root, the grants passed on until it is.
      expect(
          s,
          contains('present_card(P, X, Gs, Gs1?, [pending(P?) | Vs?], '
              'Done?) :- ground(P?) | '
              'present_card_wait(P?, X?, Gs?, Gs1, Vs, Done).'));
      expect(
          s,
          contains('present_card_wait(P, card(H1, H2?), Gs, Gs1?, Vs?, '
              'Done?) :- present_card(P?, card(H1?, H2), Gs?, Gs1, Vs, '
              'Done).'));
      expect(
          s,
          contains('present_card_wait(P, X, [G | Gs], [G? | Gs1?], Vs?, '
              'Done?) :- present_card_wait(P?, X?, Gs?, Gs1, Vs, Done).'));
      expect(s, contains('procedure path(Path?, Integer?, Path).'));
      expect(s, contains('procedure shown(Path?, _?, Stream(_)).'));
      expect(s, contains('shown(P, X, [pending(P?) | Vs?]) :- '
          'shown_whole(X?, Vs).'));
    });

    test('binds the question with the term the person\'s input forms, by '
        'clauses typed at its type, the result carrying the term, a grant of '
        'no such term passed on', () {
      expect(s, contains('procedure form_yesNo(_?, Formed(YesNo)).'));
      expect(s, contains('form_yesNo(yes, formed(yes)).'));
      expect(s, contains('form_yesNo(no, formed(no)).'));
      expect(s, contains('form_yesNo(_, refused) :- otherwise | true.'));
      expect(
          s,
          contains('procedure take_yesNo(Formed(YesNo)?, Path?, YesNo, '
              'Input(Drawn)?, Inputs(Drawn)?, Inputs(Drawn), Done).'));
      expect(s, contains('take_yesNo(formed(V), _, V?, _, Gs, Gs?, done).'));
      expect(
          s,
          contains('take_yesNo(refused, P, X?, G, Gs, [G? | Gs1?], Done?) :- '
              'answer_yesNo(P?, X, Gs?, Gs1, Done).'));
      expect(
          s,
          contains('procedure answer_yesNo(Path?, YesNo, Inputs(Drawn)?, '
              'Inputs(Drawn), Done).'));
      // A grant for another position goes on through take_N's refused
      // clause, rebuilt whole (C).
      expect(
          s,
          contains('answer_yesNo(P, X?, [input(Id, P1, R) | Gs], Gs1?, '
              'Done?) :- otherwise | take_yesNo(refused, P?, X, '
              'input(Id?, P1?, R?), Gs?, Gs1, Done).'));
    });
  });

  group('the construct process of a reader-mode question', () {
    test('a menu of forms: the whole term the person\'s, formed from the '
        'grant alternative by alternative, a primitive by its guard', () {
      final s = compile('''
Request ::= post(String) ; quit.
procedure (Request?)*agent(Integer?).
(post(T))*agent(_) :- ground(T?) | true.
(quit)*agent(_).
''').source;
      expect(
          s,
          contains('construct(Id, request_r(X?), Gs, Ds?) :- '
              'answer_request([], X, Gs?, _, Done), run_request_r(Id?, menu([form(post, '
              "[field('String', text)]), button(quit)]), [input([])], Done?, "
              'Ds).'));
      expect(s,
          contains('form_request(post(R1), F?) :- form_string(R1?, F1), '
              'join_request_post_1(F1?, F).'));
      expect(s,
          contains('join_request_post_1(formed(X1), formed(post(X1?))).'));
      expect(s, contains('join_request_post_1(refused, refused).'));
      expect(s, contains('form_request(quit, formed(quit)).'));
      // The term is written in the body, where the guard narrows the input
      // (Q2).
      expect(s, contains('form_string(R, F?) :- string(R?) | '
          'F = formed(R?).'));
    });

    test('form_any, which no construct calls, the type _ refused everywhere '
        'in an interactive type, is not emitted; the formers of the '
        'primitives are (vGLP #8 Cowork, 2026-10-11 17:24 UTC, NOTED)', () {
      final s = compile('''
Request ::= post(String) ; quit.
procedure (Request?)*agent(Integer?).
(post(T))*agent(_) :- ground(T?) | true.
(quit)*agent(_).
''').source;
      for (final f in ['string', 'integer', 'number', 'real', 'constant',
          'module']) {
        expect(s, contains('procedure form_$f(_?, Formed('));
      }
      expect(s, isNot(contains('form_any')));
    });

    test('a union with a primitive alternative: the primitive formed by its '
        'guard, the term written in the body (Q2)', () {
      final s = compile('''
Amount ::= Integer ; none.
Bid ::= bid(Amount, String).
procedure (Bid?)*offer(Bid).
(B)*offer(B?).
''').source;
      expect(s, contains('procedure form_amount(_?, Formed(Amount)).'));
      expect(s, contains('form_amount(none, formed(none)).'));
      expect(s,
          contains('form_amount(R, F?) :- integer(R?) | F = formed(R?).'));
      expect(s, contains('form_amount(_, refused) :- otherwise | true.'));
    });

    test('a stream: an input box that stays open, one element per '
        'submission', () {
      final s = compile('''
Peer ::= Constant.
Msg ::= msg(Peer, String).
procedure (Stream(String)?)*chat(Peer?, Stream(Msg)).
(Ms)*chat(Peer, Out?) :- ground(Peer?) | send_all(Peer?, Ms?, Out).
procedure send_all(Peer?, Stream(String)?, Stream(Msg)).
send_all(Peer, [M|Ms], [msg(Peer?, M?)|Out?]) :-
    ground(Peer?) | send_all(Peer?, Ms?, Out).
send_all(_, [], []).
''').source;
      // The stream's own position, the root [], is that of every element
      // the person submits (C).
      expect(
          s,
          contains('construct(Id, stream_string_r(X?), Gs, Ds?) :- '
              'answer_stream_string([], X, Gs?, _, Done), '
              'run_stream_string_r(Id?, input_box(text), [input([])], Done?, '
              'Ds).'));
      expect(
          s,
          contains('take_stream_string(formed(V), P, [V? | X1?], _, Gs, Gs1?, '
              'Done?) :- answer_stream_string(P?, X1, Gs?, Gs1, Done).'));
    });

    test('a structure with a date and a peer, and a nested structure', () {
      final s = compile('''
Peer ::= Constant.
Date ::= Integer.
Lot ::= lot(Integer, Date).
Order ::= order(Peer, Lot) ; cancel.
procedure (Order?)*order(Order).
(O)*order(O?).
''').source;
      expect(
          s,
          contains("menu([form(order, [field('Peer', peer), field('Lot', "
              "form(lot, [field('Integer', number), field('Date', date)]))]), "
              'button(cancel)])'));
      expect(
          s,
          contains('form_order(order(R1, R2), F?) :- '
              'form_constant(R1?, F1), form_lot(R2?, F2), '
              'join_order_order_2(F1?, F2?, F).'));
      // Formed where both children formed, refused where either is, one
      // clause per position (item 3).
      expect(
          s,
          contains('join_order_order_2(formed(X1), formed(X2), '
              'formed(order(X1?, X2?))).'));
      expect(s, contains('join_order_order_2(refused, _, refused).'));
      expect(s, contains('join_order_order_2(_, refused, refused).'));
      expect(s, isNot(contains('all_formed')));
    });
  });

  group('a position the person writes of type Real (item 6)', () {
    test('is built: formed by real/1, its default widget the number field '
        '(Definition "Widget Declaration, Default Widget" at c2e8b57)', () {
      final c = compile('''
Price ::= price(Real).
procedure (Price?)*quote(Price).
(P)*quote(P?).
''');
      expect(c.widgets['Price?'], "form(price, [field('Real', number)])");
      expect(
          c.source,
          contains('form_price(price(R1), F?) :- form_real(R1?, F1), '
              'join_price_price_1(F1?, F).'));
      expect(c.source, contains('procedure form_real(_?, Formed(Real)).'));
      expect(c.source,
          contains('form_real(R, F?) :- real(R?) | F = formed(R?).'));
    });

    test('a primitive alternative Real of a union is formed by real/1, and '
        'the Real the program writes is shown on real/1', () {
      final s = compile('''
Amount ::= Real ; none.
procedure (Amount?)*bid(Amount).
(A)*bid(A?).
''').source;
      expect(s, contains('form_amount(R, F?) :- real(R?) | F = formed(R?).'));
      expect(s, isNot(contains('form_amount(R, F?) :- number(')));
      final w = compile('''
YesNo ::= yes ; no.
Val ::= Real ; ask(YesNo?).
procedure (Val)*show(YesNo).
(ask(A))*show(A?).
''').source;
      expect(w,
          contains('present_val(_, X, Gs, Gs?, [X?], done) :- real(X?) | '
              'true.'));
    });
  });

  group('views that change', () {
    test('a stream the program writes beside one the person writes: a thread '
        'drawn as it grows, and an input box', () {
      final s = compile('''
Board ::= board(Stream(Integer), Stream(String)?).
procedure (Board)*game(Stream(String)).
(board([1, 2, 3], Moves))*game(Moves?).
''').source;
      expect(
          s,
          contains("form(board, [field('Stream(Integer)', thread), "
              "field('Stream(String)', input_box(text))])"));
      // The thread at its position, [1], its elements so far, each pending
      // until written whole and its unwritten tail not drawn (vGLP 118e04d,
      // Definition "Construct Process").
      expect(
          s,
          contains('present_board(P, board(H1, H2?), Gs, Gs1?, Vs?, D2?) :- '
              'ground(P?) | path(P?, 1, Q1), thread(Q1?, H1?, V1), '
              'path(P?, 2, M2), '
              'path(P?, 2, Q2), answer_stream_string(Q2?, H2, Gs?, Gs1, D2), '
              'start_board_board_2(V1?, [input(M2?)], Vs).'));
      expect(s, contains('thread(P, Xs, [[] | Vs?]) :- '
          'thread_tail(P?, Xs?, Vs).'));
      expect(
          s,
          contains('thread_cell(P, X, Xs, Vs?) :- ground(P?) | '
              'path(P?, 1, P1), path(P?, 2, P2), shown(P1?, X?, V1), '
              'thread(P2?, Xs?, V2), cons_views(V1?, V2?, Vs).'));
      expect(
          s,
          contains('comb_board_board_2(_, V2, [V1 | S1], S2, '
              '[board(V1?, V2?) | Vs?]) :- ground(V1?), ground(V2?) | '
              'comb_board_board_2(V1?, V2?, S1?, S2?, Vs).'));
    });

    // Its Peers was `[] ; [Peer | Peers]` until vGLP #8 Cowork's ruling of
    // 2026-10-11 15:47 UTC (item 1), which refuses it as not flat wherever
    // it stands; its flat form is a Stream.
    test('a list the program writes, with a choice of its element coming '
        'back: a picker', () {
      final s = compile('''
Peer ::= Constant.
Peers ::= Stream(Peer).
Pick ::= pick(Peers, Peer?).
procedure (Pick)*choose(Peer).
(pick([bob, carol], P))*choose(P?).
''').source;
      expect(s, contains('run_pick_w(Id?, picker, Vs?, Done?, Ds)'));
    });

    test('a structure the program writes holding one with a question', () {
      final s = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Inner ::= inner(Peer, YesNo?).
Outer ::= outer(Integer, Inner, String?).
procedure (Outer)*nested(YesNo, String).
(outer(7, inner(bob, A), S))*nested(A?, S?).
''').source;
      expect(
          s,
          contains("form(outer, [field('Integer', shown), field('Inner', "
              "form(inner, [field('Peer', shown), field('YesNo', "
              "buttons([yes, no]))])), field('String', text)])"));
      // The nested structure's position is its argument's, [2], and its
      // question's below it [2, 2]; the outer question's is [3] (C).
      expect(
          s,
          contains('present_outer(P, outer(H1, H2, H3?), Gs, Gs2?, Vs?, '
              'Done?) :- ground(P?) | path(P?, 1, Q1), shown(Q1?, H1?, V1), '
              'path(P?, 2, Q2), '
              'present_inner(Q2?, H2?, Gs?, Gs1, V2, D2), path(P?, 3, M3), '
              'path(P?, 3, Q3), answer_string(Q3?, H3, Gs1?, Gs2, D3), '
              'all_done([D2?, D3?], Done), '
              'start_outer_outer_3(V1?, V2?, [input(M3?)], Vs).'));
      expect(
          s,
          contains('present_inner(P, inner(H1, H2?), Gs, Gs1?, Vs?, D2?) :- '
              'ground(P?) | path(P?, 1, Q1), shown(Q1?, H1?, V1), '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2), '
              'start_inner_inner_2(V1?, [input(M2?)], Vs).'));
      // The nested structure not yet written when it is drawn: pending at
      // [2], the grants passed on to the outer question at [3] until it is.
      expect(
          s,
          contains('present_inner(P, X, Gs, Gs1?, [pending(P?) | Vs?], '
              'Done?) :- ground(P?) | '
              'present_inner_wait(P?, X?, Gs?, Gs1, Vs, Done).'));
      expect(
          s,
          contains('present_inner_wait(P, X, [G | Gs], [G? | Gs1?], Vs?, '
              'Done?) :- present_inner_wait(P?, X?, Gs?, Gs1, Vs, Done).'));
      expect(
          s,
          contains('present_outer_wait(P, outer(H1, H2, H3?), Gs, Gs1?, Vs?, '
              'Done?) :- present_outer(P?, outer(H1?, H2?, H3), Gs?, Gs1, '
              'Vs, Done).'));
    });

    test('a writer-mode type the person writes nowhere holds no question, and '
        'its construct withdraws at once', () {
      final s = compile('''
Note ::= note(String).
procedure (Note)*tell(String?).
(note(T?))*tell(T).
''').source;
      expect(s, contains('construct(Id, note_w(_), _, [withdraw(Id?)]).'));
    });
  });

  group('the grant names its question: input(Id, P, R) (C)', () {
    test('a construct holding two questions of one type marks each with its '
        'own position, and one answer_N serves both, each called with its '
        'own', () {
      final s = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Both ::= both(Peer, YesNo, YesNo).
Card ::= card(Peer, YesNo?, YesNo?).
procedure (Card)*respond(Peer?, Both).
(card(From?, A, B))*respond(From, Both?) :-
    ground(From?) | both(From?, A?, B?, Both).
procedure both(Peer?, YesNo?, YesNo?, Both).
both(P, A, B, both(P?, A?, B?)).
''').source;
      expect(
          s,
          contains('present_card(P, card(H1, H2?, H3?), Gs, Gs2?, Vs?, '
              'Done?) :- ground(P?) | path(P?, 1, Q1), shown(Q1?, H1?, V1), '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2), path(P?, 3, M3), '
              'path(P?, 3, Q3), answer_yesNo(Q3?, H3, Gs1?, Gs2, D3), '
              'all_done([D2?, D3?], Done), '
              'start_card_card_3(V1?, [input(M2?)], [input(M3?)], Vs).'));
      expect('procedure answer_yesNo('.allMatches(s), hasLength(1));
      expect(
          s,
          contains('answer_yesNo(P, X?, [input(Id, P1, R) | Gs], Gs1?, '
              'Done?) :- P1? =?= P?, ground(R?) | form_yesNo(R?, F), '
              'take_yesNo(F?, P?, X, input(Id?, P1?, R?), Gs?, Gs1, Done).'));
    });

    // Until vGLP #7 Cowork's ruling of 2026-10-11 14:31 UTC (item 2) a list
    // cell the program writes held questions here, at [1] and [2, 1]; a list
    // cell is now a Stream's or a List's, and one outside them holding a
    // question is refused (vGLP at 66b6500, Definition "Volitional
    // Procedure, Interactive Type, ...": "An interactive type is flat").
    test('the position is the path of argument indices from the root, and a '
        'list cell the program writes holding a question, outside a Stream '
        'or a List, is refused', () {
      const src = '''
YesNo ::= yes ; no.
Nil ::= [].
Tail ::= [YesNo? | Nil].
Two ::= [YesNo? | Tail].
procedure (Two)*pair(YesNo, YesNo).
([A, B])*pair(A?, B?).
''';
      final (l, c) = at(src, 'procedure (Two)');
      expect(
          () => compile(src),
          refusedAt(
              'The interactive type Two is not flat: the program writes Two, '
              'a list cell of which is neither a Stream nor a List, holding a '
              'question, and an interactive type is flat, "built from '
              'constants, String, Integer, Real, Key, Constant, Module, named '
              'types so built, tuples and unions, and Stream or List" (vGLP, '
              'Definition "Volitional Procedure, Interactive Type, Interactive '
              'Term, Ordinary Clause, vGLP Program")',
              l,
              c));
    });

    // Until the pending drawing (vGLP 118e04d, Definition "Construct
    // Process") an alternative holding no question did not read its
    // position, its view waiting for it, ground(H1?); it is one widget
    // position, shown, drawn pending at its position until written whole.
    test('an alternative holding no question is one widget position, shown at '
        'its position and drawn pending there until written whole, and one '
        'reading its position once does not guard on it', () {
      final s = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Inner ::= inner(Peer, YesNo?).
T ::= a(Integer) ; b(Inner).
procedure (T)*mixed(YesNo).
(b(inner(bob, A)))*mixed(A?).
''').source;
      expect(s,
          contains('present_t(P, a(H1), Gs, Gs?, Vs?, done) :- '
              'shown(P?, a(H1?), Vs).'));
      expect(
          s,
          contains('present_t(P, b(H1), Gs, Gs1?, Vs?, D1?) :- '
              'path(P?, 1, Q1), present_inner(Q1?, H1?, Gs?, Gs1, V1, D1), '));
    });
  });

  group('the functor of a moded interactive type in Question', () {
    test('the same type in both modes is two', () {
      final c = compile('''
YesNo ::= yes ; no.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
procedure (YesNo)*tell(Integer?).
(no)*tell(_).
''');
      expect(c.functors, {'YesNo?': 'yesNo_r', 'YesNo': 'yesNo_w'});
      expect(c.source,
          contains('Question ::= yesNo_r(YesNo?) ; yesNo_w(YesNo).'));
    });

    test('two moded types that give one functor are told apart in the order '
        'of their declarations', () {
      final c = compile('''
A_b ::= x.
A(T) ::= a(T).
B ::= y.
procedure (A_b?)*f(Integer?).
(x)*f(_).
procedure (A(B)?)*g(Integer?).
(a(y))*g(_).
''');
      expect(c.functors, {'A_b?': 'a_b_r', 'A(B)?': 'a_b_r_2'});
      expect(c.source, contains('Question ::= a_b_r(A_b?) ; a_b_r_2(A(B)?).'));
    });
  });

  // "The union is formed over the alternatives of these types, an
  // alternative common to two of them taken once, and the compilation
  // refuses a program two of whose alternatives have one functor and arity
  // and differ, since the alternatives of a union have distinct functors"
  // (vGLP, sections/parts/dispatcher.tex, after the dispatcher; vGLP,
  // 2026-10-16 12:05 UTC, Udi: yes).
  group('the union of the person channel\'s type over the alternatives of '
      'the raw inputs\' types', () {
    test('YesNo and Pick in one program give yes ; no ; yes(MyChild), the '
        'common no taken once, and the program loads', () {
      final c = compile('''
YesNo ::= yes ; no.
MyChild ::= Constant.
Pick ::= yes(MyChild) ; no.
procedure (YesNo?)*ok(Stream(YesNo)).
(A)*ok([A?]).
procedure (Pick?)*pick(Stream(Pick)).
(P)*pick([P?]).
''');
      expect(c.source,
          contains('Drawn ::= yesNo_r(_) ; pick_r(_) ; YesNo ; yes(MyChild).'));
      final f = File('$_programs/vglp_union_fixture_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}.glp');
      try {
        f.writeAsStringSync(c.source);
        final engine = GlpEngine(
            rootSelfGlpPath: File('$_programs/self.glp').absolute.path);
        expect(engine.loadFile(f.absolute.path), isTrue);
      } finally {
        if (f.existsSync()) f.deleteSync();
      }
    });

    test('two alternatives of one functor and arity that differ are refused, '
        'naming the two', () {
      expect(
          () => compile('''
MyChild ::= Constant.
Pick ::= yes(MyChild) ; no.
Count ::= yes(Integer) ; none.
procedure (Pick?)*pick(Stream(Pick)).
(P)*pick([P?]).
procedure (Count?)*count(Stream(Count)).
(N)*count([N?]).
'''),
          refused('have the alternatives yes(MyChild) and yes(Integer), of '
              'one functor and arity, which differ'));
    });
  });

  group('a functor that is not a name (vGLP #6 Cowork, 2026-10-11 12:55 UTC, '
      'Task A)', () {
    test('names its join and start procedures by its characters made a name, '
        'each one outside a name replaced by `_`', () {
      final s = compile('''
Choice ::= a ; b.
Pick ::= 'odd one'(Choice).
procedure (Pick?)*pick(Pick).
(P)*pick(P?).
''').source;
      expect(
          s,
          contains("form_pick('odd one'(R1), F?) :- form_choice(R1?, F1), "
              'join_pick_odd_one_1(F1?, F).'));
      expect(s,
          contains("join_pick_odd_one_1(formed(X1), formed('odd one'(X1?)))."));
      expect(s, isNot(contains('join_pick_odd one')));
      final w = compile('''
Board ::= 'the board'(Stream(Integer), Stream(String)?).
procedure (Board)*game(Stream(String)).
('the board'([1, 2, 3], Moves))*game(Moves?).
''').source;
      expect(w, contains('start_board_the_board_2('));
      expect(w, isNot(contains('start_board_the board')));
    });

    test('two functors made one name are told apart by `_2`, as the '
        'Definition\'s t is', () {
      final s = compile('''
Choice ::= a ; b.
Pick ::= 'odd one'(Choice) ; odd_one(Choice) ; 'odd-one'(Choice).
procedure (Pick?)*pick(Pick).
(P)*pick(P?).
''').source;
      expect(
          s,
          contains("form_pick('odd one'(R1), F?) :- form_choice(R1?, F1), "
              'join_pick_odd_one_1(F1?, F).'));
      expect(
          s,
          contains('form_pick(odd_one(R1), F?) :- form_choice(R1?, F1), '
              'join_pick_odd_one_1_2(F1?, F).'));
      expect(
          s,
          contains("form_pick('odd-one'(R1), F?) :- form_choice(R1?, F1), "
              'join_pick_odd_one_1_3(F1?, F).'));
      expect(s,
          contains('join_pick_odd_one_1_2(formed(X1), formed(odd_one(X1?))).'));
    });
  });

  // vGLP #6 Cowork, 2026-10-11 12:55 UTC, Task B, from Definition "Widget
  // Declaration, Default Widget" and the Answer rule of Definition "vmaGLP
  // Transition System" (vGLP 90988ea).  The runs are elicitation_test's.
  group('Task B (1): a list the person writes', () {
    test('draws the stream widget with a button that ends the list, [] its '
        'answer on the end button\'s grant, [end] (vGLP #7 Cowork, 2026-10-11 '
        '14:31 UTC, item 3); neither ref(T) nor form(\'.\', ...)', () {
      final c = compile('''
List(X) ::= [] ; [X | List(X)].
procedure (List(String)?)*names(List(String)).
(Ns)*names(Ns?).
''');
      expect(c.widgets['List(String)?'], 'input_list(text)');
      final s = c.source;
      expect(listEnd, '[end]');
      expect(
          s,
          contains('answer_list_string(P, [], [input(_, P1, [end]) | Gs], '
              'Gs?, done) :- P1? =?= P? | true.'));
      expect(s, isNot(contains('input(_, P1, [])')));
      expect(
          s,
          contains('take_list_string(formed(V), P, [V? | X1?], _, Gs, Gs1?, '
              'Done?) :- answer_list_string(P?, X1, Gs?, Gs1, Done).'));
      expect(s, isNot(contains('ref(')));
      expect(s, isNot(contains("form('.'")));
    });

    test('a List of no parameter is a list too', () {
      final c = compile('''
List ::= [] ; [Integer | List].
procedure (List?)*numbers(List).
(Ns)*numbers(Ns?).
''');
      expect(c.widgets['List?'], 'input_list(number)');
    });

    test('a stream is not ended by the person: no clause takes []', () {
      final c = compile('''
procedure (Stream(String)?)*lines(Stream(String)).
(Ls)*lines(Ls?).
''');
      expect(c.widgets['Stream(String)?'], 'input_box(text)');
      expect(c.source, isNot(contains('answer_stream_string(P, [], ')));
    });
  });

  group('Task B (2): a recursive type the person writes that is neither a '
      'Stream nor a List', () {
    test('is refused by the compiler, the message naming the type, at the '
        'interactive type', () {
      const tree = '''
Tree ::= leaf ; node(Tree, Tree).
procedure (Tree?)*plant(Tree).
(T)*plant(T?).
''';
      final (l1, c1) = at(tree, 'procedure');
      expect(
          () => compile(tree),
          refusedAt(
              'The interactive type Tree? is not flat: the person writes '
              'Tree, a recursive type that is neither a Stream nor a List, '
              'and an interactive type is flat, "it has no other recursive '
              'type" (vGLP, Definition "Volitional Procedure, Interactive '
              'Type, Interactive Term, Ordinary Clause, vGLP Program")',
              l1,
              c1));
      const names = '''
Names ::= [] ; [String | Names].
procedure (Names?)*name(Names).
(Ns)*name(Ns?).
''';
      final (l2, c2) = at(names, 'procedure');
      expect(
          () => compile(names),
          refusedAt(
              'the person writes Names, a recursive type that is neither a '
              'Stream nor a List, and an interactive type is flat, "it has no '
              'other recursive type"',
              l2,
              c2));
    });

    test('a list cell the person writes outside a Stream or a List is refused '
        'too, and no form(\'.\', ...) is emitted', () {
      const pair = '''
Nil ::= [].
Pair ::= [Integer | Nil].
procedure (Pair?)*pair(Pair).
(P)*pair(P?).
''';
      final (l, c) = at(pair, 'procedure');
      expect(
          () => compile(pair),
          refusedAt(
              'The interactive type Pair? is not flat: the person writes '
              'Pair, a list cell of which is neither a Stream nor a List, and '
              'an interactive type is flat, "built from constants, String, '
              'Integer, Real, Key, Constant, Module, named types so built, '
              'tuples and unions, and Stream or List"',
              l,
              c));
    });
  });

  // vGLP at 66b6500, Definition "Volitional Procedure, Interactive Type,
  // Interactive Term, Ordinary Clause, vGLP Program": "An interactive type
  // is flat: it is built from constants, String, Integer, Real, Key,
  // Constant, Module, named types so built, tuples and unions, and Stream or
  // List as the whole type or as an argument of a tuple, never under another
  // Stream or List; it has no other recursive type, and no type parameter at
  // a position the person writes."  vGLP #7 Cowork, 2026-10-11 14:31 UTC,
  // item 2: refused "a stream or list inside an element of a stream or list
  // the person writes; a list cell outside Stream and List; a recursive type
  // other than Stream and List, wherever it holds a question, program-written
  // structures included".
  group('flat interactive types (vGLP #7 Cowork, 2026-10-11 14:31 UTC, item '
      '2)', () {
    test('a stream or a list that is the element of a stream or a list the '
        'person writes is refused', () {
      const streams = '''
procedure (Stream(Stream(String))?)*lines(Stream(Stream(String))).
(Ls)*lines(Ls?).
''';
      expect(
          () => compile(streams),
          refusedAt(
              'The interactive type Stream(Stream(String))? is not flat: the '
              'person writes Stream(Stream(String)), whose element '
              'Stream(String) is a stream or a list, and an interactive type '
              'is flat, "never under another Stream or List" (vGLP, '
              'Definition "Volitional Procedure, Interactive Type, '
              'Interactive Term, Ordinary Clause, vGLP Program")',
              1,
              1));
      const lists = '''
List(X) ::= [] ; [X | List(X)].
procedure (List(List(Integer))?)*rows(List(List(Integer))).
(Rs)*rows(Rs?).
''';
      final (l, c) = at(lists, 'procedure');
      expect(
          () => compile(lists),
          refusedAt(
              'the person writes List(List(Integer)), whose element '
              'List(Integer) is a stream or a list',
              l,
              c));
    });

    test('a tuple \'.\'(H, T) is a list cell, refused outside a Stream or a '
        'List where it holds a question', () {
      const src = '''
Nil ::= [].
T ::= t('.'(Integer?, Nil)).
procedure (T)*cell(Integer).
(t([N]))*cell(N?).
''';
      final (l, c) = at(src, 'procedure');
      expect(
          () => compile(src),
          refusedAt(
              "the program writes '.'(Integer?, Nil), a list cell of which is "
              'neither a Stream nor a List, holding a question',
              l,
              c));
    });

    test('a Stream or a List that is an alternative of a union, neither the '
        'whole type nor an argument of a tuple, is refused', () {
      const src = '''
Maybe ::= none ; Stream(Integer).
procedure (Maybe?)*maybe(Maybe).
(M)*maybe(M?).
''';
      final (l, c) = at(src, 'procedure');
      expect(
          () => compile(src),
          refusedAt(
              'The interactive type Maybe? is not flat: the person writes '
              'Maybe, a union one of whose alternatives is Stream(Integer), '
              'neither the whole type nor an argument of a tuple, and an '
              'interactive type is flat, "Stream or List as the whole type or '
              'as an argument of a tuple"',
              l,
              c));
    });

    test('a recursive type other than a Stream or a List that holds a '
        'question is refused in a structure the program writes too', () {
      const src = '''
YesNo ::= yes ; no.
Tree ::= leaf ; node(Tree, YesNo?).
Card ::= card(Integer, Tree).
procedure (Card)*grow(YesNo).
(card(1, node(leaf, A)))*grow(A?).
''';
      final (l, c) = at(src, 'procedure');
      expect(
          () => compile(src),
          refusedAt(
              'The interactive type Card is not flat: the program writes '
              'Tree, a recursive type that is neither a Stream nor a List, '
              'holding a question, and an interactive type is flat, "it has '
              'no other recursive type"',
              l,
              c));
      // Recursive through a Stream: the element is the type again.
      const through = '''
Thread ::= post(String, Stream(Thread)).
procedure (Thread?)*reply(Thread).
(T)*reply(T?).
''';
      final (l2, c2) = at(through, 'procedure');
      expect(
          () => compile(through),
          refusedAt(
              'the person writes Thread, a recursive type that is neither a '
              'Stream nor a List, and an interactive type is flat, "it has no '
              'other recursive type"',
              l2,
              c2));
    });

    // Until vGLP #8 Cowork's ruling of 2026-10-11 15:47 UTC (item 1) a
    // structure the program writes holding no question was shown as it was
    // written, a recursive type and a list cell there not refused; the flat
    // refusals now hold over the whole interactive type.
    test('a structure the program writes holding no question is refused too '
        'where it is not flat, each refusal at the interactive type\'s line '
        'and column (vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 1)', () {
      // A recursive type other than a Stream or a List: sGLP's Peers of
      // coins.vglp and graph.vglp until their rewrite.
      const recursive = '''
YesNo ::= yes ; no.
Peers ::= [] ; [Constant | Peers].
Menu ::= menu(Peers, YesNo?).
procedure (Menu)*choose(YesNo).
(menu([a, b], A))*choose(A?).
''';
      final (l1, c1) = at(recursive, 'procedure');
      expect(
          () => compile(recursive),
          refusedAt(
              'The interactive type Menu is not flat: the program writes '
              'Peers, a recursive type that is neither a Stream nor a List, '
              'and an interactive type is flat, "it has no other recursive '
              'type" (vGLP, Definition "Volitional Procedure, Interactive '
              'Type, Interactive Term, Ordinary Clause, vGLP Program")',
              l1,
              c1));
      // A list cell outside a Stream or a List.
      const cell = '''
YesNo ::= yes ; no.
Nil ::= [].
Pair ::= [Integer | Nil].
Menu ::= menu(Pair, YesNo?).
procedure (Menu)*choose(YesNo).
(menu([1], A))*choose(A?).
''';
      final (l2, c2) = at(cell, 'procedure');
      expect(
          () => compile(cell),
          refusedAt(
              'The interactive type Menu is not flat: the program writes '
              'Pair, a list cell of which is neither a Stream nor a List, and '
              'an interactive type is flat, "built from constants, String, '
              'Integer, Real, Key, Constant, Module, named types so built, '
              'tuples and unions, and Stream or List" (vGLP, Definition',
              l2,
              c2));
      // A stream under a stream.
      const nested = '''
YesNo ::= yes ; no.
Feed ::= feed(Stream(Stream(String)), YesNo?).
procedure (Feed)*see(YesNo).
(feed([["a"]], A))*see(A?).
''';
      final (l3, c3) = at(nested, 'procedure');
      expect(
          () => compile(nested),
          refusedAt(
              'The interactive type Feed is not flat: the program writes '
              'Stream(Stream(String)), whose element Stream(String) is a '
              'stream or a list, and an interactive type is flat, "never '
              'under another Stream or List" (vGLP, Definition',
              l3,
              c3));
      // A Stream as an alternative of a union.
      const union = '''
YesNo ::= yes ; no.
Maybe ::= none ; Stream(Integer).
Card ::= card(Maybe, YesNo?).
procedure (Card)*see(YesNo).
(card(none, A))*see(A?).
''';
      final (l4, c4) = at(union, 'procedure');
      expect(
          () => compile(union),
          refusedAt(
              'The interactive type Card is not flat: the program writes '
              'Maybe, a union one of whose alternatives is Stream(Integer), '
              'neither the whole type nor an argument of a tuple, and an '
              'interactive type is flat, "Stream or List as the whole type or '
              'as an argument of a tuple" (vGLP, Definition',
              l4,
              c4));
      // A writer-mode type that holds no question at all, refused the same.
      const none = '''
Nil ::= [].
Pair ::= [Integer | Nil].
Note ::= note(Pair).
procedure (Note)*tell(Integer?).
(note([N?]))*tell(N).
''';
      final (l5, c5) = at(none, 'procedure');
      expect(
          () => compile(none),
          refusedAt(
              'The interactive type Note is not flat: the program writes '
              'Pair, a list cell of which is neither a Stream nor a List, and '
              'an interactive type is flat',
              l5,
              c5));
    });

    test('its flat form, a Stream or a List or a named type defined as one, '
        'is shown as a thread, and no ref(T) or form(\'.\', ...) is emitted '
        '(sGLP\'s Peers and Wallet written List(Peer) and List(Holding))',
        () {
      final c = compile('''
Peer ::= Constant.
List(X) ::= [] ; [X | List(X)].
Holding ::= holding(Peer, Integer).
Peers ::= List(Peer).
Wallet ::= List(Holding).
Request ::= pay(Peer, Integer) ; swap(Peer, Integer).
Menu ::= menu(Peers, Wallet, Request?).
procedure (Menu)*ask(Peers?, Wallet?, Request).
(menu(Fs?, W?, R))*ask(Fs, W, R?).
''');
      expect(
          c.widgets['Menu'],
          // Wallet's elements are tuples the program writes, each a form
          // labelled by its arguments' declared types (Definition "Widget
          // Declaration, Default Widget"; vGLP #8 Cowork, 2026-10-13 08:15
          // UTC).
          "form(menu, [field('Peers', thread), field('Wallet', "
          "thread(form(holding, [field('Peer', shown), field('Integer', "
          "shown)]))), "
          "field('Request', menu([form(pay, [field('Peer', peer), "
          "field('Integer', number)]), form(swap, [field('Peer', peer), "
          "field('Integer', number)])]))])");
      expect(c.source, isNot(contains('ref(')));
      expect(c.source, isNot(contains("form('.'")));
    });

    test('a Stream a scope\'s definition names by its instantiation, '
        'Stream<Integer>, is the Stream it is: not refused as a recursive '
        'type where the program writes it (bonds_agent.vglp\'s SwapOffer, '
        'of its directory\'s self.glp)', () {
      final tmp = Directory.systemTemp.createTempSync('vglp_scope_');
      addTearDown(() => tmp.deleteSync(recursive: true));
      final self = File('${tmp.path}/self.glp')
        ..writeAsStringSync('Offer ::= offer(Integer, Stream(Integer)).\n');
      final inScope = buildAncestorScope(
          chain: [self.path],
          rootSelfGlpPath: File('$_programs/self.glp').absolute.path);
      final c = compileCanonical('''
YesNo ::= yes ; no.
Card ::= card(Offer, YesNo?).
procedure (Card)*see(Offer?, YesNo).
(card(O?, A))*see(O, A?).
''', dispatcher: dispatcher, scope: inScope);
      // The offer the program writes, a tuple, a form labelled by its
      // arguments' declared types (vGLP #8 Cowork, 2026-10-13 08:15 UTC).
      expect(
          c.widgets['Card'],
          "form(card, [field('Offer', form(offer, [field('Integer', shown), "
          "field('Stream(Integer)', thread)])), "
          "field('YesNo', buttons([yes, no]))])");
    });

    test('the type _ at a position the person writes is refused, none of the '
        'Definition\'s building blocks (vGLP #8 Cowork, 2026-10-11 15:47 UTC, '
        'item 3)', () {
      const why = 'is not flat: the person writes a position of type _, none '
          'of the types an interactive type is built from, and an '
          'interactive type is flat, "built from constants, String, Integer, '
          'Real, Key, Constant, Module, named types so built, tuples and '
          'unions, and Stream or List" (vGLP, Definition "Volitional '
          'Procedure, Interactive Type, Interactive Term, Ordinary Clause, '
          'vGLP Program")';
      // A field of a form the person writes.
      const field = '''
Note ::= note(String, _).
procedure (Note?)*note(Note).
(N)*note(N?).
''';
      final (l1, c1) = at(field, 'procedure');
      expect(() => compile(field),
          refusedAt('The interactive type Note? $why', l1, c1));
      // A question inside a structure the program writes.
      const inside = '''
Card ::= card(Integer, _?).
procedure (Card)*see(_).
(card(1, X))*see(X?).
''';
      final (l2, c2) = at(inside, 'procedure');
      expect(() => compile(inside),
          refusedAt('The interactive type Card $why', l2, c2));
      // The element of a stream the person writes.
      const stream = '''
procedure (Stream(_)?)*any(Stream(_)).
(Xs)*any(Xs?).
''';
      expect(() => compile(stream),
          refusedAt('The interactive type Stream(_)? $why', 1, 1));
    });

    test('the type _ at a position the program writes is refused too, the '
        'type _ refused everywhere in an interactive type (vGLP #8 Cowork, '
        '2026-10-11 17:24 UTC, Q2)', () {
      const why = 'is not flat: the program writes a position of type _, '
          'none of the types an interactive type is built from, and an '
          'interactive type is flat, "built from constants, String, Integer, '
          'Real, Key, Constant, Module, named types so built, tuples and '
          'unions, and Stream or List" (vGLP, Definition "Volitional '
          'Procedure, Interactive Type, Interactive Term, Ordinary Clause, '
          'vGLP Program")';
      // Beside a question, in a structure the program writes.
      const beside = '''
YesNo ::= yes ; no.
Card ::= card(_, YesNo?).
procedure (Card)*see(YesNo).
(card(1, A))*see(A?).
''';
      final (l1, c1) = at(beside, 'procedure');
      expect(() => compile(beside),
          refusedAt('The interactive type Card $why', l1, c1));
      // In a structure the program writes that holds no question.
      const note = '''
Note ::= note(_).
procedure (Note)*tell(Integer?).
(note(N?))*tell(N).
''';
      final (l2, c2) = at(note, 'procedure');
      expect(() => compile(note),
          refusedAt('The interactive type Note $why', l2, c2));
      // The element of a stream the program writes.
      const thread = '''
procedure (Stream(_))*tell(Stream(_)?).
(Xs?)*tell(Xs).
''';
      expect(() => compile(thread),
          refusedAt('The interactive type Stream(_) $why', 1, 1));
    });

    test('a position the person writes of a type parameter is refused '
        'outright, the message citing the Definition (vGLP #7 Cowork, '
        '2026-10-11 14:31 UTC, item 1)', () {
      const box = '''
Box(X) ::= box(X).
procedure(X) (Box(X)?)*take(X).
(box(V))*take(V?).
''';
      final (l1, c1) = at(box, 'procedure');
      const why = 'The interactive type Box(X)? is refused: the person writes '
          'a position of type X, a type parameter, and an interactive type has '
          '"no type parameter at a position the person writes" (vGLP, '
          'Definition "Volitional Procedure, Interactive Type, Interactive '
          'Term, Ordinary Clause, vGLP Program")';
      expect(() => compile(box), refusedAt(why, l1, c1));
      // The whole type a parameter, and a stream's element one.
      const whole = '''
procedure(X) (X?)*take(X).
(V)*take(V?).
''';
      expect(
          () => compile(whole),
          refusedAt(
              'The interactive type X? is refused: the person writes a '
              'position of type X, a type parameter',
              1,
              1));
      const stream = '''
procedure(E) (Stream(E)?)*takes(Stream(E)).
(Vs)*takes(Vs?).
''';
      expect(
          () => compile(stream),
          refusedAt(
              'The interactive type Stream(E)? is refused: the person writes '
              'a position of type E, a type parameter',
              1,
              1));
      // At a position the program writes a parameter is shown.
      final c = compile('''
YesNo ::= yes ; no.
Card(X) ::= card(X, YesNo?).
procedure(X) (Card(X))*see(X?, YesNo).
(card(V?, A))*see(V, A?).
''');
      expect(c.widgets['Card(X)'],
          "form(card, [field('X', shown), field('YesNo', buttons([yes, no]))])");
    });
  });

  // vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 2: "An alias of a Stream or
  // List, `Lines ::= Stream(String)`, is a named type so built and is NOT
  // refused: it draws as the stream it names, like Date and Key draw by
  // their names."
  group('an alias of a Stream or a List (vGLP #7 Cowork, 2026-10-11 14:31 '
      'UTC, item 2)', () {
    test('draws as the stream it names, and is answered as it, an element '
        'per grant', () {
      final c = compile('''
Lines ::= Stream(String).
procedure (Lines?)*write(Lines).
(Ls)*write(Ls?).
''');
      expect(c.widgets['Lines?'], 'input_box(text)');
      final s = c.source;
      expect(
          s,
          contains('take_lines(formed(V), P, [V? | X1?], _, Gs, Gs1?, Done?) '
              ':- answer_lines(P?, X1, Gs?, Gs1, Done).'));
      expect(s, isNot(contains("form('.'")));
      expect(s, isNot(contains('answer_lines(P, [], ')));
    });

    test('of a List, with its end button, through two names', () {
      final c = compile('''
List(X) ::= [] ; [X | List(X)].
Names ::= List(String).
Roll ::= Names.
procedure (Roll?)*call(Roll).
(R)*call(R?).
''');
      expect(c.widgets['Roll?'], 'input_list(text)');
      expect(
          c.source,
          contains('answer_roll(P, [], [input(_, P1, [end]) | Gs], Gs?, done) '
              ':- P1? =?= P? | true.'));
    });

    test('a field of a form the person writes, a question of its own; and '
        'one the program writes, a thread', () {
      final c = compile('''
Lines ::= Stream(String).
YesNo ::= yes ; no.
Post ::= post(String, Lines).
Feed ::= feed(Lines, YesNo?).
procedure (Post?)*post(Post).
(P)*post(P?).
procedure (Feed)*feed(YesNo).
(feed(["a"], A))*feed(A?).
''');
      expect(c.widgets['Post?'],
          "form(post, [field('String', text), question('Lines', input_box(text))])");
      expect(c.widgets['Feed'],
          "form(feed, [field('Lines', thread), field('YesNo', buttons([yes, no]))])");
    });
  });

  group('Task B (3): a stream or a list inside a form the person writes', () {
    const post = '''
List(X) ::= [] ; [X | List(X)].
Post ::= post(String, Stream(String)) ; tags(List(Constant)) ; quit.
procedure (Post?)*poster(Post).
(P)*poster(P?).
''';

    test('is a question of its own, drawn under the form: question(L, W), and '
        'no input_box as a field', () {
      final c = compile(post);
      expect(
          c.widgets['Post?'],
          "menu([form(post, [field('String', text), "
          "question('Stream(String)', input_box(text))]), "
          "form(tags, [question('List(Constant)', input_list(text))]), "
          'button(quit)])');
      expect(c.widgets['Post?'], isNot(contains("field('Stream(String)'")));
    });

    test('the form\'s answer carries a fresh variable there, answered from '
        'the grants that follow at its own position, and the construct is '
        'drawn again with the answer', () {
      final s = compile(post).source;
      expect(
          s,
          contains('construct(Id, post_r(X?), Gs, Ds?) :- '
              'answer_post([], X, Gs?, _, Vs, Done), run_post_r(Id?, menu('));
      expect(s, contains('[input([]) | Vs?], Done?, Ds).'));
      // Formed with [] there, not from the input.
      expect(
          s,
          contains('form_post(post(R1, _), F?) :- form_string(R1?, F1), '
              'join_post_post_2(F1?, F).'));
      expect(s, contains('join_post_post_2(formed(X1), formed(post(X1?, []))).'));
      expect(s, contains('form_post(tags(_), formed(tags([]))).'));
      expect(
          s,
          contains('take_post(formed(V), P, X?, _, Gs, Gs1?, [W?], Done?) :- '
              'open_post(V?, P?, X, Gs?, Gs1, W, Done).'));
      expect(
          s,
          contains('open_post(post(H1, _), P, post(H1?, Y2?), Gs, Gs1?, '
              'post(H1?, input(M2?)), D2?) :- ground(P?), ground(H1?) | '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_stream_string(Q2?, Y2, Gs?, Gs1, D2).'));
      expect(
          s,
          contains('open_post(tags(_), P, tags(Y1?), Gs, Gs1?, '
              'tags(input(M1?)), D1?) :- ground(P?) | path(P?, 1, M1), '
              'path(P?, 1, Q1), answer_list_constant(Q1?, Y1, Gs?, Gs1, D1).'));
      expect(
          s,
          contains('open_post(V, _, X?, Gs, Gs?, W?, done) :- ground(V?) | '
              'X = V?, W = V?.'));
    });

    test('in a form inside the form, at the path down to it', () {
      final c = compile('''
List(X) ::= [] ; [X | List(X)].
Wrap ::= wrap(Integer, List(Integer)).
Order ::= order(Integer, Wrap).
procedure (Order?)*order(Order).
(O)*order(O?).
''');
      expect(
          c.widgets['Order?'],
          "form(order, [field('Integer', number), field('Wrap', form(wrap, "
          "[field('Integer', number), question('List(Integer)', "
          'input_list(number))]))])');
      expect(
          c.source,
          contains('open_order(order(H1, H2), P, order(H1?, X2?), Gs, Gs1?, '
              'order(H1?, V2?), D2?) :- ground(H1?) | path(P?, 2, Q2), '
              'open_wrap(H2?, Q2?, X2, Gs?, Gs1, V2, D2).'));
    });

    test('in a question of a structure the program writes, drawn again once '
        'answered', () {
      final s = compile('''
Peer ::= Constant.
Reply ::= reply(String, Stream(String)).
Card ::= card(Peer, Reply?).
procedure (Card)*respond(Reply).
(card(bob, R))*respond(R?).
''').source;
      expect(
          s,
          contains('present_card(P, card(H1, H2?), Gs, Gs1?, Vs?, D2?) :- '
              'ground(P?) | path(P?, 1, Q1), shown(Q1?, H1?, V1), '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_reply(Q2?, H2, Gs?, Gs1, W2, D2), '
              'start_card_card_2(V1?, [input(M2?) | W2?], Vs).'));
    });

    test('is refused in an element of a stream or a list, whose elements '
        'share one position, the type not flat (vGLP #7 Cowork, 2026-10-11 '
        '14:31 UTC, item 2)', () {
      const src = '''
Req ::= post(String, Stream(String)).
procedure (Stream(Req)?)*agent(Stream(Req)).
(Rs)*agent(Rs?).
''';
      final (l, c) = at(src, 'procedure');
      expect(
          () => compile(src),
          refusedAt(
              'The interactive type Stream(Req)? is not flat: the person '
              'writes Stream(Req), whose element Req holds a stream or a '
              'list, and an interactive type is flat, "never under another '
              'Stream or List"',
              l,
              c));
    });
  });

  group('Task B (4): a position of type Constant or Module draws a text field',
      () {
    test('as String does', () {
      final c = compile('''
Tag ::= tag(Constant, Module, String).
procedure (Tag?)*tag(Tag).
(T)*tag(T?).
''');
      expect(
          c.widgets['Tag?'],
          "form(tag, [field('Constant', text), field('Module', text), "
          "field('String', text)])");
    });
  });

  // A tuple the program writes: "a form with one field per argument, each
  // labelled by the name of its declared type", and "a value the program
  // writes: shown and not edited" (vGLP, Definition "Widget Declaration,
  // Default Widget"); a tuple inside it a form of its own, by the induction
  // of Proposition "Default Widgets" (vGLP #8 Cowork, 2026-10-13 08:15 UTC,
  // item b).
  group('a tuple the program writes is a form of shown fields, each labelled',
      () {
    test('coins\' card: the offer, its lot and its coins, each field '
        'labelled by its declared type', () {
      final c = compile('''
YesNo ::= yes ; no.
Coin ::= coin(Key, Integer).
Lot ::= lot(Key, Integer).
Offer ::= offer(Key, Lot, Stream(Coin)).
Card ::= card(Offer, YesNo?).
procedure (Card)*see(Offer?, YesNo).
(card(O?, A))*see(O, A?).
''');
      expect(
          c.widgets['Card'],
          "form(card, [field('Offer', form(offer, [field('Key', shown), "
          "field('Lot', form(lot, [field('Key', shown), field('Integer', "
          "shown)])), field('Stream(Coin)', thread(form(coin, "
          "[field('Key', shown), field('Integer', shown)])))])), "
          "field('YesNo', buttons([yes, no]))])");
    });

    test('a union the program writes is a menu, each alternative by its '
        'widget; one of constants and primitives alone stays shown', () {
      final c = compile('''
YesNo ::= yes ; no.
Kind ::= big ; small.
Item ::= coin(Key, Integer) ; bond(Key, Integer, Integer) ; none.
Offer ::= offer(Kind, Item).
Card ::= card(Offer, YesNo?).
procedure (Card)*see(Offer?, YesNo).
(card(O?, A))*see(O, A?).
''');
      expect(
          c.widgets['Card'],
          "form(card, [field('Offer', form(offer, [field('Kind', shown), "
          "field('Item', menu([form(coin, [field('Key', shown), "
          "field('Integer', shown)]), form(bond, [field('Key', shown), "
          "field('Integer', shown), field('Integer', shown)]), shown]))])), "
          "field('YesNo', buttons([yes, no]))])");
    });

    test('a form the person writes is labelled as before', () {
      final c = compile('''
Lot ::= lot(Key, Integer).
Swap ::= swap(Key, Lot).
procedure (Swap?)*ask(Swap).
(S)*ask(S?).
''');
      expect(
          c.widgets['Swap?'],
          "form(swap, [field('Key', peer), field('Lot', form(lot, "
          "[field('Key', peer), field('Integer', number)]))])");
    });
  });

  group('Task B (5): a position of type Key, or of a type defined as Key, '
      'draws a peer field, whatever its name', () {
    test('Key, Issuer ::= Key and Owner ::= Issuer; a String is a text field',
        () {
      final c = compile('''
Issuer ::= Key.
Owner ::= Issuer.
Name ::= String.
Lot ::= lot(Key, Issuer, Owner, Name, Integer).
procedure (Lot?)*lot(Lot).
(L)*lot(L?).
procedure (Key?)*key(Key).
(K)*key(K?).
''');
      expect(
          c.widgets['Lot?'],
          "form(lot, [field('Key', peer), field('Issuer', peer), "
          "field('Owner', peer), field('Name', text), field('Integer', "
          'number)])');
      expect(c.widgets['Key?'], 'peer');
    });
  });

  group('Task B (6): a form field is labelled by the name of its declared '
      'type', () {
    test('in a form the person writes and in one the program writes', () {
      final c = compile('''
Peer ::= Constant.
Date ::= Integer.
YesNo ::= yes ; no.
Pay ::= pay(Peer, Integer, Date, Real).
Offer ::= offer(Peer, Stream(Integer), YesNo?).
procedure (Pay?)*pay(Pay).
(P)*pay(P?).
procedure (Offer)*offer(YesNo).
(offer(bob, [1, 2], A))*offer(A?).
''');
      expect(
          c.widgets['Pay?'],
          "form(pay, [field('Peer', peer), field('Integer', number), "
          "field('Date', date), field('Real', number)])");
      expect(
          c.widgets['Offer'],
          "form(offer, [field('Peer', shown), field('Stream(Integer)', "
          "thread), field('YesNo', buttons([yes, no]))])");
    });
  });

  group('a string in the compiled program', () {
    // One string constant, 'x' and "x" one value and '"x"' the
    // three-character string, printed by GLP's printer in its one canonical
    // form, single-quoted and bare where the lexer reads it so, the
    // double-quoted form read and never printed (GLP-Spec appendix-lp.tex,
    // Definition "Logic Programs Syntax"; vGLP #5 Cowork, 2026-10-07 20:01
    // UTC, item 2): in the program's types and clauses, in the construct
    // process's buttons and formers, and in the module read back.
    const choice = '''
Choice ::= 'Yes please' ; "no thanks" ; '"x"'.
procedure (Choice?)*pick(String).
('Yes please')*pick('"x"').
("no thanks")*pick("x").
''';

    test('is printed in its one canonical form', () {
      final s = compile(choice).source;
      expect(s, contains("Choice ::= 'Yes please' ; 'no thanks' ; '\"x\"'."));
      expect(s, contains("pick1('\"x\"', 'Yes please', [])."));
      expect(s, contains("pick1(x, 'no thanks', [])."));
      expect(s, contains("form_choice('no thanks', formed('no thanks'))."));
      expect(s, contains("form_choice('\"x\"', formed('\"x\"'))."));
      expect(s, isNot(contains('"no thanks"')));
      expect(s, isNot(contains('pick1("x"')));
    });

    test('its default widget names its constants so', () {
      expect(compile(choice).widgets['Choice?'],
          "buttons(['Yes please', 'no thanks', '\"x\"'])");
    });

    test('reads back as the same string', () {
      final m = compile(choice).module;
      final pick1 = m.procedures.singleWhere((p) => p.name == 'pick1');
      expect([
        for (final c in pick1.clauses)
          [for (final a in c.head.args.take(2)) (a as ConstTerm).value]
      ], [
        ['"x"', 'Yes please'],
        ['x', 'no thanks'],
      ]);
    });
  });

  group('what is not built', () {
    test('a position the program writes inside one the person writes', () {
      expect(() => compile('''
Reply ::= done.
Order ::= order(Integer, Reply?).
procedure (Order?)*order(Order).
(O)*order(O?).
'''), refused('a position of it is written by the program'));
    });

    test('a position the person writes of a type parameter: refused '
        'outright since vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 1 (the '
        'flat types above)', () {
      expect(() => compile('''
Box(X) ::= box(X).
procedure(X) (Box(X)?)*take(X).
(box(V))*take(V?).
'''), refusedAt('the person writes a position of type X, a type parameter',
          2, 1));
    });
  });

  // vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D: "A stream or list
  // the program writes whose elements hold questions, as the paper has it:
  // Present hands the person each element with the writers inside it
  // (Definition "vmaGLP Transition System"), each such writer a question
  // (Definition "Question"), drawn by Definition "Widget Declaration" (a
  // stream the program writes: a list or a thread; a question by the widget
  // of its type).  Draw each element as it is presented and take the
  // answers to its questions as any grant."  Not built until then, refused
  // as "a stream the program writes whose elements hold questions".  The
  // runs are elicitation_test's and glp_multiagent's bridge_test's, on
  // programs/tests/vglp/cards.
  group('Task D: a stream or a list the program writes whose elements hold '
      'questions', () {
    test('is built: its widget a thread of the element type\'s widget, each '
        'element presented at its own position as it arrives, the element '
        'of a list cell at P with 1 at its end and its tail at P with 2', () {
      final c = compile('''
YesNo ::= yes ; no.
Card ::= card(Integer, YesNo?).
procedure (Stream(Card))*cards.
(Cs?)*cards :- deal(Cs).
procedure deal(Stream(Card)).
deal([]).
''');
      const card = "form(card, [field('Integer', shown), field('YesNo', "
          'buttons([yes, no]))])';
      expect(c.widgets['Stream(Card)'], 'thread($card)');
      final s = c.source;
      expect(
          s,
          contains('construct(Id, stream_card_w(X), Gs, Ds?) :- '
              'present_stream_card([], X?, Gs?, _, Vs, Done), '
              'run_stream_card_w(Id?, thread($card), Vs?, Done?, Ds).'));
      // The views, the elements' so far: a cell as it is written; [] for
      // the stream closed with none; and the unwritten tail not drawn, []
      // where no element is written yet, its grants passed on until it is
      // (vGLP 118e04d, Definition "Construct Process").
      expect(
          s,
          contains('present_stream_card(P, [X | Xs], Gs, Gs1?, Vs?, Done?) :- '
              'present_stream_card_cell(P?, X?, Xs?, Gs?, Gs1, Vs, Done).'));
      expect(s,
          contains('present_stream_card(_, [], Gs, Gs?, [[]], done).'));
      expect(
          s,
          contains('present_stream_card(P, Xs, Gs, Gs1?, [[] | Vs?], '
              'Done?) :- present_stream_card_tail(P?, Xs?, Gs?, Gs1, Vs, '
              'Done).'));
      expect(
          s,
          contains('present_stream_card_tail(P, Xs, [G | Gs], [G? | Gs1?], '
              'Vs?, Done?) :- present_stream_card_tail(P?, Xs?, Gs?, Gs1, Vs, '
              'Done).'));
      // Done once the stream is closed and every element answered.
      expect(s,
          contains('present_stream_card_tail(_, [], Gs, Gs?, [], done).'));
      // Each element presented at P with 1 at its end, its tail at P with
      // 2, the grants it does not take passed on to those after it, and the
      // cell drawn again as its element's view or its tail's changes.
      expect(
          s,
          contains('present_stream_card_cell(P, X, Xs, Gs, Gs2?, Vs?, Done?) '
              ':- ground(P?) | path(P?, 1, Q1), '
              'present_card(Q1?, X?, Gs?, Gs1, V1, D1), path(P?, 2, Q2), '
              'present_stream_card(Q2?, Xs?, Gs1?, Gs2, V2, D2), '
              'cons_views(V1?, V2?, Vs), all_done([D1?, D2?], Done).'));
      expect(s, isNot(contains('ref(')));
      expect(s, isNot(contains('append_one')));
    });

    test('as an argument of a tuple, a List: the form\'s field a thread, the '
        'tuple\'s views combined as its list\'s change', () {
      final c = compile('''
YesNo ::= yes ; no.
Card ::= card(Integer, YesNo?).
List(X) ::= [] ; [X | List(X)].
Panel ::= panel(List(Card)).
procedure (Panel)*show(YesNo, YesNo).
(panel([card(1, A1), card(2, A2)]))*show(A1?, A2?).
''');
      expect(
          c.widgets['Panel'],
          "form(panel, [field('List(Card)', thread(form(card, "
          "[field('Integer', shown), field('YesNo', buttons([yes, no]))])))])");
      expect(
          c.source,
          contains('present_panel(P, panel(H1), Gs, Gs1?, Vs?, D1?) :- '
              'path(P?, 1, Q1), present_list_card(Q1?, H1?, Gs?, Gs1, V1, '
              'D1), start_panel_panel_1(V1?, Vs).'));
    });

    test('an element that is itself a question: the program hands the person '
        'its writer, marked input(P) in the view and answered there', () {
      final c = compile('''
YesNo ::= yes ; no.
procedure (Stream(YesNo?))*ballot(YesNo, YesNo).
([A1, A2])*ballot(A1?, A2?).
''');
      expect(c.widgets['Stream(YesNo?)'], 'thread(buttons([yes, no]))');
      expect(
          c.source,
          contains('present_stream_yesNo(P, [X? | Xs], Gs, Gs1?, Vs?, '
              'Done?) :- present_stream_yesNo_cell(P?, X, Xs?, Gs?, Gs1, Vs, '
              'Done).'));
      expect(
          c.source,
          contains('present_stream_yesNo_cell(P, X?, Xs, Gs, Gs2?, Vs?, '
              'Done?) :- ground(P?) | path(P?, 1, M1), '
              'path(P?, 1, Q1), answer_yesNo(Q1?, X, Gs?, Gs1, D1), '
              'path(P?, 2, Q2), present_stream_yesNo(Q2?, Xs?, Gs1?, Gs2, V2, '
              'D2), cons_views([input(M1?)], V2?, Vs), all_done([D1?, D2?], '
              'Done).'));
    });

    test('an element holding a stream or a list is refused, the type not '
        'flat', () {
      const src = '''
YesNo ::= yes ; no.
Ballot ::= ballot(Stream(YesNo?)).
procedure (Stream(Ballot))*ballots.
(Bs?)*ballots :- none(Bs).
procedure none(Stream(Ballot)).
none([]).
''';
      final (l, c) = at(src, 'procedure (Stream');
      expect(
          () => compile(src),
          refusedAt(
              'The interactive type Stream(Ballot) is not flat: the program '
              'writes Stream(Ballot), whose element Ballot holds a stream or '
              'a list, holding a question, and an interactive type is flat, '
              '"never under another Stream or List"',
              l,
              c));
    });
  });

  group('widget declarations, T =::= W', () {
    test('are read from the source text, which GLP\'s lexer could not read '
        'with them, and the source is in the paper\'s syntax', () {
      const text = '''
YesNo ::= yes ; no.
YesNo? =::= toggle.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
''';
      expect(isPaperSyntaxSource(text), isTrue);
      final w = extractWidgetDeclarations(text);
      expect(w.byModedType, {'YesNo?': 'toggle'});
      expect(w.stripped.split('\n'), hasLength(text.split('\n').length));
      expect(w.stripped, isNot(contains('=::=')));
    });

    test('name the widget of a question of their moded type, in its mode, '
        'at the interactive type and below it', () {
      final c = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Card ::= card(Peer, YesNo?).
Card =::= inbox_card.
YesNo? =::= toggle.
procedure (Card)*respond(YesNo).
(card(bob, A))*respond(A?).
procedure (Pair)*both(YesNo, YesNo).
(pair(A, B))*both(A?, B?).
Pair ::= pair(YesNo?, YesNo?).
''');
      expect(c.widgets['Card'], 'inbox_card');
      expect(c.widgets['Pair'],
          "form(pair, [field('YesNo', toggle), field('YesNo', toggle)])");
    });

    test('a declaration for the other mode does not apply', () {
      final c = compile('''
YesNo ::= yes ; no.
YesNo =::= lamp.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
''');
      expect(c.widgets['YesNo?'], 'buttons([yes, no])');
    });

    test('a quoted atom names a widget too', () {
      final c = compile('''
YesNo ::= yes ; no.
YesNo? =::= 'Big buttons'.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
''');
      expect(c.widgets['YesNo?'], "'Big buttons'");
    });

    test('a double-quoted string names the same widget as the quoted name', () {
      // 'x' and "x" are two spellings of one string (GLP-Spec
      // appendix-lp.tex, Definition "Logic Programs Syntax").
      String widget(String w) => compile('''
YesNo ::= yes ; no.
YesNo? =::= $w.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
''').widgets['YesNo?']!;
      expect(widget('"Big buttons"'), "'Big buttons'");
      expect(widget('"toggle"'), 'toggle');
      expect(widget('"toggle"'), widget("'toggle'"));
    });

    test('are refused where W is not an atom, T not a moded type, or two name '
        'one moded type', () {
      expect(() => compile('''
YesNo ::= yes ; no.
YesNo? =::= toggle(big).
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
'''), refused('is not an atom naming a widget'));
      expect(() => compile('''
YesNo ::= yes ; no.
yes =::= toggle.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
'''), refused('is not a moded type'));
      expect(() => compile('''
YesNo ::= yes ; no.
YesNo? =::= toggle.
YesNo? =::= lamp.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
'''), refused('Two widget declarations'));
    });

    test('keep the lines of the source, so an error after one is reported '
        'where it is', () {
      expect(() => compile('''
YesNo ::= yes ; no.
YesNo? =::= toggle.
procedure (YesNo?)*ask(Integer?).
(yes)*ask(_).
ask(1, t).
'''), throwsA(isA<CompileError>().having((e) => e.line, 'line', 5)));
    });
  });
  // "A vGLP program declares the type of its output to its person by
  // defining the type Output, a reserved name; the dispatcher draws that
  // output as a thread whose elements are drawn by the widget of Output"
  // (vGLP 169b008, Definition "Canonical Compilation"); a program whose
  // output end is anonymous defines none and draws nothing, the output thread
  // is construct 0 and the questions are numbered from 1, and an Output with
  // a position the person writes is refused, naming it (vGLP, 2026-10-13
  // 19:25 UTC, items 1, 3 and 4).
  group('the output to the person, of the reserved type Output', () {
    test('defined: dispatch/3 typed Stream(Output), the dispatcher given the '
        'thread of Output\'s widget, each tuple a form headed by its functor',
        () {
      final c = compile('''$card
Output ::= said(Peer) ; heard(Peer, Integer) ; ok.
''');
      expect(c.widgets['Output'],
          "thread(menu([form(said, [field('Peer', shown)]), "
          "form(heard, [field('Peer', shown), field('Integer', shown)]), "
          "shown]))");
      expect(
          c.source,
          contains("dispatch(Stream(Ask(Question))?, "
              "PersonChannel(Drawn)?, "
              "Channel(Stream(_), Stream(Output)))."));
      // The compiled program draws its output as construct 0, its value
      // under output, and hands the draws to the generic dispatcher, which
      // numbers the questions from 1 (vGLP, Definition "Construct Process").
      expect(
          c.source,
          contains("dispatch(Asks, PCh, ch(MIn?, MOut)) :- dispatch(Asks?, "
              "PCh?, MIn, ODs?, Ss), output(thread(menu(["));
      expect(c.source, contains('MOut?, ODs), constructs(Ss?).'));
      expect(c.source, contains('[draw(0, W?, output([])) | Ds?]'));
      expect(c.source, contains('[draw(0, W?, output(Es?)) | Ds?]'));
      expect(c.source, contains('merge_draws(ODs?, DOut?, POut)'));
      expect(c.source, contains('[], [], 1, Ss)'));
      expect(c.source, contains('Drawn ::= card_w(_) ; output(List(Output)) ; '
          'YesNo.'));
    });

    test('undefined: dispatch/3 calls the dispatcher that draws nothing of the '
        'output', () {
      final c = compile(card);
      expect(c.widgets.containsKey('Output'), isFalse);
      expect(
          c.source,
          contains("PersonChannel(Drawn)?, "
              "Channel(Stream(_), Stream(_)))."));
      expect(c.source,
          contains('dispatch(Asks, PCh, MCh?) :- dispatch(Asks?, PCh?, MCh, Ss)'));
    });

    test('a primitive Output: a thread of shown elements', () {
      final c = compile('''$card
Output ::= String.
''');
      expect(c.widgets['Output'], 'thread');
    });

    test('an Output with a position the person writes is refused, naming it',
        () {
      expect(
          () => compile('''$card
Output ::= ask(Peer, YesNo?).
'''),
          refused('The type Output, the type of the program\'s output to its '
              'person, has a position the person writes'));
    });
  });
}

