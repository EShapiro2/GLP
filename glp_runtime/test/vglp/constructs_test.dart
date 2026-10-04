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
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/vglp/canonical.dart';
import 'package:glp_runtime/vglp/dispatcher.dart';

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
          containsAll(['Ask', 'Spawn', 'Input', 'Path', 'Draw', 'PersonIn',
              'Inputs', 'Deads']));
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
              'Channel(PersonIn, Stream(_))?, Channel(Stream(_), '
              'Stream(_))).'));
      expect(
          c.source,
          contains('dispatch(Asks, PCh, MCh?) :- dispatch(Asks?, PCh?, MCh, '
              'Ss), constructs(Ss?).'));
      // The generic dispatch/4, parameterised in the questions, unexported.
      expect(
          c.source,
          contains('\nprocedure(Q) dispatch(Stream(Ask(Q))?, '
              'Channel(PersonIn, Stream(_))?, Channel(Stream(_), Stream(_)), '
              'Stream(Spawn(Q))).'));
      expect(c.source, contains('Draw ::= draw(Integer, _, _) ; '
          'withdraw(Integer).'));
      expect(c.source, contains('Ask(Q) ::= ask(Constant, Q).'));
      expect(
          c.source,
          contains('Spawn(Q) ::= spawn(Integer, Q, Inputs, '
              'Stream(Draw)?).'));
      // On an ask it writes the spawn of the construct process with the next
      // identifier, keeping the writer of its grants and the reader of its
      // draws; constructs/1 calls construct/4 on each spawn.
      expect(
          c.source,
          contains('serve([ask(_, Q) | Asks], Is, COut, CInto?, DOut?, Rs, Xs, '
              'N, [spawn(N?, Q?, Gs?, Ds) | Ss?]) :- ground(N?) | '
              'merge(Ds?, CInto1?, CInto)'));
      expect(
          c.source,
          contains('constructs([spawn(Id, Q, Gs, Ds?) | Ss]) :- '
              'construct(Id?, Q?, Gs?, Ds), constructs(Ss?).'));
      expect(c.source, contains('constructs([]).'));
      // A grant is input(Id, P, R), routed whole by Id, and the question at
      // P reads the input from it (C).
      expect(c.source, contains('Input ::= input(_, _, _).'));
      expect(c.source, contains('Path ::= [] ; [Integer | Path].'));
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
      expect(c.source, contains('PersonIn ::= [_ | PersonIn].'));
      expect(c.source, contains('Inputs ::= [Input | Inputs].'));
      expect(c.source, contains('Route ::= route(Integer, Inputs?).'));
      expect(c.source, contains('Deads ::= [] ; [Inputs? | Deads].'));
      expect(c.source,
          contains('procedure split_grants(PersonIn?, Inputs, Stream(_)).'));
      expect(
          c.source,
          contains('procedure(Q) serve(Stream(Ask(Q))?, Inputs?, '
              'Stream(Draw)?, Stream(Draw), Stream(_), Routes?, Deads?, '
              'Integer?, Stream(Spawn(Q))).'));
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
      expect(c.source, contains('procedure(Q) dispatch_1('));
      expect(c.source, contains('Draw_1 ::= draw(Integer, _, _) ; '
          'withdraw(Integer).'));
      expect(c.source, contains('Ask_1(Q) ::= ask(Constant, Q).'));
      expect(c.source, contains('procedure run_1(Integer?, _?, Stream(_)?, '
          'Done?, Stream(Draw_1)).'));
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
          'Inputs?, Stream(Draw)).'));
      expect(c.source,
          contains('procedure constructs(Stream(Spawn(Question))?).'));
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
              'run(Id?, form(card, [shown, buttons([yes, no])]), Vs?, Done?, '
              'Ds).'));
      // The view waits for the peer, so the output comes before the input
      // inside it; the question is marked with its position, the card's at
      // the root [] and the question its argument 2, [2] (C).
      expect(
          s,
          contains('present_card(P, card(H1, H2?), Gs, Gs1?, '
              '[card(H1?, input(M2?))], D2?) :- ground(P?), ground(H1?) | '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2).'));
      expect(s, contains('procedure path(Path?, Integer?, Path).'));
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
              'Input?, Inputs?, Inputs, Done).'));
      expect(s, contains('take_yesNo(formed(V), _, V?, _, Gs, Gs?, done).'));
      expect(
          s,
          contains('take_yesNo(refused, P, X?, G, Gs, [G? | Gs1?], Done?) :- '
              'answer_yesNo(P?, X, Gs?, Gs1, Done).'));
      expect(
          s,
          contains('procedure answer_yesNo(Path?, YesNo, Inputs?, Inputs, '
              'Done).'));
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
(post(T))*agent(N) :- ground(N?), ground(T?) | true.
(quit)*agent(_).
''').source;
      expect(
          s,
          contains('construct(Id, request_r(X?), Gs, Ds?) :- '
              'answer_request([], X, Gs?, _, Done), run(Id?, menu([form(post, '
              '[text]), button(quit)]), [input([])], Done?, Ds).'));
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
              'run(Id?, input_box(text), [input([])], Done?, Ds).'));
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
          contains('menu([form(order, [peer, form(lot, [number, date])]), '
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
      expect(c.widgets['Price?'], 'form(price, [number])');
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
      expect(s, contains('form(board, [thread, input_box(text)])'));
      expect(
          s,
          contains('present_board(P, board(H1, H2?), Gs, Gs1?, Vs?, D2?) :- '
              'ground(P?) | thread(H1?, V1), path(P?, 2, M2), '
              'path(P?, 2, Q2), answer_stream_string(Q2?, H2, Gs?, Gs1, D2), '
              'start_board_board_2(V1?, [input(M2?)], Vs).'));
      expect(
          s,
          contains('comb_board_board_2(_, V2, [V1 | S1], S2, '
              '[board(V1?, V2?) | Vs?]) :- ground(V1?), ground(V2?) | '
              'comb_board_board_2(V1?, V2?, S1?, S2?, Vs).'));
    });

    test('a list the program writes, with a choice of its element coming '
        'back: a picker', () {
      final s = compile('''
Peer ::= Constant.
Peers ::= [] ; [Peer | Peers].
Pick ::= pick(Peers, Peer?).
procedure (Pick)*choose(Peer).
(pick([bob, carol], P))*choose(P?).
''').source;
      expect(s, contains('run(Id?, picker, Vs?, Done?, Ds)'));
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
          contains('form(outer, [shown, form(inner, [shown, buttons([yes, '
              'no])]), text])'));
      // The nested structure's position is its argument's, [2], and its
      // question's below it [2, 2]; the outer question's is [3] (C).
      expect(
          s,
          contains('present_outer(P, outer(H1, H2, H3?), Gs, Gs2?, Vs?, '
              'Done?) :- ground(P?) | shown(H1?, V1), path(P?, 2, Q2), '
              'present_inner(Q2?, H2?, Gs?, Gs1, V2, D2), path(P?, 3, M3), '
              'path(P?, 3, Q3), answer_string(Q3?, H3, Gs1?, Gs2, D3), '
              'all_done([D2?, D3?], Done), '
              'start_outer_outer_3(V1?, V2?, [input(M3?)], Vs).'));
      expect(
          s,
          contains('present_inner(P, inner(H1, H2?), Gs, Gs1?, '
              '[inner(H1?, input(M2?))], D2?) :- ground(P?), ground(H1?) | '
              'path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2).'));
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
          contains('present_card(P, card(H1, H2?, H3?), Gs, Gs2?, '
              '[card(H1?, input(M2?), input(M3?))], Done?) :- ground(P?), '
              'ground(H1?) | path(P?, 2, M2), path(P?, 2, Q2), '
              'answer_yesNo(Q2?, H2, Gs?, Gs1, D2), path(P?, 3, M3), '
              'path(P?, 3, Q3), answer_yesNo(Q3?, H3, Gs1?, Gs2, D3), '
              'all_done([D2?, D3?], Done).'));
      expect('procedure answer_yesNo('.allMatches(s), hasLength(1));
      expect(
          s,
          contains('answer_yesNo(P, X?, [input(Id, P1, R) | Gs], Gs1?, '
              'Done?) :- P1? =?= P?, ground(R?) | form_yesNo(R?, F), '
              'take_yesNo(F?, P?, X, input(Id?, P1?, R?), Gs?, Gs1, Done).'));
    });

    test('the position is the path of argument indices from the root, a '
        'list cell\'s head its argument 1 and its tail its argument 2', () {
      final s = compile('''
YesNo ::= yes ; no.
Nil ::= [].
Tail ::= [YesNo? | Nil].
Two ::= [YesNo? | Tail].
procedure (Two)*pair(YesNo, YesNo).
([A, B])*pair(A?, B?).
''').source;
      // The first question is at [1]; the second, the head of the tail, at
      // [2, 1], present_tail called at [2].
      expect(
          s,
          contains('present_two(P, [H1? | H2], Gs, Gs2?, Vs?, Done?) :- '
              'ground(P?) | path(P?, 1, M1), path(P?, 1, Q1), '
              'answer_yesNo(Q1?, H1, Gs?, Gs1, D1), path(P?, 2, Q2), '
              'present_tail(Q2?, H2?, Gs1?, Gs2, V2, D2)'));
      expect(
          s,
          contains('present_tail(P, [H1? | H2], Gs, Gs1?, '
              '[[input(M1?) | H2?]], D1?) :- ground(P?), ground(H2?) | '
              'path(P?, 1, M1), path(P?, 1, Q1), '
              'answer_yesNo(Q1?, H1, Gs?, Gs1, D1).'));
      expect(s, contains('construct(Id, two_w(X), Gs, Ds?) :- '
          'present_two([], X?, Gs?, _, Vs, Done)'));
    });

    test('an alternative holding no question does not read its position, '
        'and one reading it once does not guard on it', () {
      final s = compile('''
Peer ::= Constant.
YesNo ::= yes ; no.
Inner ::= inner(Peer, YesNo?).
T ::= a(Integer) ; b(Inner).
procedure (T)*mixed(YesNo).
(b(inner(bob, A)))*mixed(A?).
''').source;
      expect(s,
          contains('present_t(_, a(H1), Gs, Gs?, [a(H1?)], done) :- '
              'ground(H1?) | true.'));
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

  group('what is not built', () {
    test('a position the program writes inside one the person writes', () {
      expect(() => compile('''
Reply ::= done.
Order ::= order(Integer, Reply?).
procedure (Order?)*order(Order).
(O)*order(O?).
'''), refused('a position of it is written by the program'));
    });

    test('a position the person writes of a type parameter', () {
      expect(() => compile('''
Box(X) ::= box(X).
procedure(X) (Box(X)?)*take(X).
(box(V))*take(V?).
'''), refused('of a type parameter'));
    });


    test('a stream the program writes whose elements hold questions', () {
      expect(() => compile('''
YesNo ::= yes ; no.
Card ::= card(Integer, YesNo?).
procedure (Stream(Card))*cards.
(Cs?)*cards :- deal(Cs).
procedure deal(Stream(Card)).
deal([]).
'''), refused('whose elements hold questions'));
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
      expect(c.widgets['Pair'], 'form(pair, [toggle, toggle])');
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
}
