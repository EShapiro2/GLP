// glp_runtime/test/vglp/program_compilation_test.dart
//
// The canonical compilation end to end: a vGLP module to one self-contained
// GLP module.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/vglp/mediator.dart';
import 'package:glp_runtime/vglp/program_compilation.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

const _programs = '../programs';

/// The deployed `.vglp` sources, by which both groups below go: the paths of
/// `find programs -name "*.vglp"`, less the one-clause fixture of
/// programs/tests/vglp, which load_test covers.  One list, so that a source
/// added to the tree is added once.
const _deployedVglp = [
  'social/graph/core/agent.vglp',
  'social/graph/core/home.vglp',
  'grassapp/grassapp_agent.vglp',
  'cssn/childsafe/agent.vglp',
  'cssn/childsafe/child_agent.vglp',
  'currencies/coins/currency/coins_agent.vglp',
  'currencies/bonds/bonds_agent.vglp',
  'currencies/sovereign/denominated/sovereign_agent.vglp',
];

void main() {
  final rootSelfGlp = File('$_programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    setRootScopeEnvironmentSource(rootSelfGlp.readAsStringSync());
  }

  final mediator = MediatorSource.fromDirectory('$_programs/vglp');

  CompiledProgram compile(String source) => compileProgram(
      Parser(Lexer(source).tokenize(), vglp: true).parseModule(), mediator);

  group('one self-contained module', () {
    const src = '''
IntroChannel   ::= Channel(Stream(Constant), Stream(Constant)).
Response       ::= accept(IntroChannel) ; no.
Decision       ::= yes ; no.
ColdCallAnswer ::= response(Response?).
UserContent    ::= response(Response).
UserInMsg      ::= msg(Constant, Constant, UserContent)
                 ; decision(Decision, Constant, ColdCallAnswer).
UserInStream   ::= [] ; [UserInMsg | UserInStream].
ColdCallOffer  ::= offer(Constant).

procedure respond(ColdCallOffer?, Response, UserInStream) *(Answer).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true
*(no) true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('it carries the source types, the added types and the mediator\'s',
        () {
      final out = compile(src).source;
      expect(out, contains('UserInStream ::='));       // the source's own
      expect(out, contains('Xs_respond_1 ::= xs_respond_1(Decision).'));  // added
      expect(out, contains('Answer ::= Xs_respond_1 ; Xs_respond_2.'));
      expect(out, contains('Reply_respond_1 ::= then(Xs_respond_1) ; else.'));
      expect(out, contains('Reply_respond_2 ::= then(Xs_respond_2).'));
      expect(out, contains('Escrow ::= esc_respond_1(Reply_respond_1?) ; '
          'esc_respond_2(Reply_respond_2?).'));
      expect(out, contains(                                             // instantiated
          'UserAnswer ::= answer(ReqId, Answer) ; decline(ReqId).'));
    });

    test('no type parameter survives the emission', () {
      final out = compile(src).source;
      expect(out, isNot(contains('(A)')));
      expect(out, isNot(contains('(A, E, X)')));
      expect(out, isNot(contains('(E, X)')));
      expect(out, isNot(contains('PendingList(E)')));
    });

    test('every compiled procedure is declared immediately above its clauses',
        () {
      final out = compile(src).source;
      final lines = out.split('\n');
      final declLine =
          lines.indexWhere((l) => l.startsWith('procedure respond('));
      expect(declLine, greaterThan(-1));
      expect(lines[declLine + 1], startsWith('respond('));
    });

    test('the mediator comes with it, clauses and declarations', () {
      final out = compile(src).source;
      for (final p in ['med', 'answer', 'close', 'drop', 'med_split']) {
        expect(out, contains('procedure $p('), reason: '$p is missing');
      }
      // abort/3 stays generic in the slot's reply type.
      expect(out, contains('procedure(R) abort('));
      // The timer and the deadline went on 2026-09-15 (Udi), so no compiled
      // program carries them.
      expect(out, isNot(contains('procedure timer(')));
      expect(out, isNot(contains('procedure deadline(')));
    });

    test('the pending table\'s program clauses come ahead of the search clauses',
        () {
      final out = compile(src).source;
      final own = out.indexOf('answer(ReqId, xs_respond_1(X1), ');
      final search = out.indexOf('otherwise | answer(');
      expect(own, greaterThan(-1));
      expect(search, greaterThan(own));
      expect(out, contains('answer(ReqId, xs_respond_2(X1), '));
      // close only for the clause with an else-branch
      expect(out, contains('close(ReqId, [pending(Id, esc_respond_1(R?)) | Ps], Ps?)'));
      expect(out, isNot(contains('esc_respond_2(R?)) | Ps], Ps?) :- (ReqId? =?= Id?) | R = else')));
    });

    test('the slots are typed by their clauses\' reply types', () {
      final out = compile(src).source;
      expect(out, contains('Slot(Reply_respond_1)?, Slot(Reply_respond_2)?)'));
      expect(out, contains('Slot(R) ::= none ; ask(R, ReqId).'));
    });

    test('the emitted module parses back as GLP', () {
      final out = compile(src).source;
      final back = Parser(Lexer(out).tokenize()).parseModule();
      expect(back.procedures, isNotEmpty);
      // Nothing volition-guarded survives: GLP is vGLP without volition guards.
      for (final p in back.procedures) {
        for (final c in p.clauses) {
          expect(c.isVolitionGuarded, isFalse);
        }
      }
    });

    test('display declarations are carried through unchanged', () {
      final out = compile('''
$src
display respond *(Answer=yes, From?) : panel(inbox), label("Accept"), transient.
''');
      expect(out.source, contains('display respond'));
      expect(out.source, contains('label("Accept")'));
      // An atom is printed bare and a string literal keeps its quotes, as the
      // source had them: a declaration printed panel("inbox") would no longer
      // match its clause's guard.
      expect(out.source, contains('panel(inbox)'));
      expect(out.source, contains('*(Answer=yes, From?)'));
      expect(out.source, isNot(contains('"inbox"')));
      expect(out.source, isNot(contains('"yes"')));
    });
  });

  // A display declaration is "for a volition-guarded clause of predicate p with
  // volition guard *(...)" and "names its clause's volition guard, so an
  // else-branch has none of its own" (vGLP, Definition "Display Declaration,
  // Default Display").  One naming a guard no clause of the program has names
  // no clause, and the compilation rejects the source instead of carrying the
  // declaration through: until 2026-09-20 both `:emit` and the load carried
  // coins' `display respond_swap *(no, From?, Want?, Offered?)` verbatim after
  // the clause of that guard was gone, with no diagnostic (reported by vGLP,
  // 2026-09-18).
  group('a display declaration names a clause of the program', () {
    const src = '''
Offer    ::= offer(Constant).
Decision ::= yes ; no.
OutMsg   ::= decided(Decision).
Out      ::= [] ; [OutMsg | Out].

procedure respond(Offer?, Out) *(Answer).
*(Answer=yes, From?)
respond(offer(From), [decided(Answer?)]) :- ground(From?) | true
*(no) true.

procedure note(Offer?, Out).
note(offer(From), [decided(no)]) :- ground(From?) | true.
''';

    String rejection(String decl) {
      try {
        compile('$src$decl\n');
      } on CompileError catch (e) {
        return e.toString();
      }
      fail('"$decl" was compiled, and it names no clause of the program.');
    }

    test('one that names its clause\'s guard compiles', () {
      final out = compile('$src'
          'display respond *(Answer=yes, From?) : label("Accept"), transient.\n');
      expect(out.source, contains('display respond *(Answer=yes, From?) : '
          'label("Accept"), transient.'));
    });

    test('one whose guard no clause carries is rejected, and the message names '
        'the declaration and what the program has instead', () {
      final message =
          rejection('display respond *(Answer=no, From?) : label("Decline").');
      expect(message, contains('display respond *(Answer=no, From?)'));
      expect(message, contains('names no clause'));
      expect(message,
          contains('the volition-guarded clauses of respond carry '
              '*(Answer=yes, From?)'));
    });

    test('an else-branch is no clause of its own, so its answer is not a guard '
        'a declaration may name', () {
      // The clause above has the else-branch *(no): the reported case, where
      // `display respond_swap *(no, ...)` outlived its clause.
      final message = rejection('display respond *(no) : label("Decline").');
      expect(message, contains('display respond *(no)'));
      expect(message, contains('names no clause'));
    });

    // "display p *(...) n : ..." names "the n-th of several clauses of p with
    // that volition guard" (vGLP, Definition "Display Declaration, Default
    // Display"), so the match is by predicate, guard and index.
    const twoOfOneGuard = '''
Offer    ::= offer(Constant).
Decision ::= yes ; no.
OutMsg   ::= decided(Decision).
Out      ::= [] ; [OutMsg | Out].

procedure respond(Offer?, Out) *(Answer).
*(Answer=yes, From?)
respond(offer(From), [decided(Answer?)]) :- ground(From?) | true.
*(Answer=yes, From?)
respond(offer(From), [decided(Answer?)]) :- constant(From?) | true.
''';

    test('an index of 1 names the sole clause of that guard, and the compiled '
        'declaration carries it', () {
      final out = compile('$src'
          'display respond *(Answer=yes, From?) 1 : label("Accept").\n');
      expect(out.source,
          contains('display respond *(Answer=yes, From?) 1 : '
              'label("Accept").'));
    });

    test('an index naming the second of two clauses of one guard compiles', () {
      final out = compile('$twoOfOneGuard'
          'display respond *(Answer=yes, From?) 2 : label("Accept").\n');
      expect(out.source,
          contains('display respond *(Answer=yes, From?) 2 : '
              'label("Accept").'));
    });

    test('an index beyond the clauses of that guard is rejected, and the '
        'message counts what the program has', () {
      final message =
          rejection('display respond *(Answer=yes, From?) 2 : label("Accept").');
      expect(message, contains('display respond *(Answer=yes, From?) 2'));
      expect(message, contains('names no clause'));
      expect(message,
          contains('respond has 1 clause with that volition guard'));
    });

    test('one for a procedure with no volition-guarded clause is rejected', () {
      final message =
          rejection('display note *(Answer=yes, From?) : label("Note").');
      expect(message, contains('no clause of note carries a volition guard'));
    });

    test('one for a procedure the program does not define is rejected', () {
      final message = rejection('display greet *(Name) : label("Greet").');
      expect(message, contains('the program has no procedure greet'));
    });

    // The sources on disc: this is the check that would have caught the coins
    // declaration, and it is run over each source whether or not that source
    // compiles yet.
    for (final path in _deployedVglp) {
      test('$path declares the displays of its own clauses', () {
        final file = File('$_programs/$path');
        expect(file.existsSync(), isTrue, reason: '$path is not on disc');
        checkDisplayDecls(
            Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
                .parseModule());
      });
    }

    test('the message form names a pattern, not a clause, and is untouched',
        () {
      final out = compile('$src'
          'display msg(agent, person, hello) : panel(inbox), view(list).\n');
      expect(out.source,
          contains('display msg(agent, person, hello) : panel(inbox), '
              'view(list).'));
    });
  });

  group('the question parameters of a procedure declaration', () {
    // vGLP, sections/vglp.tex, Section "Volition-Guarded GLP": the question
    // parameters are "every writer that a volition guard of the procedure
    // names, each of them once; a volition guard's writers are matched to them
    // by name".  EVERY writer, so an undeclared guard writer is refused; every
    // writer A GUARD NAMES, so a declared parameter no guard names is refused.
    const types = '''
Offer    ::= offer(Constant).
Decision ::= yes ; no.
OutMsg   ::= decided(Decision).
Out      ::= [] ; [OutMsg | Out].
''';

    String rejection(String source) {
      try {
        compile(source);
      } on CompileError catch (e) {
        return e.toString();
      }
      fail('the source compiled, and its question parameters do not match its '
          'volition guards.');
    }

    test('a guard writer the declaration does not name is rejected, by name',
        () {
      final message = rejection('${types}'
          'procedure respond(Offer?, Out).\n'
          '*(Answer=yes, From?)\n'
          'respond(offer(From), [decided(Answer?)]) :- ground(From?) | true.\n');
      expect(message, contains('names the writer "Answer"'));
      expect(message, contains('the declaration of respond/2 does not'));
      expect(message, contains('*(Answer)'));
    });

    test('a declared parameter no guard names is rejected, by name', () {
      final message = rejection('${types}'
          'procedure respond(Offer?, Out) *(Answer, Amount).\n'
          '*(Answer=yes, From?)\n'
          'respond(offer(From), [decided(Answer?)]) :- ground(From?) | true.\n');
      expect(message, contains('names the question parameter "Amount"'));
      expect(message, contains('no volition guard of the procedure names'));
    });

    test('the compiled declaration carries the parameters through', () {
      final out = compile('${types}'
          'procedure respond(Offer?, Out) *(Answer, Other).\n'
          '*(Answer=yes, From?)\n'
          'respond(offer(From), [decided(Answer?)]) :- ground(From?) | true.\n'
          '*(Other=no, From?)\n'
          'respond(offer(From), [decided(Other?)]) :- constant(From?) | true.\n');
      expect(out.types.procDecls.firstWhere((d) => d.name == 'respond')
          .questionParams, ['Answer', 'Other']);
      expect(out.source, contains('*(Answer, Other).'));
      // And the emitted text parses back as GLP, list and all.
      expect(() => Parser(Lexer(out.source).tokenize()).parseModule(),
          returnsNormally);
    });

    test('a procedure whose guards name no writer carries no list', () {
      final out = compile('${types}'
          'procedure respond(Offer?, Out).\n'
          '*(yes, From?)\n'
          'respond(offer(From), [decided(no)]) :- ground(From?) | true.\n');
      expect(out.types.procDecls.firstWhere((d) => d.name == 'respond')
          .questionParams, isEmpty);
      expect(out.source, isNot(contains(') *(')));
    });

    test('two positions of one type in one clause are two parameters', () {
      // "a swap's give amount and its want amount" (same sentence).  The
      // runnable fixture is programs/tests/vglp/question_parameters.
      final out = compile('''
Lot   ::= lot(Constant, Integer).
Trade ::= trade(Constant, Constant, Lot, Lot).

procedure swap(Constant?, Stream(Trade)) *(Friend, GiveCoin, GiveAmount, WantCoin, WantAmount).
*(Friend, GiveCoin, GiveAmount, WantCoin, WantAmount)
swap(Id, [trade(Id?, Friend?, lot(GiveCoin?, GiveAmount?),
                lot(WantCoin?, WantAmount?))]) :-
    ground(Id?) | true.
''');
      expect(out.source, contains('Xs_swap_1 ::= '
          'xs_swap_1(Constant, Constant, Integer, Constant, Integer).'));
    });

    test('a name two clauses type differently is rejected', () {
      expect(
          () => compile('''
Lot   ::= lot(Constant, Integer).
Trade ::= trade(Constant, Lot).
Note  ::= note(Constant, Constant).
Out   ::= [] ; [Note | Out].

procedure post(Constant?, Stream(Trade)) *(Amount).
*(Amount)
post(Id, [trade(Id?, lot(gb, Amount?))]) :- ground(Id?) | true.

procedure mention(Constant?, Out) *(Amount).
*(Amount)
mention(Id, [note(Id?, Amount?)]) :- ground(Id?) | true.
'''),
          returnsNormally,
          reason: 'two procedures, two names: one type throughout THE '
              'PROCEDURE, not throughout the program');
    });
  });

  group('the deployed sources', () {
    // EIGHT, and a list written from memory misses one: cssn/child_agent.vglp
    // arrived on 2026-08-16, coins_agent.vglp and bonds_agent.vglp on
    // 2026-09-03, social/graph/core/home.vglp on 2026-09-09 and
    // sovereign/denominated/sovereign_agent.vglp on 2026-09-15.  Count them on
    // disc --- `find programs -name "*.vglp"`, less the one-clause fixture of
    // programs/tests/vglp, which load_test covers.  Four of the paths moved on
    // 2026-09-08, each source going with its .glp into the certified program of
    // a mini-app --- social/graph/core, coins/currency and cssn/childsafe ---
    // since a lone .vglp is compiled by the loader and must stand beside its
    // .glp.  They all PARSE as vGLP.  coins/currency/coins_agent.vglp,
    // bonds/bonds_agent.vglp and sovereign/denominated/sovereign_agent.vglp
    // also COMPILE and RUN --- Sections N2, N3 and N4 of test/run_all_tests.sh
    // load their emitted .glp and run the village market; social/graph/core/
    // home.vglp compiles, its home.glp being the compiler's output, and no
    // section of the suite runs it.  The rest do not yet compile, the edits
    // being with their owners — social/graph and grassapp SGSG's, the two cssn
    // sources CSSN's.  Each parse test becomes the compilation test as its
    // source is repaired.
    for (final path in _deployedVglp) {
      test('$path parses as vGLP', () {
        final file = File('$_programs/$path');
        expect(file.existsSync(), isTrue, reason: '$path is not on disc');
        final m = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
            .parseModule();
        expect(m.procedures, isNotEmpty);
        expect(
            m.procedures
                .expand((p) => p.clauses)
                .any((c) => c.isVolitionGuarded),
            isTrue);
      });
    }
  });
}
