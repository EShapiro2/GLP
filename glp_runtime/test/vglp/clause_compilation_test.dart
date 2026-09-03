// glp_runtime/test/vglp/clause_compilation_test.dart
//
// The clause transformation of the canonical compilation, checked as emitted
// GLP text.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/vglp/clause_compilation.dart';

const _preamble = '''
Stream(X) ::= [] ; [X | Stream(X)].
Response ::= response(Constant).
Decision ::= decision(Constant, Constant, Response).
Offer ::= offer(Constant).
''';

/// Compile one procedure of a vGLP source and return its clauses as GLP text,
/// one clause per line.
List<String> emit(String source, String signature) {
  final m = Parser(Lexer(_preamble + source).tokenize(), vglp: true)
      .parseModule();
  final decls = {for (final d in m.procDeclarations) d.key: d};
  final defined = {for (final p in m.procedures) '${p.name}/${p.arity}'};
  final slots = {
    for (final p in m.procedures)
      '${p.name}/${p.arity}':
          p.clauses.where((c) => c.isVolitionGuarded).length
  };
  final proc =
      m.procedures.firstWhere((p) => '${p.name}/${p.arity}' == signature);
  final cp = compileProcedure(proc,
      decl: decls[signature]!,
      isProcedureOfM: (n, a) => defined.contains('$n/$a'),
      clauseName: (p, j) => '${p.name}_$j',
      slotCountOf: (n, a) => slots['$n/$a'] ?? 0);
  final printer = GlpPrinter();
  return cp.clauses.map((c) => printer.printClause(c).trim()).toList();
}

void main() {
  group('a tail-recursive request clause — the persistent form', () {
    const src = '''
procedure agent(Constant?, Stream(Constant)?, Stream(Constant)).
*(Target)
agent(Id, UserIn, Outs) :-
    ground(Id?), ground(Target?) |
    connect(Target?, Outs?, Outs1),
    agent(Id?, UserIn?, Outs1?).
''';

    test('the head gains the mediator channel and one slot', () {
      final cs = emit(src, 'agent/3');
      // answer clause and ask clause, no else-branch; and the otherwise clause
      expect(cs.length, 3);
      expect(cs[0], startsWith('agent(Med, Id, UserIn, Outs, '));
    });

    test('the otherwise clause comes last, and no safe run reaches it', () {
      // Input coverage requires it: the slot type, one for every slot of the
      // program, admits every clause's answer, and a goal's slots hold only
      // asks of its own clauses.
      expect(emit(src, 'agent/3').last,
          'agent(_, _, _, A3?, _) :- otherwise | true.');
    });

    test('the answer clause takes the then-branch and the answer binds Target',
        () {
      expect(emit(src, 'agent/3')[0],
          'agent(Med, Id, UserIn, Outs, ask(then(xs_agent_1(Target)), _)) :- '
          'ground(Id?), ground(Target?) | '
          'connect(Target?, Outs?, Outs1), '
          'agent(Med?, Id?, UserIn?, Outs1?, none).');
    });

    test('the ask clause poses the question once and re-poses the goal', () {
      expect(emit(src, 'agent/3')[1],
          'agent(Med, Id, UserIn, A3?, none) :- ground(Id?) | '
          'send(ask(agent_1, ctx_agent_1, R, Id1, no_deadline), Med?, Med1), '
          'agent(Med1?, Id?, UserIn?, A3, ask(R?, Id1?)).');
    });

    test('the ask clause keeps only the guards that read no answer position',
        () {
      // ground(Target?) reads the answer, so it is not in G_c; ground(Id?) is.
      final ask = emit(src, 'agent/3')[1];
      expect(ask, contains('ground(Id?)'));
      expect(ask, isNot(contains('ground(Target?)')));
    });

    test('the output argument is delegated, not rebuilt', () {
      // Outs is an output of the declaration: the ask clause takes its reader
      // in the head and hands the writer to the re-posed goal, which is how a
      // GLP clause delegates an output it does not itself produce.
      final ask = emit(src, 'agent/3')[1];
      expect(ask, contains('agent(Med, Id, UserIn, A3?, none)'));
      expect(ask, contains('agent(Med1?, Id?, UserIn?, A3, '));
    });
  });

  group('a responder with an else-branch', () {
    const src = '''
procedure respond(Offer?, Constant, Stream(Decision)).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true
*(no) true.
''';

    test('four clauses: answer, else, ask, otherwise', () {
      final cs = emit(src, 'respond/3');
      expect(cs.length, 4);
      expect(cs.last, 'respond(_, _, A2?, A3?, _) :- otherwise | true.');
    });

    test('the else clause matches the else reply and carries no answer', () {
      final cs = emit(src, 'respond/3');
      expect(cs[1], startsWith('respond(Med, offer(From), Resp?, '));
      expect(cs[1], contains('ask(else, _)'));
      expect(cs[1], isNot(contains('then(')));
    });

    test('the context reaches the mediator with the ask', () {
      expect(emit(src, 'respond/3')[2], contains('ctx_respond_1(From?)'));
    });

    test('the ask carries a deadline, because the clause has an else-branch',
        () {
      // D_k is deadline iff C_k has an else-branch: the machine answers on the
      // deadline only where the program says how.
      expect(emit(src, 'respond/3')[2], contains(', deadline), Med?, Med1)'));
    });
  });

  group('sibling clauses on one goal', () {
    const src = '''
procedure respond(Offer?, Constant, Stream(Decision)).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('two slots, and each clause exposes its own', () {
      final cs = emit(src, 'respond/3');
      expect(cs.length, 5);  // answer + ask, twice, and the otherwise clause
      expect(cs[0], contains('ask(then(xs_respond_1(Answer)), _), S2)'));
      expect(cs[2], contains('S1, ask(then(xs_respond_2(Answer)), _))'));
    });

    test('a clause aborts the other slots, not its own', () {
      final cs = emit(src, 'respond/3');
      expect(cs[0], contains('aborts([S2?], Med?, Med1)'));
      expect(cs[2], contains('aborts([S1?], Med?, Med1)'));
    });

    test('the body true of a guarded unit clause is not copied after aborts',
        () {
      // `| true` is the idiom of an empty body; copied it would be a call of
      // true/0, which no procedure defines.
      final cs = emit(src, 'respond/3');
      expect(cs[0], endsWith('aborts([S2?], Med?, Med1).'));
      expect(cs[2], endsWith('aborts([S1?], Med?, Med1).'));
    });

    test('each ask clause carries its own clause name', () {
      final cs = emit(src, 'respond/3');
      expect(cs[1], contains('ask(respond_1, '));
      expect(cs[3], contains('ask(respond_2, '));
    });

    test('an ask clause leaves the sibling slot untouched', () {
      expect(emit(src, 'respond/3')[1],
          contains('respond(Med1?, offer(From?), A2, A3, ask(R?, Id?), S2?)'));
    });
  });

  group('an ordinary clause of a procedure that has volition-guarded ones', () {
    const src = '''
procedure agent(Constant?, Stream(Constant)?, Stream(Constant)).
agent(Id, UserIn, Outs?) :- ground(Id?) | true.
*(Target)
agent(Id, UserIn, Outs) :-
    ground(Id?), ground(Target?) | connect(Target?, Outs?, Outs1),
    agent(Id?, UserIn?, Outs1?).
''';

    test('it aborts every slot of the goal', () {
      expect(emit(src, 'agent/3')[0], contains('aborts([S1?], Med?, Med1)'));
    });
  });

  group('a procedure with no volition-guarded clause', () {
    const src = '''
procedure relay(Constant?, Stream(Constant)?, Stream(Constant)).
relay(Id, In, Out?) :- ground(Id?) | true.
''';

    test('it gains the channel and no slot, and calls no aborts', () {
      final cs = emit(src, 'relay/3');
      expect(cs.single, startsWith('relay(Med, Id, In, Out?)'));
      expect(cs.single, isNot(contains('aborts')));
    });

    test('and no otherwise clause: with m = 0 there is no slot to cover', () {
      expect(emit(src, 'relay/3').any((c) => c.contains('otherwise')), isFalse);
    });
  });
}
