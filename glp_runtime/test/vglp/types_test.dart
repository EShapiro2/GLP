// glp_runtime/test/vglp/types_test.dart
//
// The types the canonical compilation adds.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation",
// the "types" item.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/vglp/types.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

// The vocabulary of programs/social/graph, cut to what these clauses need, so
// that the clauses below are the deployed ones rather than invented shapes.
// Stream(X), Channel(In, Out) and Constant come from the root self.glp.
const _preamble = '''
IntroChannel   ::= Channel(Stream(Constant), Stream(Constant)).
Response       ::= accept(IntroChannel) ; no.
Decision       ::= yes ; no.
ColdCallAnswer ::= response(Response?).
UserContent    ::= response(Response).
UserInMsg      ::= msg(Constant, Constant, UserContent)
                 ; decision(Decision, Constant, ColdCallAnswer).
UserInStream   ::= [] ; [UserInMsg | UserInStream].
ColdCallOffer  ::= offer(Constant).
''';

CompiledTypes compile(String source) => compileTypes(
    Parser(Lexer(_preamble + source).tokenize(), vglp: true).parseModule());

void main() {
  // Root scope from programs/self.glp, as the engine sets it: without it the
  // environment has no Constant and no Stream, and the answer writers' types
  // cannot be read off the checker.
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    setRootScopeEnvironmentSource(rootSelfGlp.readAsStringSync());
  }

  group('the answer and context types of a clause', () {
    const src = '''
procedure respond(ColdCallOffer?, Response, UserInStream).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('each volition-guarded clause gets an answer type', () {
      final t = compile(src);
      final c = t.byClause['respond_1']!;
      expect(c.answer!.name, 'Xs_respond_1');
      expect(c.answer.toString(), startsWith('Xs_respond_1 ::= xs('));
    });

    test('the answer writer is typed by where it occurs, not by its own name',
        () {
      // Answer occurs only inside the head's third argument, at the first
      // position of decision/3, which is Decision.  Nothing declares Answer:
      // its type is the type of the position it sits at.
      expect(compile(src).byClause['respond_1']!.answer.toString(),
          'Xs_respond_1 ::= xs(Decision).');
    });

    test('the context reader is typed the same way', () {
      expect(compile(src).byClause['respond_1']!.context.toString(),
          'Ctx_respond_1 ::= ctx(Constant).');
    });
  });

  group("the program's answer and context types", () {
    const src = '''
procedure respond(ColdCallOffer?, Response, UserInStream).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('A unions the clauses\' answer types', () {
      final a = compile(src)
          .typeDefs
          .firstWhere((d) => d.name == answerTypeName);
      expect(a.toString(), 'Answer ::= Xs_respond_1 ; Xs_respond_2.');
    });

    test('the context type unions theirs', () {
      final x = compile(src)
          .typeDefs
          .firstWhere((d) => d.name == contextTypeName);
      expect(x.toString(), 'Context ::= Ctx_respond_1 ; Ctx_respond_2.');
    });
  });

  group('a clause with no question and no context', () {
    const src = '''
procedure agent(Constant?, UserInStream?).
*
agent(Id, UserIn) :- ground(Id?) | agent(Id?, UserIn?).
''';

    test('it contributes the bare constants', () {
      final t = compile(src);
      expect(t.byClause['agent_1']!.answer, isNull);
      expect(t.byClause['agent_1']!.context, isNull);
      expect(t.typeDefs.firstWhere((d) => d.name == answerTypeName).toString(),
          'Answer ::= xs.');
      expect(t.typeDefs.firstWhere((d) => d.name == contextTypeName).toString(),
          'Context ::= ctx.');
    });
  });

  group('the rewritten procedure declarations', () {
    const src = '''
procedure agent(Constant?, UserInStream?, UserInStream?).
*(Target)
agent(Id, UserIn, Outs) :-
    ground(Id?), ground(Target?) |
    agent(Target?, UserIn?, Outs?).
''';

    test('the channel comes first and the slots last', () {
      final d = compile(src).procDecls.firstWhere((d) => d.name == 'agent');
      expect(d.arity, 5);
      expect(d.argTypes.first.toString(),
          'Channel(Closed, Stream(AgentMsg(Answer, Context)))?');
      expect(d.argTypes.last.toString(), 'Slot(Answer)?');
    });

    test("the source's own argument types are unchanged between them", () {
      final d = compile(src).procDecls.firstWhere((d) => d.name == 'agent');
      expect(d.argTypes.sublist(1, 4).map((t) => t.toString()),
          ['Constant?', 'UserInStream?', 'UserInStream?']);
    });

    test('one slot per volition-guarded clause of the procedure', () {
      final d = compile('''
procedure respond(ColdCallOffer?, Response, UserInStream).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''').procDecls.single;
      expect(d.arity, 6);  // 3 + channel + 2 slots
    });

    test('a procedure with no volition-guarded clause gains the channel alone',
        () {
      final d = compile('''
procedure relay(Constant?, UserInStream?).
relay(Id, In) :- ground(Id?) | relay(Id?, In?).
''').procDecls.single;
      expect(d.arity, 3);
      expect(d.argTypes.first.toString(),
          'Channel(Closed, Stream(AgentMsg(Answer, Context)))?');
    });
  });
}
