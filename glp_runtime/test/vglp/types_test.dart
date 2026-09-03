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
      expect(c.answer.toString(), startsWith('Xs_respond_1 ::= xs_respond_1('));
    });

    test('the answer writer is typed by where it occurs, not by its own name',
        () {
      // Answer occurs only inside the head's third argument, at the first
      // position of decision/3, which is Decision.  Nothing declares Answer:
      // its type is the type of the position it sits at.
      expect(compile(src).byClause['respond_1']!.answer.toString(),
          'Xs_respond_1 ::= xs_respond_1(Decision).');
    });

    test('the context reader is typed the same way', () {
      expect(compile(src).byClause['respond_1']!.context.toString(),
          'Ctx_respond_1 ::= ctx_respond_1(Constant).');
    });
  });

  group('an anonymous question position', () {
    // The volition guard writes a ground term there and no writer: its type is
    // the type of the term, Constant for an atom and Integer for an integer.
    test('is typed by its ground term', () {
      expect(compile('''
procedure respond(ColdCallOffer?, Response, UserInStream).
*(yes, From?)
respond(offer(From), Resp?, [decision(yes, From?, response(Resp))]) :-
    ground(From?) | true.
''').byClause['respond_1']!.answer.toString(),
          'Xs_respond_1 ::= xs_respond_1(Constant).');
      expect(compile('''
procedure respond(ColdCallOffer?, Response, UserInStream).
*(3, From?)
respond(offer(From), Resp?, [decision(yes, From?, response(Resp))]) :-
    ground(From?) | true.
''').byClause['respond_1']!.answer.toString(),
          'Xs_respond_1 ::= xs_respond_1(Integer).');
    });
  });

  group('an answer writer at a type-parameter position', () {
    // send_friend is generic in the message M it carries; Text occurs nowhere
    // but inside msg(Id?, Target?, text(Text?)) at that position.  The call
    // instantiates M from the term's shape: FriendMsg and NetInMsg both accept
    // the term, and both type the position Constant.
    const decls = '''
FriendContent ::= text(Constant) ; hello.
FriendMsg     ::= msg(Constant, Constant, FriendContent).
NetInMsg      ::= msg(Constant, Constant, FriendContent)
                ; msg(Constant, ColdCallOffer).
OutputEntry   ::= friend_output(Constant, Stream(FriendMsg))
                ; user_output(UserInStream).
procedure(M, Ent) send_friend(Constant?, M?, Stream(Ent)?, Stream(Ent)).
send_friend(_, _, Outs, Outs?).
''';

    test('is typed by the shape of the term it occurs in', () {
      expect(compile('''
$decls
procedure agent(Constant?, UserInStream?, Stream(OutputEntry)?, Stream(OutputEntry)).
*(Target, Text)
agent(Id, UserIn, Outs, Outs1?) :-
    ground(Id?), ground(Target?) |
    send_friend(Target?, msg(Id?, Target?, text(Text?)), Outs?, Outs1).
''').byClause['agent_1']!.answer.toString(),
          'Xs_agent_1 ::= xs_agent_1(Constant, Constant).');
    });

    test('a declared occurrence wins over a shape', () {
      // Text also occurs in the head, at a String position: that is its type,
      // whatever the shape would say.
      expect(compile('''
$decls
Note ::= note(String).
procedure agent(Constant?, Note?, Stream(OutputEntry)?, Stream(OutputEntry)).
*(Target, Text)
agent(Id, note(Text), Outs, Outs1?) :-
    ground(Id?), ground(Target?) |
    send_friend(Target?, msg(Id?, Target?, text(Text?)), Outs?, Outs1).
''').byClause['agent_1']!.answer.toString(),
          'Xs_agent_1 ::= xs_agent_1(Constant, String).');
    });

    test('types that disagree on the position stop the compilation', () {
      expect(
          () => compile('''
Alpha ::= f(Constant).
Beta  ::= f(Integer).
procedure(M) q(M?).
q(_).
procedure p(Constant?).
*(X)
p(Id) :- ground(Id?) | q(f(X?)).
'''),
          throwsA(isA<StateError>().having(
              (e) => e.message, 'message', contains('disagree'))));
    });

    test('a term of no type stops the compilation', () {
      expect(
          () => compile('''
procedure(M) q(M?).
q(_).
procedure p(Constant?).
*(X)
p(Id) :- ground(Id?) | q(nothing_has_this(X?)).
'''),
          throwsA(isA<StateError>().having(
              (e) => e.message, 'message', contains('no type'))));
    });

    test('a writer whose only occurrence is a guard =?= takes the other side',
        () {
      // Friend occurs in no head or body position: the guard Friend? =?= Id?
      // compares it with Id, whose type the head declares.
      expect(compile('''
$decls
procedure agent(Constant?, Stream(OutputEntry)?, Stream(OutputEntry)).
*(Child, Friend)
agent(Id, Outs, Outs1?) :-
    ground(Id?), Friend? =?= Id?, ground(Child?) |
    send_friend(Child?, msg(Id?, Child?, hello), Outs?, Outs1).
''').byClause['agent_1']!.answer.toString(),
          'Xs_agent_1 ::= xs_agent_1(Constant, Constant).');
    });

    test('a writer inside a list at a Stream position is typed by the element',
        () {
      expect(compile('''
$decls
procedure agent(Constant?, Stream(FriendMsg)).
*(Target)
agent(Id, [msg(Id?, Target?, hello)]) :- ground(Id?) | true.
''').byClause['agent_1']!.answer.toString(),
          'Xs_agent_1 ::= xs_agent_1(Constant).');
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

    test('two clauses with questions of one length keep distinct functors', () {
      // A union's top-level functors must be distinct, so the functor names
      // the clause: xs_respond_1 and xs_respond_2, never xs/1 twice.
      final t = compile(src);
      expect(t.byClause['respond_1']!.answer.toString(),
          'Xs_respond_1 ::= xs_respond_1(Decision).');
      expect(t.byClause['respond_2']!.answer.toString(),
          'Xs_respond_2 ::= xs_respond_2(Decision).');
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
          'Answer ::= xs_agent_1.');
      expect(t.typeDefs.firstWhere((d) => d.name == contextTypeName).toString(),
          'Context ::= ctx_agent_1.');
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
      // AgentMsg and Slot are the vocabulary's, instantiated at the program's
      // answer and context types: monomorphic, so referenced bare.
      expect(d.argTypes.first.toString(),
          'Channel(Closed, Stream(AgentMsg))?');
      expect(d.argTypes.last.toString(), 'Slot?');
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
          'Channel(Closed, Stream(AgentMsg))?');
    });
  });
}
