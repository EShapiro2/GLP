// glp_runtime/test/vglp/parser_test.dart
//
// The vGLP surface syntax.
//
// A volitional procedure's declaration and its clauses.
// Spec: vGLP, sections/vglp.tex, Definition "Guarded Clause, Volitional
// Procedure, Interactive Type, Interactive Term, Ordinary Clause, Procedure,
// vGLP Program", and the fragments of the currency agent after it.
//
// The volition guard before a clause and the else-branch after its body, in
// which the sources not yet in that syntax are written.
// Spec: the Definition it replaced, "Guarded Clause, Volition-Guarded Clause,
// Volition Guard, Question, Answer, Context, Else-Branch, Ordinary Clause,
// Procedure, vGLP Program", and the responder exhibit after it.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart' show TypeRef;

void main() {
  Module parse(String source, {bool vglp = true}) =>
      Parser(Lexer(source).tokenize(), vglp: vglp).parseModule();

  Clause only(String source) => parse(source).procedures.single.clauses.single;

  Matcher refusedWith(String text) => throwsA(isA<CompileError>()
      .having((e) => e.message, 'message', contains(text)));

  group('volitional procedure', () {
    // The responder of the paper's fragments: its interactive type in writer
    // mode, its interactive term a compound term.
    const responder = '''
Peer ::= Constant.
Offer ::= offer(Peer).
Response ::= accept(Peer) ; refuse(Peer).
YesNo ::= yes ; no.
Card ::= card(Peer, YesNo?).
procedure (Card)*respond_coldcall(Offer?, Response).
(card(From?, Answer))*respond_coldcall(offer(From), Resp?) :-
    ground(From?) | decide(Answer?, From?, Resp).
''';

    // The agent of the paper's fragments: its interactive type in reader
    // mode, its interactive term a variable, its clause unguarded.
    const agent = '''
Peer ::= Constant.
Request ::= post(String) ; quit.
procedure (Stream(Request)?)*agent(Peer?, Stream(String)).
(Reqs)*agent(Id, Outs?) :- serve(Reqs?, Id?, Outs).
''';

    test('procedure (T)*p(T1, ..., Tn) declares p of arity n, T in writer mode',
        () {
      final m = parse(responder);
      final v = m.volitionalDeclarations.single;
      expect(v.name, 'respond_coldcall');
      expect(v.arity, 2);
      expect(v.signature, 'respond_coldcall/2');
      final t = v.interactiveType as TypeRef;
      expect(t.name, 'Card');
      expect(t.isInput, isFalse);
      expect(v.readerMode, isFalse);
    });

    test('T in reader mode, as an argument type is', () {
      final v = parse(agent).volitionalDeclarations.single;
      expect(v.signature, 'agent/2');
      final t = v.interactiveType as TypeRef;
      expect(t.name, 'Stream');
      expect(t.isInput, isTrue);
      expect((t.typeArgs.single as TypeRef).name, 'Request');
      expect(v.readerMode, isTrue);
    });

    test('the declaration is that of the guarded clauses of arity n+1, T last',
        () {
      final m = parse(responder);
      final d = m.procDeclarations.singleWhere((d) => d.name == 'respond_coldcall');
      expect(d.key, 'respond_coldcall/3');
      expect(identical(d, m.volitionalDeclarations.single.decl), isTrue);
      expect((d.argTypes[0] as TypeRef).name, 'Offer');
      expect((d.argTypes[1] as TypeRef).name, 'Response');
      expect((d.argTypes[2] as TypeRef).name, 'Card');
    });

    test('(A)*p(S1, ..., Sn) :- G | B is the guarded clause p(S1, ..., Sn, A) '
        ':- G | B, A its interactive term', () {
      final p = parse(responder).procedures.single;
      expect(p.signature, 'respond_coldcall/3');
      expect(p.isVolitional, isTrue);
      final c = p.clauses.single;
      expect(c.head.arity, 3);
      expect(identical(c.head.args.last, c.interactiveTerm), isTrue);
      final a = c.interactiveTerm as StructTerm;
      expect(a.functor, 'card');
      expect(c.guards!.single.predicate, 'ground');
      expect(c.body!.single.functor, 'decide');
      expect(c.isVolitionGuarded, isFalse);

      // The same clause as GLP, written as that guarded clause.
      final glp = only('respond_coldcall(offer(From), Resp?, card(From?, Answer)) :-\n'
          '    ground(From?) | decide(Answer?, From?, Resp).');
      expect('$c', '$glp');
      expect(glp.interactiveTerm, isNull);
    });

    test('the interactive term may be a variable', () {
      final c = parse(agent).procedures.single.clauses.single;
      final a = c.interactiveTerm as VarTerm;
      expect(a.name, 'Reqs');
      expect(a.isReader, isFalse);
      expect(c.guards, isNull);
      expect(c.body!.single.functor, 'serve');
    });

    for (final anon in ['_', '_?', '_Reqs', '_Reqs?']) {
      test('the interactive term "$anon" is refused, T in writer mode', () {
        expect(
            () => parse('procedure (Card)*p(Offer?).\n'
                '($anon)*p(offer(From)) :- ground(From?) | true.'),
            refusedWith('anonymous variable'));
      });
      test('the interactive term "$anon" is refused, T in reader mode', () {
        expect(
            () => parse('procedure (Stream(Request)?)*agent(Peer?).\n'
                '($anon)*agent(Id) :- ground(Id?) | true.'),
            refusedWith('anonymous variable'));
      });
    }

    test('an anonymous variable inside the interactive term is no refusal', () {
      final c = parse('procedure (Card)*p(Offer?).\n'
              '(card(From?, _))*p(offer(From)) :- ground(From?) | true.')
          .procedures.single.clauses.single;
      expect((c.interactiveTerm as StructTerm).args.last, isA<UnderscoreTerm>());
    });

    test('an empty interactive term is refused', () {
      expect(() => parse('procedure (Card)*p(Offer?).\n'
              '()*p(offer(From)) :- ground(From?) | true.'),
          refusedWith('empty interactive term'));
    });

    test('sibling clauses stay one procedure, each with its interactive term',
        () {
      final p = parse('''
procedure (YesNo)*ask(Peer?).
(yes)*ask(From) :- ground(From?) | true.
(no)*ask(From) :- ground(From?) | true.
''').procedures.single;
      expect(p.signature, 'ask/2');
      expect(p.clauses.map((c) => (c.interactiveTerm as ConstTerm).value),
          ['yes', 'no']);
    });

    test('a nullary volitional procedure: procedure (T)*p. and (A)*p :- B', () {
      final m = parse('procedure (Stream(String)?)*chat.\n'
          '(Ms)*chat :- show(Ms?).');
      expect(m.volitionalDeclarations.single.signature, 'chat/0');
      expect(m.procedures.single.signature, 'chat/1');
      expect('${m.procedures.single.clauses.single.interactiveTerm}', 'Ms');
    });

    test('ordinary procedures beside it are not volitional', () {
      final m = parse('$agent\n'
          'procedure serve(Stream(Request)?, Peer?, Stream(String)).\n'
          'serve([quit|_], _, []).');
      expect(m.procedures.map((p) => p.isVolitional), [true, false]);
      expect(m.volitionalDeclarations.length, 1);
    });

    test('exported, as any procedure declaration', () {
      final m = parse('exported procedure (Card)*respond(Offer?).\n'
          '(card(From?, A))*respond(offer(From)) :- ground(From?) | d(A?).');
      final v = m.volitionalDeclarations.single;
      expect(v.decl.exported, isTrue);
      expect(v.signature, 'respond/1');
    });

    test('with a parameter list, as any procedure declaration', () {
      final v = parse('procedure(X) (Stream(X)?)*take(X?).\n'
              '(Xs)*take(Y) :- ground(Y?) | drop(Xs?).')
          .volitionalDeclarations.single;
      expect(v.decl.typeParams, ['X']);
      expect((v.interactiveType as TypeRef).name, 'Stream');
      expect(v.signature, 'take/1');
    });

    // An import mirrors its export's declaration (TGLP modules.tex,
    // "Self-contained type checking"), so a volitional export is imported as
    // it is declared (GLP #3 Cowork, 2026-10-10 07:48 UTC, "19:55").  Until
    // 2026-10-10 `imported procedure (T)*M#p(...)` was refused, "An imported
    // declaration carries no interactive type".
    for (final (mode, exportSource, importSource) in [
      (
        'T in writer mode',
        'exported procedure (Card)*respond(Offer?).\n'
            '(card(From?, A))*respond(offer(From)) :- ground(From?) | d(A?).',
        'imported procedure (Card)*m#respond(Offer?).',
      ),
      (
        'T in reader mode',
        'exported procedure (Stream(Request)?)*agent(Peer?, Stream(String)).\n'
            '(Reqs)*agent(Id, Outs?) :- serve(Reqs?, Id?, Outs).',
        'imported procedure (Stream(Request)?)*m#agent(Peer?, Stream(String)).',
      ),
      (
        'with a parameter list',
        'exported procedure(X) (Stream(X)?)*take(X?).\n'
            '(Xs)*take(Y) :- ground(Y?) | drop(Xs?).',
        'imported procedure(X) (Stream(X)?)*m#take(X?).',
      ),
    ]) {
      test('a volitional export is imported as it is declared, $mode', () {
        final e = parse(exportSource).volitionalDeclarations.single;
        final i = parse(importSource).volitionalDeclarations.single;
        expect(e.decl.exported, isTrue);
        expect(i.decl.imported, isTrue);
        expect(i.decl.exported, isFalse);
        expect(i.decl.modulePath, 'm');
        expect(i.signature, e.signature);
        expect(i.decl.key, e.decl.key);
        expect('${i.interactiveType}', '${e.interactiveType}');
        expect(i.readerMode, e.readerMode);
        expect(i.decl.typeParams, e.decl.typeParams);
        expect(i.decl.argTypes.map((t) => '$t'),
            e.decl.argTypes.map((t) => '$t'));
      });
    }

    test('a module importing a volitional export and calling it loads in '
        'vGLP mode, the import with no clauses of its own', () {
      final m = parse('''
Peer ::= Constant.
Offer ::= offer(Peer).
YesNo ::= yes ; no.
Card ::= card(Peer, YesNo?).
imported procedure (Card)*m#respond(Offer?).
procedure go(Peer?).
go(P) :- m # respond(offer(P?)).
''');
      final v = m.volitionalDeclarations.single;
      expect(v.decl.imported, isTrue);
      expect('$v', 'procedure (Card)*respond(Offer?).');
      expect(m.procDeclarations.map((d) => '$d'),
          ['imported procedure m#respond(Offer?, Card).', 'procedure go(Peer?).']);
      expect(m.procedures.map((p) => p.signature), ['go/1']);
      final call = m.procedures.single.clauses.single.body!.single as RemoteGoal;
      expect(call.staticModuleName, 'm');
      expect('${call.goal}', 'respond(offer(P?))');
    });

    test('a clause (A)*p of no procedure declared (T)*p is refused', () {
      expect(() => parse('(yes)*ask(From) :- ground(From?) | true.'),
          refusedWith('is of no procedure declared'));
      expect(
          () => parse('procedure ask(Peer?, YesNo).\n'
              '(yes)*ask(From) :- ground(From?) | true.'),
          refusedWith('is of no procedure declared'));
    });

    test('a clause of a volitional procedure not written (A)*p is refused', () {
      expect(
          () => parse('procedure (YesNo)*ask(Peer?).\n'
              '(yes)*ask(From) :- ground(From?) | true.\n'
              'ask(From, no) :- ground(From?) | true.'),
          refusedWith('is written "(A)*ask(S1, ..., Sn) :- G | B"'));
    });

    test('in a .glp source the declaration is a parse error', () {
      expect(() => parse(agent, vglp: false),
          refusedWith('may appear only in a .vglp source'));
    });

    test('in a .glp source the clause is a parse error', () {
      expect(
          () => parse('(Reqs)*agent(Id, Outs?) :- serve(Reqs?, Id?, Outs).',
              vglp: false),
          refusedWith('may appear only in a .vglp source'));
    });

    // The sources of programs/tests/vglp in the Definition's syntax.
    for (final (path, volitional) in [
      ('tests/vglp/fragments/questions.vglp',
          {'agent/3': 'Stream(Request)?', 'respond_coldcall/2': 'Card'}),
      ('tests/vglp/stream/chat.vglp', {'chat/2': 'Stream(String)?'}),
    ]) {
      test('$path parses, its volitional procedures as declared', () {
        final m = parse(File('../programs/$path').readAsStringSync());
        expect({
          for (final v in m.volitionalDeclarations)
            v.signature: '${v.interactiveType}'
        }, volitional);
        for (final p in m.procedures) {
          final declared = volitional.containsKey('${p.name}/${p.arity - 1}');
          expect(p.isVolitional, declared, reason: p.signature);
        }
      });
    }
  });

  group('volition guard', () {
    test('bare * is the guard with no question and no context', () {
      final c = only('* p(X) :- ground(X?) | true.');
      expect(c.isVolitionGuarded, isTrue);
      expect(c.volitionGuard!.question, isEmpty);
      expect(c.volitionGuard!.context, isEmpty);
    });

    test('X=T names the answer writer and its ground value', () {
      final c = only('*(Answer=yes) p(Answer) :- true | true.');
      final q = c.volitionGuard!.question.single;
      expect(q.writer!.name, 'Answer');
      expect((q.value as ConstTerm).value, 'yes');
      expect(q.isField, isFalse);
    });

    test('a bare writer abbreviates X=_ and is a field', () {
      final c = only('*(Amount) p(Amount) :- true | true.');
      final q = c.volitionGuard!.question.single;
      expect(q.writer!.name, 'Amount');
      expect(q.isField, isTrue);
    });

    test('a bare ground term abbreviates _=T', () {
      final c = only('*(yes) p(X) :- ground(X?) | true.');
      final q = c.volitionGuard!.question.single;
      expect(q.writer, isNull);
      expect((q.value as ConstTerm).value, 'yes');
    });

    test('a reader is a context position, not a question position', () {
      final c = only('*(Answer=yes, From?) p(From, Answer) :- ground(From?) | true.');
      expect(c.volitionGuard!.question.length, 1);
      expect(c.volitionGuard!.context.single.name, 'From');
      expect(c.volitionGuard!.context.single.isReader, isTrue);
    });

    test('question and context keep their order across a mixed guard', () {
      final c = only('*(yes, From?, WantSpec?, Offered?) '
          'p(From, WantSpec, Offered) :- ground(From?) | true.');
      expect(c.volitionGuard!.question.length, 1);
      expect(c.volitionGuard!.context.map((v) => v.name),
          ['From', 'WantSpec', 'Offered']);
    });

    test('sibling volition-guarded clauses stay one procedure', () {
      final p = parse('*(Answer=no, From?) r(From, Answer) :- ground(From?) | true.\n'
              '*(Answer=yes, From?) r(From, Answer) :- ground(From?) | true.')
          .procedures.single;
      expect(p.signature, 'r/2');
      expect(p.clauses.length, 2);
      expect(p.clauses.every((c) => c.isVolitionGuarded), isTrue);
    });

    test('an ordinary clause of the same procedure carries no guard', () {
      final p = parse('*(Answer=yes) r(Answer) :- true | true.\n'
              'r(no).')
          .procedures.single;
      expect(p.clauses.first.isVolitionGuarded, isTrue);
      expect(p.clauses.last.isVolitionGuarded, isFalse);
    });

    test('a volition guard in a .glp source is a parse error', () {
      expect(() => parse('*(Answer=yes) p(Answer) :- true | true.', vglp: false),
          throwsA(isA<CompileError>()));
    });
  });

  group('else-branch', () {
    test('the responder exhibit of Section 4 parses', () {
      final c = only('''
*(Answer=yes, From?)
respond_coldcall(offer(From), Resp?,
    [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true
*(no) true.
''');
      expect(c.isVolitionGuarded, isTrue);
      final e = c.elseBranch!;
      expect((e.answer.single as ConstTerm).value, 'no');
      expect(e.body.single.functor, 'true');
    });

    test('the else answer may be a reader the guard makes ground', () {
      final c = only('*(Ans=yes, From?) p(From, Ans) :- ground(From?) | q(Ans?) '
          '*(From?) r(From?).');
      expect((c.elseBranch!.answer.single as VarTerm).isReader, isTrue);
      expect(c.elseBranch!.body.single.functor, 'r');
    });

    test('an else body may be a conjunction', () {
      final c = only('*(A=yes) p(A) :- true | q(A?) *(no) r(A?), s(A?).');
      expect(c.elseBranch!.body.map((g) => g.functor), ['r', 's']);
    });

    test('an else answer of the wrong width is a parse error', () {
      expect(() => only('*(A=yes, B=up) p(A, B) :- true | q(A?) *(no) true.'),
          throwsA(isA<CompileError>()));
    });

    test('an else-branch without a volition guard is a parse error', () {
      expect(() => only('p(A) :- true | q(A?) *(no) true.'),
          throwsA(isA<CompileError>()));
    });

    test('multiplication in a body goal is not read as an else-branch', () {
      final c = only('*(A=yes) p(A, X, Y, Z) :- ground(X?), ground(Y?) | '
          'Z := X? * (Y? + 1).');
      expect(c.elseBranch, isNull);
      expect(c.body!.single.functor, ':=');
    });
  });

  group('GLP is vGLP without volition guards', () {
    test('an ordinary program parses the same in either mode', () {
      const src = 'merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).\n'
          'merge([], Ys, Ys?).';
      final a = parse(src, vglp: false).procedures.single;
      final b = parse(src, vglp: true).procedures.single;
      expect(a.clauses.length, b.clauses.length);
      expect(a.clauses.every((c) => !c.isVolitionGuarded), isTrue);
      expect(b.clauses.every((c) => !c.isVolitionGuarded), isTrue);
    });
  });
}
