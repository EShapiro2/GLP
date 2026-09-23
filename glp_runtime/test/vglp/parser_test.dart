// glp_runtime/test/vglp/parser_test.dart
//
// The vGLP surface syntax: a volition guard before a clause and an else-branch
// after its body.
// Spec: vGLP, sections/vglp.tex, Definition "Guarded Clause, Volition-Guarded
// Clause, Volition Guard, Question, Answer, Context, Else-Branch, Ordinary
// Clause, Procedure, vGLP Program", and the responder exhibit after it.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

void main() {
  Module parse(String source, {bool vglp = true}) =>
      Parser(Lexer(source).tokenize(), vglp: vglp).parseModule();

  Clause only(String source) => parse(source).procedures.single.clauses.single;

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

  group('the question parameters of a procedure declaration', () {
    // vGLP, sections/vglp.tex, Section "Volition-Guarded GLP": "A procedure
    // declaration carries its question parameters after its argument list,
    // procedure p(...) *(X1, ..., Xm)., being every writer that a volition
    // guard of the procedure names, each of them once".
    ProcDecl decl(String source, {bool vglp = true}) =>
        parse(source, vglp: vglp).procDeclarations.single;

    test('the list follows the argument list and names writers', () {
      final d = decl('procedure mint(Constant?, Constant) *(K, Maturity).\n'
          'mint(A, b) :- true | true.');
      expect(d.questionParams, ['K', 'Maturity']);
    });

    test('a declaration with no list carries no question parameters', () {
      final d = decl('procedure plain(Constant?, Constant).\n'
          'plain(A, b) :- true | true.');
      expect(d.questionParams, isEmpty);
    });

    test('the list follows a type-parameter list and "exported" alike', () {
      final d = decl('exported procedure(X) q(Stream(X)?, Constant) *(A).\n'
          'q(S, c) :- true | true.');
      expect(d.typeParams, ['X']);
      expect(d.questionParams, ['A']);
      expect(d.exported, isTrue);
    });

    test('a reader is not a writer and is refused', () {
      expect(
          () => decl('procedure t(Constant?, Constant) *(K?).\n'
              't(A, b) :- true | true.'),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              contains('"K?" is not a writer'))));
    });

    test('a name twice is refused: each of them once', () {
      expect(
          () => decl('procedure s(Constant?, Constant) *(K, K).\n'
              's(A, b) :- true | true.'),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              contains('named twice'))));
    });

    test('a .glp source takes the list too, being what the compilation emits',
        () {
      final d = decl('procedure mint(Constant?, Constant) *(K).\n'
          'mint(A, b) :- true | true.', vglp: false);
      expect(d.questionParams, ['K']);
    });

    test('the declaration prints its list back', () {
      final d = decl('procedure mint(Constant?, Constant) *(K, Maturity).\n'
          'mint(A, b) :- true | true.');
      expect(d.toString(), 'procedure mint(Constant?, Constant) *(K, Maturity).');
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
