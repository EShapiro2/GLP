// glp_runtime/test/vglp/display_decl_test.dart
//
// Display declarations: how a construct looks and how the program's output is
// viewed, declared as the types are.
// Spec: vGLP, sections/elicitation.tex, subsection "Display Declarations",
// Definition "Display Declaration, Default Display".

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  Module parse(String source, {bool vglp = true}) =>
      Parser(Lexer(source).tokenize(), vglp: vglp).parseModule();

  DisplayDecl only(String source, {bool vglp = true}) =>
      parse(source, vglp: vglp).displayDecls.single;

  group('the clause form', () {
    test('names its predicate, its guard and every item', () {
      final d = only('display pay *(Friend, Coin, Amount) : '
          'panel(wallet), label("Pay a friend"), field(Friend, peer), '
          'field(Amount, number), persistent.\n'
          '*(Friend, Coin, Amount) pay(Friend, Coin, Amount) :- '
          'ground(Friend?), ground(Coin?), ground(Amount?) | true.');
      expect(d.isClauseForm, isTrue);
      expect(d.predicate, 'pay');
      expect(d.guard!.question.map((q) => q.writer!.name),
          ['Friend', 'Coin', 'Amount']);
      expect(d.items.map((i) => i.name),
          ['panel', 'label', 'field', 'field', 'persistent']);
    });

    test('a field names a writer of the question and its widget', () {
      final d = only('display pay *(Amount) : field(Amount, number).\n'
          '*(Amount) pay(Amount) :- ground(Amount?) | true.');
      final field = d.items.single;
      expect((field.args[0] as VarTerm).name, 'Amount');
      expect((field.args[1] as ConstTerm).value, 'number');
    });

    test('transient stands alone, as persistent does', () {
      final d = only('display r *(Answer=yes, From?) : transient.\n'
          '*(Answer=yes, From?) r(From, Answer) :- ground(From?) | true.');
      expect(d.items.single.name, 'transient');
      expect(d.items.single.args, isEmpty);
    });

    test('the guard is what tells sibling clauses apart', () {
      final ds = parse('display r *(Answer=yes, From?) : label("Accept").\n'
              'display r *(Answer=no, From?) : label("Decline").\n'
              '*(Answer=yes, From?) r(From, Answer) :- ground(From?) | true.\n'
              '*(Answer=no, From?) r(From, Answer) :- ground(From?) | true.')
          .displayDecls;
      expect(ds.length, 2);
      expect(ds.map((d) => (d.guard!.question.single.value as ConstTerm).value),
          ['yes', 'no']);
    });
  });

  group('the message form', () {
    test('a pattern with arguments takes panel and view', () {
      final d = only('display msg(agent, person, chat(Who, Text)) : '
          'panel(chats), view(thread).');
      expect(d.isClauseForm, isFalse);
      expect((d.pattern as StructTerm).functor, 'msg');
      expect(d.items.map((i) => i.name), ['panel', 'view']);
      expect((d.items[1].args.single as ConstTerm).value, 'thread');
    });

    test('a bare atom is a pattern too', () {
      final d = only('display enrolled : panel(family), view(list).');
      expect((d.pattern as ConstTerm).value, 'enrolled');
    });
  });

  group('where they may stand', () {
    test('a .glp source carries them, because the compilation emits them', () {
      final d = only('display balances : panel(wallet), view(balances).',
          vglp: false);
      expect((d.pattern as ConstTerm).value, 'balances');
    });

    test('one may stand between a procedure declaration and its clauses', () {
      final m = parse('procedure p(Constant?).\n'
          'display p *(X) : field(X, text).\n'
          '*(X) p(X) :- ground(X?) | true.');
      expect(m.displayDecls.length, 1);
      expect(m.procedures.single.signature, 'p/1');
    });

    test('a program with none parses and declares none', () {
      expect(parse('p(a).').displayDecls, isEmpty);
    });

    test('a procedure named display is still a procedure', () {
      final m = parse('display(X) :- ground(X?) | true.');
      expect(m.displayDecls, isEmpty);
      expect(m.procedures.single.signature, 'display/1');
    });

    test('a missing ":" is a parse error', () {
      expect(() => parse('display p *(X) panel(a).'),
          throwsA(isA<CompileError>()));
    });
  });
}
