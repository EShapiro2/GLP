import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/compiler/ast.dart';

void main() {
  group('Lexer', () {
    test('tokenizes simple atoms and punctuation', () {
      final lexer = Lexer('p(X, Y).');
      final tokens = lexer.tokenize();

      expect(tokens.length, 8); // p ( X , Y ) . EOF
      expect(tokens[0].type, TokenType.ATOM);
      expect(tokens[0].lexeme, 'p');
      expect(tokens[1].type, TokenType.LPAREN);
      expect(tokens[2].type, TokenType.VARIABLE);
      expect(tokens[2].lexeme, 'X');
      expect(tokens[3].type, TokenType.COMMA);
      expect(tokens[4].type, TokenType.VARIABLE);
      expect(tokens[5].type, TokenType.RPAREN);
      expect(tokens[6].type, TokenType.DOT);
      expect(tokens[7].type, TokenType.EOF);
    });

    test('tokenizes reader variables', () {
      final lexer = Lexer('p(X, X?).');
      final tokens = lexer.tokenize();

      expect(tokens[2].type, TokenType.VARIABLE);
      expect(tokens[2].lexeme, 'X');
      expect(tokens[4].type, TokenType.READER);
      expect(tokens[4].lexeme, 'X');
    });

    test('tokenizes lists', () {
      final lexer = Lexer('[X|Xs]');
      final tokens = lexer.tokenize();

      expect(tokens[0].type, TokenType.LBRACKET);
      expect(tokens[1].type, TokenType.VARIABLE);
      expect(tokens[2].type, TokenType.PIPE);
      expect(tokens[3].type, TokenType.VARIABLE);
      expect(tokens[4].type, TokenType.RBRACKET);
    });

    test('tokenizes numbers', () {
      final lexer = Lexer('42 3.14 -17');
      final tokens = lexer.tokenize();

      expect(tokens[0].type, TokenType.NUMBER);
      expect(tokens[0].literal, 42);
      expect(tokens[1].type, TokenType.NUMBER);
      expect(tokens[1].literal, 3.14);
      expect(tokens[2].type, TokenType.NUMBER);
      expect(tokens[2].literal, -17);
    });

    test('tokenizes strings', () {
      final lexer = Lexer('"hello" \'world\'');
      final tokens = lexer.tokenize();

      expect(tokens[0].type, TokenType.STRING);
      expect(tokens[0].literal, 'hello');
      expect(tokens[1].type, TokenType.STRING);
      expect(tokens[1].literal, 'world');
    });

    test('tokenizes clause with guards and body', () {
      final lexer = Lexer('p(X) :- ground(X?) | q(X?).');
      final tokens = lexer.tokenize();

      expect(tokens.any((t) => t.type == TokenType.IMPLIES), isTrue);
      expect(tokens.where((t) => t.type == TokenType.PIPE).length, 1);
    });

    test('skips line comments', () {
      final lexer = Lexer('p(X). % This is a comment\nq(Y).');
      final tokens = lexer.tokenize();

      // Should only have tokens for p(X). and q(Y).
      final atomTokens = tokens.where((t) => t.type == TokenType.ATOM).toList();
      expect(atomTokens.length, 2);
      expect(atomTokens[0].lexeme, 'p');
      expect(atomTokens[1].lexeme, 'q');
    });

    test('skips block comments', () {
      final lexer = Lexer('p(X). /* multi\nline\ncomment */ q(Y).');
      final tokens = lexer.tokenize();

      final atomTokens = tokens.where((t) => t.type == TokenType.ATOM).toList();
      expect(atomTokens.length, 2);
    });

    test('handles underscore', () {
      final lexer = Lexer('p(_).');
      final tokens = lexer.tokenize();

      expect(tokens[2].type, TokenType.UNDERSCORE);
    });
  });

  group('Parser', () {
    test('parses simple fact', () {
      final lexer = Lexer('p(a).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      expect(program.procedures.length, 1);
      expect(program.procedures[0].name, 'p');
      expect(program.procedures[0].arity, 1);
      expect(program.procedures[0].clauses.length, 1);

      final clause = program.procedures[0].clauses[0];
      expect(clause.guards, isNull);
      expect(clause.body, isNull);
    });

    test('parses clause with variables', () {
      final lexer = Lexer('p(X, Y).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.head.args.length, 2);
      expect(clause.head.args[0], isA<VarTerm>());
      expect((clause.head.args[0] as VarTerm).name, 'X');
    });

    test('parses clause with reader', () {
      final lexer = Lexer('p(X, X?).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect((clause.head.args[0] as VarTerm).isReader, isFalse);
      expect((clause.head.args[1] as VarTerm).isReader, isTrue);
    });

    test('parses list syntax', () {
      final lexer = Lexer('p([X|Xs]).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final listArg = clause.head.args[0] as ListTerm;
      expect(listArg.head, isA<VarTerm>());
      expect(listArg.tail, isA<VarTerm>());
    });

    test('parses empty list', () {
      final lexer = Lexer('p([]).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final listArg = clause.head.args[0] as ListTerm;
      expect(listArg.isNil, isTrue);
    });

    test('parses multi-element list', () {
      final lexer = Lexer('p([a, b, c]).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final listArg = clause.head.args[0] as ListTerm;

      // [a, b, c] -> [a|[b|[c|[]]]]
      expect(listArg.head, isA<ConstTerm>());
      expect((listArg.head as ConstTerm).value, 'a');
      expect(listArg.tail, isA<ListTerm>());
    });

    test('parses clause with body', () {
      final lexer = Lexer('p(X) :- q(X).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.body, isNotNull);
      expect(clause.body!.length, 1);
      expect(clause.body![0].functor, 'q');
    });

    test('parses clause with guards and body', () {
      final lexer = Lexer('p(X) :- ground(X?) | q(X?).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.guards, isNotNull);
      expect(clause.guards!.length, 1);
      expect(clause.guards![0].predicate, 'ground');
      expect(clause.body, isNotNull);
      expect(clause.body!.length, 1);
      expect(clause.body![0].functor, 'q');
    });

    test('parses multiple clauses for same procedure', () {
      final lexer = Lexer('''
        merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
        merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
        merge([], [], []).
      ''');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      expect(program.procedures.length, 1);
      expect(program.procedures[0].name, 'merge');
      expect(program.procedures[0].arity, 3);
      expect(program.procedures[0].clauses.length, 3);
    });

    test('parses multiple procedures', () {
      final lexer = Lexer('''
        p(a).
        p(b).
        q(X) :- p(X).
      ''');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      expect(program.procedures.length, 2);
      expect(program.procedures[0].name, 'p');
      expect(program.procedures[0].clauses.length, 2);
      expect(program.procedures[1].name, 'q');
      expect(program.procedures[1].clauses.length, 1);
    });

    test('parses structures', () {
      final lexer = Lexer('p(f(X, Y)).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final structArg = clause.head.args[0] as StructTerm;
      expect(structArg.functor, 'f');
      expect(structArg.arity, 2);
    });

    test('parses underscore', () {
      final lexer = Lexer('p(_).');
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.head.args[0], isA<UnderscoreTerm>());
    });
  });
}
