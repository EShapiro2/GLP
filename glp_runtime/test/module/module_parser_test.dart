import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/token.dart';

void main() {
  group('Module Parser - Lexer', () {
    test('lexer recognizes HASH token', () {
      final lexer = Lexer('a # b');
      final tokens = lexer.tokenize();

      expect(tokens.length, 4);  // a, #, b, EOF
      expect(tokens[0].type, TokenType.ATOM);
      expect(tokens[0].lexeme, 'a');
      expect(tokens[1].type, TokenType.HASH);
      expect(tokens[1].lexeme, '#');
      expect(tokens[2].type, TokenType.ATOM);
      expect(tokens[2].lexeme, 'b');
    });

    test('lexer handles module declaration tokens', () {
      final lexer = Lexer('-module(math).');
      final tokens = lexer.tokenize();

      expect(tokens.length, 7);  // -, module, (, math, ), ., EOF
      expect(tokens[0].type, TokenType.MINUS);
      expect(tokens[1].type, TokenType.ATOM);
      expect(tokens[1].lexeme, 'module');
      expect(tokens[2].type, TokenType.LPAREN);
      expect(tokens[3].type, TokenType.ATOM);
      expect(tokens[3].lexeme, 'math');
      expect(tokens[4].type, TokenType.RPAREN);
      expect(tokens[5].type, TokenType.DOT);
    });

    test('lexer handles export declaration tokens', () {
      final lexer = Lexer('-export([foo/1, bar/2]).');
      final tokens = lexer.tokenize();

      expect(tokens.any((t) => t.lexeme == 'export'), true);
      expect(tokens.any((t) => t.lexeme == 'foo'), true);
      expect(tokens.any((t) => t.type == TokenType.SLASH), true);
    });

    test('lexer handles import declaration tokens', () {
      final lexer = Lexer('-import([math, io]).');
      final tokens = lexer.tokenize();

      expect(tokens.any((t) => t.lexeme == 'import'), true);
      expect(tokens.any((t) => t.lexeme == 'math'), true);
      expect(tokens.any((t) => t.lexeme == 'io'), true);
    });
  });

  group('Module Parser - Module Declaration', () {
    test('parser parses module declaration', () {
      final source = '''
-module(math).
factorial(0, 1).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.declaration, isNotNull);
      expect(module.declaration!.name, 'math');
      expect(module.procedures.length, 1);
      expect(module.procedures[0].name, 'factorial');
    });

    test('parser parses hierarchical module name', () {
      // Note: This test is for future hierarchical module support
      // For now, we just verify simple module names work
      final source = '-module(math).\nfoo.';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.declaration!.name, 'math');
    });
  });

  group('Module Parser - Export Declaration', () {
    test('parser parses export declaration', () {
      final source = '''
-module(math).
-export([factorial/2, gcd/3]).
factorial(0, 1).
gcd(A, 0, A?).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.exports.length, 1);
      expect(module.exports[0].exports.length, 2);
      expect(module.exports[0].exports[0].name, 'factorial');
      expect(module.exports[0].exports[0].arity, 2);
      expect(module.exports[0].exports[1].name, 'gcd');
      expect(module.exports[0].exports[1].arity, 3);
    });

    test('parser parses multiple export declarations', () {
      final source = '''
-module(math).
-export([factorial/2]).
-export([gcd/3]).
factorial(0, 1).
gcd(A, 0, A?).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.exports.length, 2);
      expect(module.exportedSignatures, {'factorial/2', 'gcd/3'});
    });

    test('parser parses empty export list', () {
      final source = '''
-module(math).
-export([]).
foo.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.exports.length, 1);
      expect(module.exports[0].exports.length, 0);
    });
  });

  group('Module Parser - Import Declaration', () {
    test('parser parses import declaration', () {
      final source = '''
-module(main).
-import([math, io]).
boot.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.imports.length, 1);
      expect(module.imports[0].imports.length, 2);
      expect(module.imports[0].imports[0], 'math');
      expect(module.imports[0].imports[1], 'io');
    });

    test('parser parses multiple import declarations', () {
      final source = '''
-module(main).
-import([math]).
-import([io]).
boot.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.imports.length, 2);
      expect(module.importedModules, ['math', 'io']);
    });

    test('parser parses empty import list', () {
      final source = '''
-module(main).
-import([]).
boot.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.imports.length, 1);
      expect(module.imports[0].imports.length, 0);
    });
  });

  group('Module Parser - Remote Goal', () {
    test('parser parses static remote goal', () {
      final source = '''
boot :- true | math # factorial(5, R).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      expect(program.procedures.length, 1);
      final clause = program.procedures[0].clauses[0];
      expect(clause.body, isNotNull);
      expect(clause.body!.length, 1);

      final goal = clause.body![0];
      expect(goal, isA<RemoteGoal>());

      final remote = goal as RemoteGoal;
      expect(remote.staticModuleName, 'math');
      expect(remote.isDynamic, false);
      expect(remote.goal.functor, 'factorial');
      expect(remote.goal.arity, 2);
    });

    test('parser parses dynamic remote goal', () {
      final source = '''
call_module(M, R) :- true | M # foo(R).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final goal = clause.body![0];
      expect(goal, isA<RemoteGoal>());

      final remote = goal as RemoteGoal;
      expect(remote.staticModuleName, isNull);
      expect(remote.isDynamic, true);
      expect(remote.module, isA<VarTerm>());
      expect((remote.module as VarTerm).name, 'M');
    });

    test('parser parses reader variable remote goal', () {
      final source = '''
call_module(M, R) :- true | M? # foo(R).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final goal = clause.body![0];
      expect(goal, isA<RemoteGoal>());

      final remote = goal as RemoteGoal;
      expect(remote.isDynamic, true);
      expect((remote.module as VarTerm).isReader, true);
    });

    test('parser parses chained remote goals', () {
      final source = '''
boot :- true |
    math # factorial(5, F),
    io # print(F?).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.body!.length, 2);
      expect(clause.body![0], isA<RemoteGoal>());
      expect(clause.body![1], isA<RemoteGoal>());
    });

    test('parser rejects module with arguments', () {
      final source = '''
boot :- true | foo(x) # bar.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      expect(() => parser.parse(), throwsA(anything));
    });
  });

  group('Module Parser - Complete Module', () {
    test('parser parses complete module file', () {
      final source = '''
-module(math).
-export([factorial/2, gcd/3]).

factorial(0, 1).
factorial(N, F) :-
    N? > 0 |
    N1 := N? - 1,
    factorial(N1?, F1),
    F := N? * F1?.

gcd(A, 0, A?).
gcd(A, B, G) :-
    B? > 0 |
    R := A? mod B?,
    gcd(B?, R?, G).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.name, 'math');
      expect(module.exportedSignatures, {'factorial/2', 'gcd/3'});
      expect(module.procedures.length, 2);
      expect(module.procedures[0].name, 'factorial');
      expect(module.procedures[1].name, 'gcd');
    });

    test('parser handles module without declarations', () {
      final source = '''
foo.
bar(X) :- baz(X?).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.declaration, isNull);
      expect(module.name, isNull);
      expect(module.exports, isEmpty);
      expect(module.imports, isEmpty);
      expect(module.procedures.length, 2);
    });

    test('legacy parse() skips declarations', () {
      final source = '''
-module(math).
-export([foo/0]).
-import([bar]).

foo.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();  // Legacy parse, skips declarations

      expect(program.procedures.length, 1);
      expect(program.procedures[0].name, 'foo');
    });
  });

  group('Module Parser - Remote Goal in Module', () {
    test('parser parses module with remote goals', () {
      final source = '''
-module(main).
-import([math]).
-export([boot/1]).

boot(_Args) :-
    true |
    math # factorial(5, F),
    math # gcd(48, 18, G).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.name, 'main');
      expect(module.importedModules, ['math']);
      expect(module.exportedSignatures, {'boot/1'});

      final bootClause = module.procedures[0].clauses[0];
      expect(bootClause.body!.length, 2);
      expect(bootClause.body![0], isA<RemoteGoal>());
      expect(bootClause.body![1], isA<RemoteGoal>());
    });
  });
}
