import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/compiler/ast.dart';

void main() {
  group('Lexer - HASH token', () {
    test('recognizes # token', () {
      final lexer = Lexer('math # factorial');
      final tokens = lexer.tokenize();

      expect(tokens.length, equals(4));  // math, #, factorial, EOF
      expect(tokens[0].type, equals(TokenType.ATOM));
      expect(tokens[1].type, equals(TokenType.HASH));
      expect(tokens[2].type, equals(TokenType.ATOM));
    });
  });

  group('Parser - Module declarations', () {
    test('parses -module declaration', () {
      final source = '-module(math). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.name, equals('math'));
    });

    test('parses hierarchical module name', () {
      final source = '-module(utils.list). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.name, equals('utils.list'));
    });

    test('parses -export declaration', () {
      final source = '-export([foo/1, bar/2]). foo(1). bar(1, 2).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.exports, containsAll(['foo/1', 'bar/2']));
    });

    test('parses -import declaration', () {
      final source = '-import([list, math]). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.imports, containsAll(['list', 'math']));
    });

    test('parses -language declaration', () {
      final source = '-language(compound). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.language, equals('compound'));
    });

    test('parses -mode declaration', () {
      final source = '-mode(trust). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.mode, equals('trust'));
    });

    test('parses -service_type declaration', () {
      final source = '-service_type(monitor). foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.serviceType, equals('monitor'));
    });

    test('parses multiple declarations', () {
      final source = '''
        -module(math).
        -export([factorial/2]).
        -import([base]).
        factorial(0, 1).
      ''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.name, equals('math'));
      expect(moduleInfo.exports, contains('factorial/2'));
      expect(moduleInfo.imports, contains('base'));
    });

    test('defaults for missing declarations', () {
      final source = 'foo(1).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final moduleInfo = parser.parseModuleInfo();

      expect(moduleInfo.name, isNull);
      expect(moduleInfo.exports, isEmpty);
      expect(moduleInfo.imports, isEmpty);
      expect(moduleInfo.language, equals('glp'));
      expect(moduleInfo.mode, equals('user'));
      expect(moduleInfo.serviceType, equals('procedures'));
    });
  });

  group('Parser - Remote goals', () {
    test('parses Module # Goal in body', () {
      final source = 'test(X) :- math # factorial(5, X).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final program = parser.parse();

      expect(program.procedures.length, equals(1));
      final clause = program.procedures[0].clauses[0];
      expect(clause.body, isNotNull);
      expect(clause.body!.length, equals(1));

      final goal = clause.body![0];
      expect(goal, isA<RemoteGoal>());

      final remoteGoal = goal as RemoteGoal;
      expect(remoteGoal.moduleName, equals('math'));
      expect(remoteGoal.goal.functor, equals('factorial'));
      expect(remoteGoal.goal.args.length, equals(2));
    });

    test('parses chained Module # Goal', () {
      final source = 'test(X) :- a # b # foo(X).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);

      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      final goal = clause.body![0] as RemoteGoal;

      // a # (b # foo(X)) - nested remote goal
      expect(goal.moduleName, equals('a'));
      expect(goal.goal, isA<RemoteGoal>());

      final innerGoal = goal.goal as RemoteGoal;
      expect(innerGoal.moduleName, equals('b'));
      expect(innerGoal.goal.functor, equals('foo'));
    });
  });

  group('GlpCompiler.compileModule - with declarations', () {
    test('uses -module declaration for name', () {
      final compiler = GlpCompiler();
      // Use simple fact to avoid SRSW violations
      final source = '''
        -module(mymath).
        double(2).
      ''';

      final module = compiler.compileModule(source);

      expect(module.name, equals('mymath'));
    });

    test('uses -export declaration for exports', () {
      final compiler = GlpCompiler();
      // Use simple facts to avoid SRSW violations
      final source = '''
        -module(mymath).
        -export([double/1]).
        double(2).
        triple(3).
      ''';

      final module = compiler.compileModule(source);

      expect(module.isExported('double', 1), isTrue);
      expect(module.isExported('triple', 1), isFalse);
    });

    test('parameter overrides declaration', () {
      final compiler = GlpCompiler();
      final source = '''
        -module(declared_name).
        foo(1).
      ''';

      final module = compiler.compileModule(source, moduleName: 'override_name');

      expect(module.name, equals('override_name'));
    });

    test('compiles module with remote goal', () {
      final compiler = GlpCompiler();
      // Use zero-arity to avoid SRSW violation
      final source = '''
        -module(caller).
        test :- math # compute.
      ''';

      // Should compile without error
      final module = compiler.compileModule(source);

      expect(module.name, equals('caller'));
      expect(module.procedures, contains('test/0'));
    });
  });
}
