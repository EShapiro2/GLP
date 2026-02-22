import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

void main() {
  // Helper: parse source into Module AST
  Module parseModule(String source) {
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    return parser.parseModule();
  }

  group('Phase 1 - 2a: exported procedure parsing', () {
    test('exported procedure declaration is parsed correctly', () {
      final source = '''
exported procedure factorial(Integer?, Integer).
factorial(0, 1).
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.name, 'factorial');
      expect(decl.exported, true);
      expect(decl.argTypes.length, 2);
    });

    test('plain procedure declaration has exported=false', () {
      final source = '''
procedure helper(Integer?, Integer).
helper(N, R) :- true | R := N? * 2.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.name, 'helper');
      expect(decl.exported, false);
    });

    test('exported procedure with moded types', () {
      final source = '''
exported procedure agent(Constant?, AgentChannel?, NetChannel?).
agent(Id, ACh, NCh) :- true | true.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.exported, true);
      expect(decl.name, 'agent');
      expect(decl.argTypes.length, 3);
    });

    test('multiple procedures, some exported some not', () {
      final source = '''
exported procedure factorial(Integer?, Integer).
factorial(0, 1).
factorial(N, F) :- N? > 0 | N1 := N? - 1, factorial(N1?, F1), F := N? * F1?.

procedure helper(Integer?, Integer).
helper(N, R) :- true | R := N? * 2.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 2);
      expect(module.procDeclarations[0].exported, true);
      expect(module.procDeclarations[0].name, 'factorial');
      expect(module.procDeclarations[1].exported, false);
      expect(module.procDeclarations[1].name, 'helper');
    });
  });

  group('Phase 1 - 2b: imported procedure parsing', () {
    test('imported procedure with module path', () {
      final source = '''
-module(main).
imported procedure social#agent(Constant?, AgentChannel?).
boot :- true | social # agent(id, Ch).
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.imported, true);
      expect(decl.exported, false);
      expect(decl.modulePath, 'social');
      expect(decl.name, 'agent');
      expect(decl.argTypes.length, 2);
    });

    test('imported procedure with deep path', () {
      final source = '''
-module(main).
imported procedure ui#actors#render(Widget?, Screen).
boot :- true | true.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.imported, true);
      expect(decl.modulePath, 'ui#actors');
      expect(decl.name, 'render');
      expect(decl.argTypes.length, 2);
    });

    test('imported procedure from ancestor scope (no path)', () {
      final source = '''
-module(main).
imported procedure merge(Stream?, Stream?, Stream).
boot :- true | true.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.imported, true);
      expect(decl.modulePath, isNull);
      expect(decl.name, 'merge');
      expect(decl.argTypes.length, 3);
    });

    test('imported procedure with qualified types using #', () {
      final source = '''
-module(boot).
imported procedure social#agent(Constant?, social#AgentChannel?).
boot :- true | true.
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 1);
      final decl = module.procDeclarations[0];
      expect(decl.imported, true);
      expect(decl.modulePath, 'social');
      expect(decl.name, 'agent');
      // The second arg type should reference 'social#AgentChannel'
      expect(decl.argTypes[1], isA<TypeRef>());
      final typeRef = decl.argTypes[1] as TypeRef;
      expect(typeRef.name, 'social#AgentChannel');
      expect(typeRef.isInput, true);
    });

    test('imported procedure has no clauses (declaration only)', () {
      // Imported procedures are pure declarations — no clauses follow
      final source = '''
-module(main).
imported procedure math#factorial(Integer?, Integer).
exported procedure compute(Integer?, Integer).
compute(N, R?) :- true | math # factorial(N?, R).
''';
      final module = parseModule(source);

      expect(module.procDeclarations.length, 2);
      expect(module.procDeclarations[0].imported, true);
      expect(module.procDeclarations[0].name, 'factorial');
      expect(module.procDeclarations[1].exported, true);
      expect(module.procDeclarations[1].name, 'compute');
      // Only compute has clauses
      expect(module.procedures.length, 1);
      expect(module.procedures[0].name, 'compute');
    });
  });

  group('Phase 1 - 2c: rejection of old syntax', () {
    test('-export([...]) is rejected with parse error', () {
      final source = '''
-export([factorial/2]).
factorial(0, 1).
''';
      expect(
        () => parseModule(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains('no longer supported'),
        )),
      );
    });

    test('-import([...]) is rejected with parse error', () {
      final source = '''
-import([math]).
boot.
''';
      expect(
        () => parseModule(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains('no longer supported'),
        )),
      );
    });
  });

  group('Phase 1 - 2d: -module(name) still works', () {
    test('-module(name) still parses', () {
      final source = '''
-module(math).
procedure factorial(Integer?, Integer).
factorial(0, 1).
''';
      final module = parseModule(source);

      expect(module.declaration, isNotNull);
      expect(module.declaration!.name, 'math');
    });
  });

  group('Phase 1 - 2e: Module # Goal still works', () {
    test('remote goal parsing unchanged', () {
      final source = '''
boot :- true | math # factorial(5, R).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();

      final clause = program.procedures[0].clauses[0];
      expect(clause.body, isNotNull);
      expect(clause.body!.length, 1);

      final goal = clause.body![0];
      expect(goal, isA<RemoteGoal>());
      final remote = goal as RemoteGoal;
      expect(remote.staticModuleName, 'math');
      expect(remote.goal.functor, 'factorial');
    });
  });

  group('Phase 1 - 2f: type-only file', () {
    test('file with only type definitions and no procedures parses successfully', () {
      final source = '''
Response ::= accept(Channel) ; no.
AgentContent ::= befriend(Constant, Response?).
''';
      final module = parseModule(source);

      expect(module.typeDefs.length, 2);
      expect(module.procedures, isEmpty);
      expect(module.procDeclarations, isEmpty);
    });
  });
}
