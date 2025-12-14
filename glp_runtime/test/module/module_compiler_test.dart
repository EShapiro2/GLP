import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/codegen.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';

/// Helper to compile GLP source to bytecode and return the ops list
List<dynamic> compile(String source) {
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final analyzer = Analyzer();
  final annotated = analyzer.analyze(program);
  final generator = CodeGenerator();
  final bytecode = generator.generate(annotated);
  return bytecode.ops;
}

void main() {
  group('ImportTable', () {
    test('assigns 1-based indices to imports', () {
      final table = ImportTable();

      expect(table.addImport('math'), 1);
      expect(table.addImport('io'), 2);
      expect(table.addImport('utils'), 3);
    });

    test('returns same index for duplicate imports', () {
      final table = ImportTable();

      expect(table.addImport('math'), 1);
      expect(table.addImport('math'), 1);  // Same
      expect(table.addImport('io'), 2);
      expect(table.addImport('math'), 1);  // Still 1
    });

    test('getIndex returns null for unknown modules', () {
      final table = ImportTable();
      table.addImport('math');

      expect(table.getIndex('math'), 1);
      expect(table.getIndex('unknown'), isNull);
    });

    test('size returns number of unique imports', () {
      final table = ImportTable();

      expect(table.size, 0);
      table.addImport('math');
      expect(table.size, 1);
      table.addImport('math');  // Duplicate
      expect(table.size, 1);
      table.addImport('io');
      expect(table.size, 2);
    });

    test('orderedImports returns imports in index order', () {
      final table = ImportTable();
      table.addImport('io');
      table.addImport('math');
      table.addImport('utils');

      expect(table.orderedImports, ['io', 'math', 'utils']);
    });

    test('contains checks for module presence', () {
      final table = ImportTable();
      table.addImport('math');

      expect(table.contains('math'), true);
      expect(table.contains('io'), false);
    });
  });

  group('RPC Transformation - Static Module', () {
    test('compiles static RPC to Distribute opcode', () {
      // Valid SRSW: R is bound in RPC, R? is used in print
      final ops = compile('''
boot :- otherwise |
    math # factorial(5, R),
    io # print(R?).
''');

      // Find Distribute instruction
      final distributeOps = ops.whereType<Distribute>().toList();

      expect(distributeOps.length, 2);  // factorial + print
      expect(distributeOps[0].importIndex, 1);  // math = index 1
      expect(distributeOps[0].functor, 'factorial');
      expect(distributeOps[0].arity, 2);
      expect(distributeOps[1].importIndex, 2);  // io = index 2
    });

    test('assigns correct indices to multiple static RPCs', () {
      final ops = compile('''
boot :- otherwise |
    math # factorial(5, F),
    io # print(F?),
    math # gcd(48, 18, G),
    io # print(G?).
''');

      final distributeOps = ops.whereType<Distribute>().toList();

      expect(distributeOps.length, 4);
      // math gets index 1, io gets index 2
      expect(distributeOps[0].importIndex, 1);  // math # factorial
      expect(distributeOps[0].functor, 'factorial');
      expect(distributeOps[1].importIndex, 2);  // io # print
      expect(distributeOps[1].functor, 'print');
      expect(distributeOps[2].importIndex, 1);  // math # gcd (same index as first math call)
      expect(distributeOps[2].functor, 'gcd');
      expect(distributeOps[3].importIndex, 2);  // io # print (same index)
    });
  });

  group('RPC Transformation - Dynamic Module', () {
    test('compiles dynamic RPC to Transmit opcode', () {
      // Simplest valid SRSW pattern: M writer in head, M? reader in body
      // Args X passed as reader from head to body
      final ops = compile('''
call_remote(M, X) :- otherwise |
    M? # foo(X?).
''');

      final transmitOps = ops.whereType<Transmit>().toList();

      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'foo');
      expect(transmitOps[0].arity, 1);
    });

    test('compiles dynamic RPC with multiple args', () {
      // M is module, X and Y are args passed through
      final ops = compile('''
call_remote(M, X, Y) :- otherwise |
    M? # bar(X?, Y?).
''');

      final transmitOps = ops.whereType<Transmit>().toList();

      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'bar');
      expect(transmitOps[0].arity, 2);
    });
  });

  group('RPC Transformation - Mixed', () {
    test('handles mix of static and dynamic RPCs', () {
      // M is writer (head), M? reader (body)
      // X is writer (head), X? reader (body)
      // F is writer (factorial output), F? reader (process input)
      final ops = compile('''
mixed(M, X) :- otherwise |
    math # factorial(X?, F),
    M? # process(F?).
''');

      final distributeOps = ops.whereType<Distribute>().toList();
      final transmitOps = ops.whereType<Transmit>().toList();

      expect(distributeOps.length, 1);
      expect(distributeOps[0].functor, 'factorial');

      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'process');
    });
  });

  group('Distribute and Transmit opcodes', () {
    test('Distribute toString formats correctly', () {
      final op = Distribute(1, 'factorial', 2);
      expect(op.toString(), 'Distribute([1] factorial/2)');
    });

    test('Transmit toString formats correctly', () {
      final op = Transmit(5, 'foo', 3);
      expect(op.toString(), 'Transmit(X5, foo/3)');
    });
  });
}
