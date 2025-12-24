import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

TypeCheckResult checkTypes(String source) {
  final lines = source.split('\n');
  final clauseLines = <String>[];
  for (final line in lines) {
    final trimmed = line.trim();
    if (trimmed.contains('::=') || trimmed.contains('::<') || trimmed.startsWith('procedure ')) {
      continue;
    }
    if (trimmed.isNotEmpty && !trimmed.startsWith('%')) {
      clauseLines.add(line);
    }
  }
  final clauseSource = clauseLines.join('\n');
  final lexer = Lexer(clauseSource);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final clauses = program.procedures.expand((p) => p.clauses).toList();
  final typeEnv = parseTypes(source);
  final checker = TypeChecker(typeEnv);
  return checker.check(clauses);
}

void main() {
  print('=== Test 1: Self-Duality (Any and Any?) ===');
  var result = checkTypes('''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    procedure foo(MyAny, MyAny?).
    foo(X, X?).
  ''');
  print('Errors: ${result.errors.length}');
  for (final e in result.errors) print('  - $e');
  print('Warnings: ${result.warnings.length}');
  for (final w in result.warnings) print('  - $w');
  print('Well-typed: ${result.isWellTyped}\n');

  print('=== Test 2: DiffList (dl_append) ===');
  result = checkTypes(r'''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    MyList ::= [MyAny | MyList] ; [].
    MyDiffList ::= MyList \ MyList?.
    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A\B?, B\C?, A?\C).
  ''');
  print('Errors: ${result.errors.length}');
  for (final e in result.errors) print('  - $e');
  print('Warnings: ${result.warnings.length}');
  for (final w in result.warnings) print('  - $w');
  print('Well-typed: ${result.isWellTyped}\n');

  print('=== Test 3: Channel (send) ===');
  result = checkTypes('''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    MyList ::= [MyAny | MyList] ; [].
    MyStream ::< MyList.
    MyChannel ::= ch(MyStream?, MyStream).
    procedure my_send(MyAny, MyChannel?, MyChannel).
    my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).
  ''');
  print('Errors: ${result.errors.length}');
  for (final e in result.errors) print('  - $e');
  print('Warnings: ${result.warnings.length}');
  for (final w in result.warnings) print('  - $w');
  print('Well-typed: ${result.isWellTyped}\n');
}
