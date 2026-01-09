import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

TypeCheckResult checkTypes(String source) {
  final lines = source.split('\n');
  final clauseLines = <String>[];
  for (final line in lines) {
    final trimmed = line.trim();
    if (trimmed.contains('::=') || trimmed.startsWith('procedure ')) continue;
    if (trimmed.isNotEmpty && !trimmed.startsWith('%')) clauseLines.add(line);
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
  print('=== NEGATIVE test: dl_append with WRONG modes ===');
  print('Clause: my_dl_append(A?\\\\B, B?\\\\C, A\\\\C?).');
  print('');
  
  var result = checkTypes('''
    MyList ::= [_ | MyList] ; [].
    MyDiffList ::= MyList \\ MyList?.
    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A?\\B, B?\\C, A\\C?).
  ''');
  
  print('isWellTyped: ${result.isWellTyped}');
  print('Expected: false (should FAIL because modes are wrong)');
  print('');
  if (result.isWellTyped) {
    print('BUG: Implementation accepted ill-typed program!');
  } else {
    print('CORRECT: Implementation rejected ill-typed program!');
  }
  if (result.errors.isNotEmpty) {
    print('Errors:');
    for (final err in result.errors) {
      print('  - $err');
    }
  }
  
  print('');
  print('=== POSITIVE test: dl_append with CORRECT modes ===');
  print('Clause: my_dl_append(A\\\\B?, B\\\\C?, A?\\\\C).');
  print('');
  
  result = checkTypes('''
    MyList ::= [_ | MyList] ; [].
    MyDiffList ::= MyList \\ MyList?.
    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A\\B?, B\\C?, A?\\C).
  ''');
  
  print('isWellTyped: ${result.isWellTyped}');
  print('Expected: true (should PASS)');
  if (!result.isWellTyped) {
    print('Errors:');
    for (final err in result.errors) {
      print('  - $err');
    }
  }
}
