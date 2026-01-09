import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  // Create environment with Stream type
  final env = TypeEnvironment.empty();

  // Stream ::= [] ; [_|Stream]
  env.addType(TypeDef(
    'Stream',
    [
      ListNilAlt(0, 0),
      ListConsAlt(
        PrimitiveModeAlt(false, 0, 0), // _ for head (produce)
        TypeRef('Stream', 0, 0),
        0,
        0,
      ),
    ],
    0,
    0,
  ));

  // procedure append(Stream?, Stream?, Stream).
  env.addProcedure(ProcDecl(
    'append',
    [
      TypeRef('Stream', 0, 0, isInput: true),  // input
      TypeRef('Stream', 0, 0, isInput: true),  // input
      TypeRef('Stream', 0, 0),                  // output
    ],
    0,
    0,
  ));

  final compiler = TypeCompiler(env);

  // Test clause 1: append([], Ys?, Ys).
  print('=== Testing clause 1: append([], Ys?, Ys) ===');
  final clause1 = TypedClause(
    head: ast.Goal('append', [
      ast.ListTerm.nil(0, 0),
      ast.VarTerm('Ys', isReader: true, line: 0, column: 0),
      ast.VarTerm('Ys', isReader: false, line: 0, column: 0),
    ], 0, 0),
  );

  final result1 = checkClause(clause1, env, compiler);
  print('isWellTyped: ${result1.isWellTyped}');
  if (!result1.isWellTyped) {
    print('Errors:');
    for (final error in result1.errors) {
      print('  - $error');
    }
  }

  // Test clause 2: append([X|Xs], Ys?, [X|Zs]) :- append(Xs?, Ys?, Zs).
  print('');
  print('=== Testing clause 2: append([X|Xs], Ys?, [X|Zs]) :- append(Xs?, Ys?, Zs) ===');

  final headArg1 = ast.ListTerm.cons(
    ast.VarTerm('X', isReader: false, line: 0, column: 0),
    ast.VarTerm('Xs', isReader: false, line: 0, column: 0),
    0, 0
  );
  final headArg2 = ast.VarTerm('Ys', isReader: true, line: 0, column: 0);
  final headArg3 = ast.ListTerm.cons(
    ast.VarTerm('X', isReader: true, line: 0, column: 0),
    ast.VarTerm('Zs', isReader: false, line: 0, column: 0),
    0, 0
  );

  final bodyAtom = ast.Goal('append', [
    ast.VarTerm('Xs', isReader: true, line: 0, column: 0),
    ast.VarTerm('Ys', isReader: true, line: 0, column: 0),
    ast.VarTerm('Zs', isReader: false, line: 0, column: 0),
  ], 0, 0);

  final clause2 = TypedClause(
    head: ast.Goal('append', [headArg1, headArg2, headArg3], 0, 0),
    bodyAtoms: [bodyAtom],
  );

  final result2 = checkClause(clause2, env, compiler);
  print('isWellTyped: ${result2.isWellTyped}');
  if (!result2.isWellTyped) {
    print('Errors:');
    for (final error in result2.errors) {
      print('  - $error');
    }
  }
}
