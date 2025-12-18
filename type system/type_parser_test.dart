// test/type_parser_test.dart
//
// Tests for the GLP type parser

import '../lib/analysis/type_checker/type_ast.dart';
import '../lib/analysis/type_checker/type_parser.dart';

void main() {
  testBasicTypeParsing();
  testListTypes();
  testProcedureDeclarations();
  testMixedContent();
  
  print('\nAll tests passed!');
}

void testBasicTypeParsing() {
  print('Testing basic type parsing...');
  
  final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.
Constant ::= Number ; String.
''';
  
  final env = parseTypes(source);
  
  assert(env.types.length == 3, 'Expected 3 types');
  assert(env.types.containsKey('Nat'), 'Missing Nat type');
  assert(env.types.containsKey('Bool'), 'Missing Bool type');
  assert(env.types.containsKey('Constant'), 'Missing Constant type');
  
  final nat = env.types['Nat']!;
  assert(nat.alternatives.length == 2, 'Nat should have 2 alternatives');
  assert(nat.alternatives[0] is ConstantAlt, 'First alt should be constant 0');
  assert(nat.alternatives[1] is StructAlt, 'Second alt should be s(Nat)');
  
  final sAlt = nat.alternatives[1] as StructAlt;
  assert(sAlt.functor == 's', 'Functor should be s');
  assert(sAlt.arity == 1, 'Arity should be 1');
  
  print('  ✓ Basic types parsed correctly');
}

void testListTypes() {
  print('Testing list type parsing...');
  
  final source = '''
NatList ::= [] ; [Nat | NatList].
Tree ::= leaf ; node(Tree, Nat, Tree).
''';
  
  final env = parseTypes(source);
  
  assert(env.types.length == 2, 'Expected 2 types');
  
  final natList = env.types['NatList']!;
  assert(natList.alternatives.length == 2, 'NatList should have 2 alternatives');
  assert(natList.alternatives[0] is ListNilAlt, 'First alt should be []');
  assert(natList.alternatives[1] is ListConsAlt, 'Second alt should be [H|T]');
  
  final consAlt = natList.alternatives[1] as ListConsAlt;
  assert(consAlt.head is TypeRef, 'Head should be TypeRef');
  assert((consAlt.head as TypeRef).name == 'Nat', 'Head should be Nat');
  assert(consAlt.tail is TypeRef, 'Tail should be TypeRef');
  assert((consAlt.tail as TypeRef).name == 'NatList', 'Tail should be NatList');
  
  print('  ✓ List types parsed correctly');
}

void testProcedureDeclarations() {
  print('Testing procedure declarations...');
  
  final source = '''
procedure append(NatList, NatList, NatList).
procedure counter(MsgList, Number).
procedure merge(Stream, Stream, Stream).
''';
  
  final env = parseTypes(source);
  
  assert(env.procedures.length == 3, 'Expected 3 procedures');
  assert(env.procedures.containsKey('append/3'), 'Missing append/3');
  assert(env.procedures.containsKey('counter/2'), 'Missing counter/2');
  assert(env.procedures.containsKey('merge/3'), 'Missing merge/3');
  
  final append = env.procedures['append/3']!;
  assert(append.arity == 3, 'append should have arity 3');
  assert(append.argTypes.length == 3, 'append should have 3 arg types');
  assert(append.argTypes[0].name == 'NatList', 'First arg should be NatList');
  
  print('  ✓ Procedure declarations parsed correctly');
}

void testMixedContent() {
  print('Testing mixed content (types + clauses)...');
  
  // This simulates a real GLP file with types and clauses mixed
  final source = '''
%% Type declarations
Nat ::= 0 ; s(Nat).
NatList ::= [] ; [Nat | NatList].

procedure append(NatList, NatList, NatList).

%% This is a comment
append([], Ys, Ys?).
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).

procedure length(NatList, Nat).

length([], 0).
length([_|Xs], s(N?)) :- length(Xs?, N).
''';
  
  final env = parseTypes(source);
  
  // Should find types and procedures, ignoring clauses
  assert(env.types.length == 2, 'Expected 2 types, got ${env.types.length}');
  assert(env.procedures.length == 2, 'Expected 2 procedures, got ${env.procedures.length}');
  
  print('  ✓ Mixed content handled correctly');
}

// Helper to run test and report errors
void runTest(String name, void Function() test) {
  try {
    test();
    print('  ✓ $name');
  } catch (e) {
    print('  ✗ $name: $e');
    rethrow;
  }
}
