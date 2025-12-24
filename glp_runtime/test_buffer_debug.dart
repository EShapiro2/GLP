import 'test/analysis/type_checker/test_helpers.dart';

void main() {
  final bufferTypes = '''
    Buffer ::= [] ; [_? | Buffer].
    procedure producer(Number?, Buffer).
    procedure consumer(Buffer?).
  ''';

  print('=== Test 1: producer fills buffer slots ===');
  print('Code:');
  print('  producer(N, [N? | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).');
  print('Expected: Buffer at OUTPUT position');
  print('  _? × output = input → pattern has reader N?');
  print('  Buffer × output = output → pattern has writer Xs');
  print('');

  final result1 = checkTypes(bufferTypes + '''
    producer(N, [N? | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).
  ''');

  print('Errors: ${result1.errors.length}');
  for (final error in result1.errors) {
    print('  - $error');
  }
  print('Well-typed: ${result1.isWellTyped}');

  print('\n=== Test 2: consumer reads from buffer slots ===');
  print('Code:');
  print('  consumer([X1, X2, X3 | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).');
  print('Expected: Buffer at INPUT position');
  print('  _? × input = output → pattern has writer X1');
  print('  Buffer × input = input → pattern has reader Xs?');
  print('');

  final result2 = checkTypes(bufferTypes + '''
    consumer([X1, X2, X3 | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).
    consumer([]).
  ''');

  print('Errors: ${result2.errors.length}');
  for (final error in result2.errors) {
    print('  - $error');
  }
  print('Well-typed: ${result2.isWellTyped}');

  print('\n=== Test 3: Full bb program ===');
  final result3 = checkTypes(bufferTypes + '''
    bb :- consumer([X1?, X2? | Xs]), producer(1, [X1, X2 | Xs?]).
    consumer([X1, X2, X3 | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).
    consumer([]).
    producer(N, [N? | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).
  ''');

  print('Errors: ${result3.errors.length}');
  for (final error in result3.errors) {
    print('  - $error');
  }
  print('Well-typed: ${result3.isWellTyped}');
}
