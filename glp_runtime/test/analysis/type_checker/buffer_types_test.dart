// test/analysis/type_checker/buffer_types_test.dart
//
// Tests for Buffer type with embedded input mode (_?)
// Validates mode combination: parent OUTPUT preserves mode, parent INPUT complements.

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Buffer Types', () {
    final bufferTypes = '''
      Buffer ::= [] ; [_? | Buffer].
      procedure producer(Number?, Buffer).
      procedure consumer(Buffer?).
    ''';

    group('POSITIVE', () {
      test('producer fills buffer slots', () {
        // producer(Buffer) - Buffer at output position
        // _? × output = input → pattern has reader N?
        // Buffer × output = output → pattern has writer Xs
        final result = checkTypes(bufferTypes + '''
          producer(N, [N? | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'producer fills buffer slots with readers at _? positions');
      });

      test('consumer reads from buffer slots', () {
        // consumer(Buffer?) - Buffer at input position
        // _? × input = output (complemented) → pattern has writers
        // Buffer × input = input (complemented) → pattern has reader Xs?
        final result = checkTypes(bufferTypes + '''
          consumer([X1, X2, X3 | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).
          consumer([]).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'consumer reads from buffer slots with writers at _? positions');
      });

      test('producer and consumer share buffer', () {
        // Full bb program
        final result = checkTypes(bufferTypes + '''
          bb :- consumer([X1?, X2? | Xs]), producer(1, [X1, X2 | Xs?]).
          consumer([X1, X2, X3 | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).
          consumer([]).
          producer(N, [N? | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'producer and consumer share buffer correctly');
      });
    });

    group('NEGATIVE', () {
      test('producer with wrong head mode fails', () {
        // Using writer N instead of reader N? at _? position in output context
        final result = checkTypes(bufferTypes + '''
          producer(N, [N | Xs]) :- number(N?) | N1 := N? + 1, producer(N1?, Xs).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'producer should use reader N? at _? position in output context');
      });

      test('consumer with wrong head mode fails', () {
        // Using reader X? instead of writer X at _? position in input context
        final result = checkTypes(bufferTypes + '''
          consumer([X1?, X2?, X3? | Xs?]) :- known(X1?) | consumer([X2?, X3? | Xs]).
          consumer([]).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'consumer should use writer X at _? position in input context');
      });

      test('consumer with wrong tail mode fails', () {
        // Using writer Xs instead of reader Xs? at Buffer position in input context
        final result = checkTypes(bufferTypes + '''
          consumer([X1, X2, X3 | Xs]) :- known(X1?) | consumer([X2?, X3? | Xs]).
          consumer([]).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'consumer should use reader Xs? at Buffer position in input context');
      });
    });
  });
}
