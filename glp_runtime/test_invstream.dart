import 'test/analysis/type_checker/test_helpers.dart';

void main() {
  print('=== InvStream Test ===');

  final result = checkTypes('''
    InvStream ::= [] ; [_? | InvStream].
    procedure fill_slots(InvStream?).
    fill_slots([]).
    fill_slots([Slot? | Rest]) :- Slot = value, fill_slots(Rest?).
  ''');

  print('Errors: ${result.errors.length}');
  for (final error in result.errors) {
    print('  - $error');
  }
  print('Warnings: ${result.warnings.length}');
  for (final warning in result.warnings) {
    print('  - $warning');
  }
  print('Well-typed: ${result.isWellTyped}');

  print('\n=== Stream Test (without [] case) ===');

  final streamResult = checkTypes('''
    MyStream ::< List.
    procedure process(MyStream?).
    process([X | Xs]) :- handle(X?), process(Xs?).
  ''');

  print('Errors: ${streamResult.errors.length}');
  for (final error in streamResult.errors) {
    print('  - $error');
  }
  print('Well-typed: ${streamResult.isWellTyped}');
}
