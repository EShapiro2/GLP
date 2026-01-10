import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  test('DEBUG: new_channel', () {
    final result = checkTypes('''
MyList ::= [_ | MyList] ; [].
MyChan ::= ch(MyList?, MyList).

procedure my_new_channel(MyChan, MyChan).
my_new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
''');
    
    print('isWellTyped: ${result.isWellTyped}');
    print('Errors (${result.errors.length}):');
    for (final e in result.errors) {
      print('  - ${e.message}');
    }
  });
}
