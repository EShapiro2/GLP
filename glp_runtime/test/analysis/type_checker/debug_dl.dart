import 'test_helpers.dart';

void main() {
  final result = checkTypes('''
    MyList ::= [_ | MyList] ; [].
    MyDiffList ::= MyList \\ MyList?.

    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A\\B?, B\\C?, A?\\C).

    procedure my_dl_to_list(MyDiffList?, MyList).
    my_dl_to_list(L\\[], L?).

    procedure use_append(MyList?, MyList?, MyList).
    use_append(L1, L2, Result) :-
        dl1(L1?, DL1),
        dl2(L2?, DL2),
        my_dl_append(DL1?, DL2?, DL3),
        my_dl_to_list(DL3?, Result).
  ''');

  print('isWellTyped: ${result.isWellTyped}');
  print('errors: ${result.errors.length}');
  for (final e in result.errors) {
    print('  - $e');
  }
}
